;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'dap-mode)
(require 'cl-lib)
(require 'projectile)

;;; Code:
(defun dap--projectile-find-launch-json ()
  "Return the location of the launch.json file in the current project."
  (when-let ((project (projectile-ensure-project (projectile-project-root))))
    (concat project "launch.json")))

(defun dap--projectile-get-launch-json ()
  "Parse the project's launch.json as json data and return the result."
  (when-let ((launch-json (dap--projectile-find-launch-json))
             (json-object-type 'plist))
    (json-read-file launch-json)))

(defun dap--parse-launch-json (json)
  "Return a list of all launch configurations in JSON.
JSON must have been acquired with `dap--projectile-get-launch-json'."
  (or (plist-get json :configurations) (list json)))

(defun dap--projectile-parse-launch-json ()
  "Return a list of all launch configurations for the current project."
  (dap--parse-launch-json (dap--projectile-get-launch-json)))

(defun dap--configuration-get-name (conf)
"Return the name of launch configuration CONF."
  (plist-get conf :name))

(defun projectile-project-basename (&optional dir)
  "Return the name of the project root directory.
Starts the project-root search at DIR."
  (let ((project (projectile-ensure-project (projectile-project-root dir))))
    (file-name-nondirectory (directory-file-name project))))

(defun projectile-relative-file (&optional file dir)
  "Return the path to FILE relative to the project root.
The search for the project root starts at DIR. FILE defaults to
variable `buffer-file-name'."
  (let ((project (projectile-ensure-project (projectile-project-root dir))))
    (file-relative-name (or file buffer-file-name) project)))

(defun projectile-relative-dirname (&optional file dir)
  "Return the path to the directory of file relative to the project root.
The search for the project root starts at DIR. FILE defaults to
variable `buffer-file-name'"
  (projectile-relative-file (file-name-directory (or file buffer-file-name))
                            dir))

(defun buffer-basename ()
  "Return the name of the current buffer's file without its directory."
  (file-name-nondirectory buffer-file-name))

(defun buffer-basename-sans-extension ()
  "Same as `buffer-basename', but without the extension."
  (file-name-sans-extension (buffer-basename)))

(defun buffer-extension ()
  "Return the extension of the buffer's file with a leading dot.
If there is either no file associated with the current buffer or
if that file has no extension, return the empty string."
  (if-let ((buffer-name buffer-file-name)
           (ext (file-name-extension buffer-name)))
      (concat "." ext)
    ""))

(defun buffer-dirname ()
  "Return the directory the buffer's file is in."
  (file-name-directory buffer-file-name))

(defun buffer-current-line ()
  "Return the line the cursor is on in the current buffer."
  (number-to-string (line-number-at-pos)))

(defun buffer-selected-text ()
  "Return the text selected in the current buffer.
If no text is selected, return the empty string."
  ;; Cannot fail, as if there is no mark, (mark) and (point) will be equal, and
  ;; (`buffer-substring-no-properties') will yield "", as it should.
  (buffer-substring-no-properties (mark) (point)))

(defun dap--warn-nil (text)
  (message (concat "warning: launch.json: " text))
  nil)

(defun dap--warn-unknown-envvar (var)
  (dap--warn-nil (format "no such environment variable '%s'" var)))

(defun dap--launch-json-getenv ()
  (let ((var (match-string 1)))
   (or (getenv var) (dap--warn-unknown-envvar var) "")))

(defvar dap--launch-json-variables
  ;; list taken from https://code.visualstudio.com/docs/editor/variables-reference
  '(("workspaceFolderBasename" . projectile-project-basename)
    ("workspaceFolder" . projectile-project-root)
    ("relativeFileDirname" . projectile-relative-dirame)
    ("relativeFile" . projectile-relative-file)
    ("fileBasenameNoExtension" . buffer-basename-sans-extension)
    ("fileBasename" . buffer-basename)
    ("fileDirname" . buffer-dirname)
    ("fileExtname" . buffer-extension)
    ("lineNumber" . buffer-current-line)
    ("selectedText" . buffer-selected-text)
    ("file" . buffer-file-name)
    ("env:\\(.*\\)" . dap--launch-json-getenv)
    ;; technically not in VSCode, but I still wanted to add a way to escape $
    ("$" . "$")
    ;; the following variables are valid in VSCode, but have no meaning in
    ;; Emacs, and are as such unsupported.
    ;; ("cwd") ;; the task runner's current working directory,
    ;;         ;; not `default-directory'
    ;; ("execPath")
    ;; ("defaultBuildTask")
    )
  "This variable is a list of dotted pairs (regex . value) that
is iterated from the top to the bottom when expanding variables
in the strings of the selected launch configuration from
launch.json or in `dap-expand-variable'.

When a regex matches (`string-match'), value is evaluated as
follows: if it is a function (or a quoted lambda), that function
is called with no arguments using `funcall', and its result,
which must be a string, is used in place of the variable.
Otherwise, if it is a symbol, the symbol's value is used the same
way. Lastly, if it is a string, the string is used as a
replacement. If no regex matches, the empty string is used as a
replacement and a warning is issued.

Note that the regex can contain capture groups, because, after
matching, the regex is searched in a temporary buffer, so that
you can use `match-string' as if it were accessing match-data in
a buffer (i.e. without a string argument). See
`dap--launch-json-getenv' for an example.")

(defun dap--launch-json-eval-poly-type (value)
  (cond ((functionp value) (funcall value))
        ((symbolp value) (symbol-value value))
        (t value)))

(defun dap--warn-var-nil (var)
  (dap--warn-nil (format "variable '%s' is unknown and was ignored" var)))

(defun dap-expand-variable (var)
  (catch 'ret
    (save-match-data
      (dolist (var-pair dap--launch-json-variables)
        (when (string-match (car var-pair) var)
          ;; since there is no way to communicate var to the match function, the
          ;; user cannot pass var to string-match to decompose var by her regex.
          ;; As such, we must create a temporary buffer, insert the string and
          ;; to a re-search so that (string-match) returns the correct results.
          (with-temp-buffer
            (insert var)
            (goto-char (point-min))

            (re-search-forward (car var-pair))
            (throw 'ret (or (dap--launch-json-eval-poly-type (cdr var-pair))
                            (dap--warn-var-nil var) ""))))))
    nil))

(defun dap-expand-variables-in-string (s)
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))

    (save-match-data
      (while (re-search-forward "${\\([^}]*\\)}" nil t)
        (replace-match (dap-expand-variable (match-string 1)))))

    (buffer-string)))

(defun dap--launch-json-expand-vars (conf)
  (cond ((listp conf)
         (apply #'nconc (cl-loop for (k v) on conf by #'cddr collect
                                 (list k (dap--launch-json-expand-vars v)))))
        ((stringp conf) (dap-expand-variables-in-string conf))
        (t conf)))

(defun dap--launch-json-prompt-configuration ()
  (dap--completing-read "Select configuration: "
                        (dap--projectile-parse-launch-json)
                        #'dap--configuration-get-name))

(defun dap-debug-launch-json ()
  "Read the project's launch.json and ask the user for a launch configuration."
  (interactive)
  (dap-debug (dap--launch-json-expand-vars
              (dap--launch-json-prompt-configuration))))

(provide 'dap-mode-launch-json)
;;; dap-mode-launch-json.el ends here
