;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Copyright (C) 2020 Nikita Bloshchanevich

;; Author: Nikita Bloshchanevich <nikblos@outlook.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'dap-mode)
(require 'lsp)
(require 'cl-lib)

;;; Code:

(defun dap--project-find-launch-json ()
  "Return the location of the launch.json file in the current project."
  (when-let ((project (lsp-workspace-root)))
    (concat project "/launch.json")))

(defun dap--project-get-launch-json ()
  "Parse the project's launch.json as json data and return the result."
  (when-let ((launch-json (dap--project-find-launch-json))
             (json-object-type 'plist))
    (json-read-file launch-json)))

(defun dap--parse-launch-json (json)
  "Return a list of all launch configurations in JSON.
JSON must have been acquired with `dap--project-get-launch-json'."
  (or (plist-get json :configurations) (list json)))

(defun dap--project-parse-launch-json ()
  "Return a list of all launch configurations for the current project."
  (dap--parse-launch-json (dap--project-get-launch-json)))

(defun dap--configuration-get-name (conf)
"Return the name of launch configuration CONF."
  (plist-get conf :name))

(defun dap--project-project-basename (&optional dir)
  "Return the name of the project root directory.
Starts the project-root search at DIR."
    (file-name-nondirectory (directory-file-name (lsp-workspace-root))))

(defun dap--project-relative-file (&optional file dir)
  "Return the path to FILE relative to the project root.
The search for the project root starts at DIR. FILE defaults to
variable `buffer-file-name'."
    (file-relative-name (or file buffer-file-name) (lsp-workspace-root)))

(defun dap--project-relative-dirname (&optional file dir)
  "Return the path to the directory of file relative to the project root.
The search for the project root starts at DIR. FILE defaults to
variable `buffer-file-name'"
  (project-relative-file (file-name-directory (or file buffer-file-name))
                            dir))

(defun dap--buffer-basename ()
  "Return the name of the current buffer's file without its directory."
  (file-name-nondirectory buffer-file-name))

(defun dap--buffer-basename-sans-extension ()
  "Same as `dap--buffer-basename', but without the extension."
  (file-name-sans-extension (buffer-basename)))

(defun dap--buffer-extension ()
  "Return the extension of the buffer's file with a leading dot.
If there is either no file associated with the current buffer or
if that file has no extension, return the empty string."
  (if-let ((buffer-name buffer-file-name)
           (ext (file-name-extension buffer-name)))
      (concat "." ext)
    ""))

(defun dap--buffer-dirname ()
  "Return the directory the buffer's file is in."
  (file-name-directory buffer-file-name))

(defun dap--buffer-current-line ()
  "Return the line the cursor is on in the current buffer."
  (number-to-string (line-number-at-pos)))

(defun dap--buffer-selected-text ()
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

(defun dap--launch-json-getenv (var)
  (let ((envvar (match-string 1 var)))
    (or (getenv envvar) (dap--warn-unknown-envvar envvar) "")))

(defvar dap-launch-json-variables
  ;; list taken from https://code.visualstudio.com/docs/editor/variables-reference
  '(("workspaceFolderBasename" . dap--project-project-basename)
    ("workspaceFolder" . lsp-workspace-root)
    ("relativeFileDirname" . dap--project-relative-dirame)
    ("relativeFile" . dap--project-relative-file)
    ("fileBasenameNoExtension" . dap--buffer-basename-sans-extension)
    ("fileBasename" . dap--buffer-basename)
    ("fileDirname" . dap--buffer-dirname)
    ("fileExtname" . dap--buffer-extension)
    ("lineNumber" . dap--buffer-current-line)
    ("selectedText" . dap--buffer-selected-text)
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

When a REGEX matches (`string-match'), its corresponding VALUE is
evaluated as follows: if it is a function (or a quoted lambda),
that function is called with `funcall', and its result, which
must be a string, is used in place of the variable. If you used
capture groups in REGEX, the function you specified in VALUE is
called with the variable as its only argument. This way, you can
use string-match to get the capture groups. If, however, REGEX
does not contain capture groups, your function is called without
any arguments. Otherwise, if it is a symbol, the symbol's value
is used the same way. Lastly, if it is a string, the string is
used as a replacement. If no regex matches, the empty string is
used as a replacement and a warning is issued.

See `dap--launch-json-getenv' for an example on how to use
capture groups in REGEX.")

(defun dap--launch-json-eval-poly-type (value var)
  (cond ((and var (functionp value)) (funcall value var))
        ((functionp value) (funcall value))
        ((symbolp value) (symbol-value value))
        (t value)))

(defun dap--warn-var-nil (var)
  (dap--warn-nil (format "variable '%s' is unknown and was ignored" var)))

(defun dap-expand-variable (var)
  (catch 'ret
    (save-match-data
      (dolist (var-pair dap-launch-json-variables)
        (when (string-match (car var-pair) var)
          (throw 'ret
                 (or
                  (dap--launch-json-eval-poly-type
                   (cdr var-pair)
                   (if (= (length (match-data)) 2) ;; no capture groups
                       nil
                     var
                     (message "wow capture groups")))
                  (dap--warn-var-nil var)
                  "")))))
    nil))

(defun dap-expand-variables-in-string (s)
  (let ((old-buffer (current-buffer)))
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))

      (save-match-data
        (while (re-search-forward "${\\([^}]*\\)}" nil t)
          (let ((var (match-string 1)))
            (replace-match
             (with-current-buffer old-buffer
               (dap-expand-variable var))))))

      (buffer-string))))

(defun dap--launch-json-expand-vars (conf)
  (cond ((listp conf)
         (apply #'nconc (cl-loop for (k v) on conf by #'cddr collect
                                 (list k (dap--launch-json-expand-vars v)))))
        ((stringp conf) (dap-expand-variables-in-string conf))
        (t conf)))

(defun dap--launch-json-prompt-configuration ()
  (dap--completing-read "Select configuration: "
                        (dap--project-parse-launch-json)
                        #'dap--configuration-get-name))

(defun dap-debug-launch-json ()
  "Read the project's launch.json and ask the user for a launch configuration."
  (interactive)
  (dap-debug (dap--launch-json-expand-vars
              (dap--launch-json-prompt-configuration))))

(provide 'dap-mode-launch-json)
;;; dap-mode-launch-json.el ends here
