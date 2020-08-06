# About
A plugin that extends Emacs' dap-mode with support for launch.json files like
they are common in VSCode.

Hopefully this plugin will get integrated into mainline dap-mode soon.

# Features
- Support for multiple launch configurations
- Supports most of [VSCode's
  variables](https://code.visualstudio.com/docs/editor/variables-reference) in
  the launch.json file

# Usage
The main entry point is `dap-debug-launch-json`. It will parse your project's
launch.json file (the project is detected with projectile) and prompt you to
select one launch configuration from a list. Afterwards, the usual dap-mode
debugger will start.

# See also
- [launch.json variable reference](https://code.visualstudio.com/docs/editor/variables-reference)
- [Debugging in VSCode (with launch.json)](https://code.visualstudio.com/docs/editor/debugging)
