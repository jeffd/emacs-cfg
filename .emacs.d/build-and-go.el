(defun build-and-go-in-xcode ()
  (shell-command "osascript -e 'tell application \"Xcode\" to build project 1'")
  (shell-command "osascript -e 'tell application \"Xcode\" to launch project 1'"))