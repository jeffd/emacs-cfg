;;; Jeff Dlouhy's Emacs Settings
(message "started loading settings ...")

(setq custom-basedir (expand-file-name "~/.emacs-cfg/.emacs.d/"))
(add-to-list 'load-path custom-basedir)
(add-to-list 'load-path "/usr/local/bin")

(defun add-path (p)
  (add-to-list 'load-path (concat custom-basedir p)))

;;; Font Settings
(set-default-font "Consolas-7")
    (set-fontset-font (frame-parameter nil 'font)
      'han '("cwTeXHei" . "unicode-bmp"))

;;; Theme Settings
(message "applying theme settings ...")
(require 'color-theme)
(setq color-theme-is-global t)
(cond ((not (eq system-type 'gnu-linux))
       (color-theme-initialize)))
(color-theme-comidia)

;;; Hide the toolbar
(tool-bar-mode -1)

;;; Fullscreen
(cond ((eq system-type 'gnu/linux)
   (defun fullscreen ()
      (interactive)
      (set-frame-parameter nil 'fullscreen
                           (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

    (global-set-key [f11] 'fullscreen)

    (defun switch-full-screen ()
      (interactive)
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

    (global-set-key [f11] 'switch-full-screen)))

(message "loading yasnippet customizations ...")
(add-path "yasnippet")
(require 'yasnippet)

;;; Git
(message "applying git settings ...")
;(require 'git)
;(require 'gitsum)

;;; Cursor and Line
(message "applying cursor settings ...")
(setq-default cursor-type 'box)
(setq-default show-trailing-whitespace t)

;;; Scrolling
(global-set-key [C-next] 'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)

;;; C Style Settings
(message "applying c style settings ...")
(setq-default indent-tabs-mode nil)
;(setq c-basic-offset 2)
(setq tab-width 2)
;;;(add-hook 'c-mode-hook '(lambda ()(c-set-style "linux")))

;; Create my personal style.
(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 2)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . -2)
                                   (case-label        . 2)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "Jeff's C Programming Style")

(c-add-style "jeff-style" my-c-style)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "jeff-style")
  ;; other customizations
  (setq tab-width 2
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline, but not hungry-delete
  (c-toggle-auto-newline 1)
  (setq c-basic-offset 2))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(message "applying Xcode settings ...")
;;; Objective-C Settings
(setq auto-mode-alist (cons '("\\.mm\\'" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.m\\'" . objc-mode) auto-mode-alist))

;;; Header File Support
;;; http://hutley.net/brett/emacs/integrating-emacs-and-xcode/
(defun xcode-choose-header-mode ()
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (progn
        ;; OK, we got a .h file, if a .m file exists we'll assume it's
        ; an objective c file. Otherwise, we'll look for a .cpp file.
        (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
              (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
          (if (file-exists-p dot-m-file)
              (progn
                (objc-mode))
            (if (file-exists-p dot-cpp-file)
                (c++-mode)))))))

(add-hook 'find-file-hook 'xcode-choose-header-mode)

;;; Xcode Build Settings
(cond ((not (eq system-type 'gnu/linux))
 (defun xcode-compile ()
  (interactive)
  (let ((df (directory-files "."))
        (has-proj-file nil))
    (while (and df (not has-proj-file))
      (let ((fn (car df)))
        (if (> (length fn) 10)
            (if (string-equal (substring fn -10) ".xcodeproj")
                (setq has-proj-file t))))
      (setq df (cdr df)))
    (if has-proj-file
        (compile "xcodebuild -configuration Debug")
      (compile "make"))))

(defun build-with-xcode ()
  (interactive)
  (defun dir () (shell-command "osascript -e 'tell application \"Xcode\" to get the project directory of project 1'"))
  (shell-command (format "cd %s" (dir))))

(define-key osx-key-mode-map (kbd "A-r") 'build-and-go-in-xcode)

(defun build-and-go-in-xcode ()

(interactive)
  (shell-command "osascript -e 'tell application \"Xcode\" to build project 1'")
  (shell-command "osascript -e 'tell application \"Xcode\" to launch project 1'"))))

;;; Scheme Settings
(message "applying scheme settings ...")
(autoload 'paredit-mode "paredit.el"
    "Minor mode for pseudo-structurally editing Lisp code." t)

(autoload 'scheme48-mode "scheme48.el" "Major mode for Scheme48 interaction." t)
(autoload 'aling-let-keybinding "align-let" "Align let mode" t)

;(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme48-mode))
(setq auto-mode-alist (cons '("\\.scm\\'" . scheme48-mode) auto-mode-alist))
(require 'scheme48)

(add-to-list 'interpreter-mode-alist '("scsh" . scheme48-mode))
(setq scheme-program-name "nuscsh")

(add-hook 'scheme-mode-hook
          (lambda ()
            (paredit-mode +1)
            (align-let-keybinding)
            (define-key paredit-mode-map (kbd "[") 'paredit-open-round)
            (define-key paredit-mode-map (kbd "]") 'paredit-close-round)
            (define-key paredit-mode-map (kbd "(") 'paredit-open-square)
            (define-key paredit-mode-map (kbd ")") 'paredit-close-square)))




;;; OpenGL Mode
(message "applying OpenGL settings ...")
(autoload 'OpenGL-minor-mode "OpenGL.el" "OpenGL editing utilities." t)
(add-hook 'OpenGL-minor-mode-hook 'OpenGL-setup-keys)

(add-hook 'c-mode-hook
 	  '(lambda ()
 	     (cond ((string-match "/\\([Oo]pen\\)?[Gg][Ll]/"
 				  (buffer-file-name))
 		    (require 'OpenGL)
		    (OpenGL-minor-mode 1)
		    (OpenGL-setup-keys)))))