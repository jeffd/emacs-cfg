;;; Jeff Dlouhy's Emacs Settings
(message "started loading settings ...")

(setq custom-basedir (expand-file-name "~/.emacs-cfg/emacs.d/"))
(add-to-list 'load-path custom-basedir)
(add-to-list 'load-path "/usr/local/plt/bin")
(add-to-list 'load-path "/usr/local/bin")
(add-to-list 'load-path "/usr/local/git/bin/")

(defun add-path (p)
  (add-to-list 'load-path (concat custom-basedir p)))

;;; Emacs Lisp Package Archive
(message "applying ELPA settings ...")
(setq package-user-dir "~/.emacs-cfg/emacs.d/elpa")
(add-path "elpa")
(load "package")
(package-initialize)

;;; Will remove when there is a true GNU Operating System
(setq inhibit-start-screen 1)
(setq inhibit-splash-screen 1)

;;; I condem thee to Hell!
(global-set-key (kbd "C-x C-c") nil)

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;;; Font Settings
;;;(set-fontset-font (frame-parameter nil 'font)
;;;                  'han '("cwTeXHei" . "unicode-bmp"))

(message "applying font settings ...")
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil
			:family "consolas" :height 130)
  (set-frame-font "Consolas-13"))

;;; Settings Theme
(message "applying theme settings ...")
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-comidia)

;;; Shell Settings
(message "applying shell settings ...")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;;; Smooth Scrolling
(message "applying scrolling settings ...")
(setq scroll-step 1
      scroll-conservatively 10000)

;;; Compile Settings
(require 'smart-compile)

;;; C-x b
(require 'iswitchb)
(iswitchb-mode 1)

;;; Narrowing
(put 'narrow-to-region 'disabled nil)

;;; Hide the toolbar and friends
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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

(cond ((eq system-type 'darwin)
       (defun toggle-fullscreen ()
         (interactive)
         (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                                  nil
                                                'fullboth)))

       (global-set-key [(meta return)] 'toggle-fullscreen)))

;;; Snippet settings
(message "loading yasnippet customizations ...")
(add-path "yasnippet")
(require 'yasnippet)

(yas/initialize)
(yas/load-directory "~/.emacs-cfg/emacs.d/yasnippet/")

(setq yas/extra-mode-hooks
      '(ruby-mode-hook actionscript-mode-hook ox-mode-hook objc-mode-hook cc-mode-hook python-mode-hook))

;;; Git
(message "applying git settings ...")
(require 'git)
(require 'gitsum)

;;; Cursor and Line
(message "applying cursor settings ...")
(setq-default cursor-type 'box)
(setq-default show-trailing-whitespace t)
(setq-default transient-mark-mode t)
(setq default-truncate-lines t)
;;;(cua-mode t)

;;; Scrolling
(global-set-key [C-next] 'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)

;;; C Style Settings
(message "applying c style settings ...")
(require 'objc-c-mode)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;;; Create my personal style.
(defconst my-c-style
  '((indent-tabs-mode . nil)
    (c-basic-offset . 2)
    (c-subword-mode 1)
    (tab-width . 2)
    (c-indent-comments-syntactically-p       . t)
    (c-comment-only-line-offset              . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi
                                   brace-else-brace
                                   brace-elseif-brace
                                   compact-empty-funcall))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . -2)
                                   (case-label        . 2)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)
                                   (objc-method-call-cont .
                                                          (c-lineup-ObjC-method-call-colons
                                                           c-lineup-ObjC-method-call
                                                           +))))
    (c-echo-syntactic-information-p . t))
  "Jeff's C Programming Style")


(c-add-style "jeff-style" my-c-style)

;;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "jeff-style")
  ;; other customizations
  (setq tab-width 2
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline, but not hungry-delete
  ;;  (c-toggle-auto-newline 1)
  (setq c-basic-offset tab-width))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq next-line-add-newlines nil)

;;; C Refactoring Functions
;;; move current function up
(defun move-function-up ()
  (interactive)
  (save-excursion
    (c-mark-function)
    (kill-region (region-beginning) (region-end))
    (c-beginning-of-defun 1)
    (yank)))

;;; move current function down
(defun move-function-down ()
  (interactive)
  (save-excursion
    (c-mark-function)
    (kill-region (region-beginning) (region-end))
    (c-beginning-of-defun -1)
    (yank)))

;;; AucTeX settings
;;; (load "auctex.el" nil t t)
;;; (load "preview-latex.el" nil t t)
;;; (setq TeX-auto-save t)
;;; (setq TeX-parse-self t)
;;; (setq-default TeX-master nil)

;;; Ejacs
;;;(add-path "js")
;;;(autoload 'js-console "js-console" nil t)
;;;(require 'json)

;;; JS Interpreter
;(require 'js-comint)
;(setq inferior-js-program-command "/usr/local/bin/objj")
;; (add-hook 'js2-mode-hook '(lambda ()
;; 			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;; 			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;; 			    (local-set-key "\C-cb" 'js-send-buffer)
;; 			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;; 			    (local-set-key "\C-cl" 'js-load-file-and-go)))


(add-path "muse")
(require 'muse-mode)
(require 'muse-publish)
(require 'muse-html)
(require 'muse-latex)
(require 'muse-xml)

;;; Objective-C Settings
(message "applying Xcode settings ...")
(setq auto-mode-alist
      (append '(("\\.mm\\'" . objc-mode)
                ("\\.m\\'" . objc-mode)
                ("\\.j\\'" . objj-mode))
              auto-mode-alist))

(require 'objc-c-mode)
(require 'objj-mode)

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
(cond ((eq system-type 'darwin)
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
             (cd "..")
             (xcode-compile))))

       (defun xcode-clean ()
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
               (compile "xcodebuild -configuration Debug clean"))))

       (defun build-with-xcode ()
         (interactive)
         (defun dir () (shell-command "osascript -e 'tell application \"Xcode\" to get the project directory of project 1'"))
         (shell-command (format "cd %s" (dir))))

                                        ;       (define-key osx-key-mode-map (kbd "A-r") 'build-and-go-in-xcode)

       (defun build-and-go-in-xcode ()

         (interactive)
         (shell-command "osascript -e 'tell application \"Xcode\" to build project 1'")
         (shell-command "osascript -e 'tell application \"Xcode\" to launch project 1'"))))

;;; Scheme Settings
(message "applying scheme settings ...")
(autoload 'scheme48-mode "scheme48.el" "Major mode for Scheme48 interaction." t)

(setq auto-mode-alist
      (append '(("\\.scm$" . scheme48-mode)
                ("\\.ss$"  . scheme-mode) ; PLT & Chez
                ("\\.sch$" . scheme-mode) ; Bigloo & Larceny
                ("\\.sc$"  . scheme-mode) ; JMS
                ("\\.asd$" . lisp-mode))  ; ASDF system files
              auto-mode-alist))

(add-hook 'scheme-mode-hook
          (lambda ()
            ;; Prevent font-lock from being confused about #| ... |#
            ;; comments.
            (set (make-local-variable 'comment-start-skip)
                 (rx (and
                      ;; If any backslashes are prefixed, ensure that
                      ;; there be an even number of them.
                      (submatch (or line-start (not (any ?\\)))
                                (* "\\\\"))
                      (or (+ ";") "#|")
                      (* (any " \t")))))))

(require 'scheme48)
(add-to-list 'interpreter-mode-alist '("scsh" . scheme48-mode))
(setq scheme-program-name "nuscsh")
(if window-system (show-paren-mode +1))

;;; LET (it be)
(message "applying LET settings ...")
(autoload 'align-let "align-let" nil t)
(autoload 'align-let-keybinding "align-let" nil t)
(add-hook 'emacs-lisp-mode-hook 'align-let-keybinding)
(add-hook 'scheme-mode-hook     'align-let-keybinding)

;;; Paredit
(message "applying paredit settings ...")
(autoload 'paredit-mode "paredit.el"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(mapc (lambda (hook) (add-hook hook (lambda ()
                                      (paredit-mode +1))))
      '(lisp-mode-hook
        emacs-lisp-mode-hook
        scheme-mode-hook))

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "[") 'paredit-open-round)
     (define-key paredit-mode-map (kbd "]") 'paredit-close-round)
     (define-key paredit-mode-map (kbd "(") 'paredit-open-square)
     (define-key paredit-mode-map (kbd ")") 'paredit-close-square)))

;;; SRFI Browser
(require 'srfi)

;;; PLT Scheme
;;;(require 'quack)

;;; Slime
(message "applying SLIME settings ...")
(add-path "slime")
(require 'slime)
(slime-setup)

;;; Slime48
(add-path "slime48")
(eval-after-load "slime"
  '(progn
     (slime-setup)
     (setq slime-lisp-implementations
           `((s48 ("scheme48") :init slime48-init-command)
             ,@slime-lisp-implementations))))

(autoload 'slime48-init-command "slime48"
  "Return a string to initialize Scheme48 running under SLIME.")

;; This snippet lets you specify a scheme48-package local variable,
;; in a file's -*- line or local variables section, and have SLIME48
;; automatically evaluate code in the right package.  For instance,
;; all of my Scheme48 source files start with:
;;   ;;; -*- Mode: Scheme; scheme48-package: ... -*-
(eval-after-load "slime48"
  '(add-hook 'slime-mode-hook
             (lambda ()
               (if (and (boundp 'scheme48-package)
                        scheme48-package)
                   (setq slime-buffer-package
                         (with-output-to-string
                           (princ scheme48-package)))))))

;;; Clojure
(message "applying Clojure settings ...")
(eval-after-load "clojure-mode"
  '(progn
     (defun clojure-paredit-hook () (paredit-mode +1))
     (add-hook 'clojure-mode-hook 'clojure-paredit-hook)

     (define-key clojure-mode-map "{" 'paredit-open-brace)
     (define-key clojure-mode-map "}" 'paredit-close-brace)))

;;; Clojure swank
;;(setq swank-clojure-jar-path "~/Development/Clojure/clojure_1.0.0/clojure.jar")

;;; Clojure classpaths
;;;(setq swank-clojure-extra-classpaths (list "/path/to/extra/classpaths"))

(add-path "swank-clojure")
(require 'swank-clojure-autoload)

;;; The following function runs Slime with Clojure, even if Slime defaults to another Lisp.
;;; The above configuration alone, however, will make Clojure the default, so all that is necessary
;;; to run Slime with Clojure is M-x slime.
(defun run-clojure ()
  "Starts clojure in Slime"
  (interactive)
  (slime 'clojure))

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

;;; Python Settings
(message "applying python settings ...")
(cond ((eq system-type 'gnu/linux)
       (autoload 'python-mode "python.el"
         "Major mode for Python replacing old python-mode" t)
       (require 'pymacs)
       (pymacs-load "ropemacs" "rope-")))

(require 'django-html-mode)

;;; Artist Mode
(autoload 'artist-mode "artist" "Enter artist-mode" t)
(require 'artist)

;;; Java Settings
;; (message "applying java settings ...")
;; (add-path "java/jde/lisp")
;; (add-path "java/elib")
;; (load-file "emacs.d/java/cedet/common/cedet.el")
;; (require 'jde)


;;; w3m
;; (message "applying browser settings ...")
;; (if (and (= emacs-major-version 23)
;;          (eq system-type 'gnu/linux))
;;     (progn
;;       (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
;;       (require 'w3m-load))
;;   (require 'w3m))

;; (setq browse-url-browser-function 'w3m-browse-url)
;;  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;;  ;; optional keyboard short-cut
;;  (global-set-key "\C-xm" 'browse-url-at-point)

;; (require 'w3m-search)
;; (add-to-list 'w3m-search-engine-alist
;;              '("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s"))

;;; Gnu Server Settings
(message "applying gnuserv settings ...")
(cond ((eq system-type 'darwin)
       (autoload 'gnuserv-start "gnuserv-compat"
         "Allow this Emacs process to be a server for client processes." t)
       (gnuserv-start))
      ((eq system-type 'gnu/linux)
       (server-start)))

