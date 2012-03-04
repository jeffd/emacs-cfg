;;; -*- Mode: Emacs-Lisp -*-
;;; Jeff Dlouhy's Emacs Settings
;;;
;;; This has git submodules and may fail to load if
;;; they are not downloaded. Do so by running:
;;;   git submodule init
;;;   git submodule update

(message "started loading settings ...")

(setq custom-basedir (expand-file-name "~/.emacs-cfg/emacs.d/"))
(add-to-list 'load-path custom-basedir)
(add-to-list 'exec-path "~/.emacs-cfg/emacs.d")
(add-to-list 'exec-path "/usr/local/plt/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/bin/python")
(add-to-list 'exec-path "/usr/local/git/bin/")
(add-to-list 'exec-path "/usr/texbin")
(add-to-list 'exec-path "/Applications/Graphviz.app/Contents/MacOS/")

(defun add-path (p)
  (add-to-list 'load-path (concat custom-basedir p)))

;;; Emacs Lisp Package Archive
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
(message "applying ELPA settings ...")
(setq package-user-dir "~/.emacs-cfg/emacs.d/elpa")
(when
    (load
     (expand-file-name "~/.emacs-cfg/emacs.d/elpa/package.el"))
  (package-initialize))

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
;;                          ("gnu" . "http://elpa.gnu.org/packages/")))

;; (add-path "elpa")
;; (load "package")
;; (package-initialize)

;;; Will remove when there is a true GNU Operating System
(setq inhibit-start-screen 1)
(setq inhibit-splash-screen 1)

(setq visible-bell (eq system-type 'gnu/linux))

;;; Mac keyboard on Linux
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;;; I condem thee to Hell!
(global-set-key (kbd "C-x C-c") nil)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;; Custom Vairables
(message "applying gnuserv settings ...")
(custom-set-variables
 '(scheme48-keywords (quote ((dynamic-wind 0 nil) (destructure 1 nil) (enum-case 2 nil) (environment-define! 2 no-font-lock) (environment-set! 2 no-font-lock) (guard 1 nil) (iterate 3 nil) (make-usual-resumer 2 no-font-lock) (mvlet 1 nil) (mvlet* 1 nil) (search-tree-modify! 2 no-font-lock) (usual-resumer 0 no-font-lock) (with-exception-handler 1 nil) (with-handler 1 nil) (with-interaction-environment 1 nil) (with-nondeterminism 0 nil) (call-with-current-input-port 1 nil) (call-with-current-noise-port 1 nil) (call-with-current-output-port 1 nil) (call-with-string-output-port 0 nil) (limit-output 2 no-font-lock) (recurring-write 2 no-font-lock) (silently 0 nil) (with-current-ports 3 nil) (define-interface 1 nil) (define-structure 2 nil) (structure 1 nil) (structures 1 nil) (export 0 nil) (atomically 0 nil) (atomically! 0 nil) (call-ensuring-atomicity 0 nil) (call-ensuring-atomicity! 0 nil) (ensure-atomicity 0 nil) (ensure-atomicity! 0 nil) (interrupt-thread 1 no-font-lock) (let-fluid 2 nil) (let-fluids defun nil) (spawn-on-scheduler 1 no-font-lock) (with-new-proposal 1 nil) (with-current-input-port 2 nil) (with-current-output-port 2 nil) (awk 3 nil) (close-after 2 no-font-lock) (if-match 2 nil) (with-cwd 1 nil) (with-cwd* 1 nil) (let-optionals scheme-let-indent nil) (let-optionals* scheme-let-indent nil) (and-let* 1 nil) (let-values 1 nil) (let*-values 1 nil))))
 '(texinfo-mode-hook (quote (turn-on-auto-fill))))

;;; Font Settings
;;;
;;; You can get Consolas for Linux by following these instructions
;;; http://igordevlog.blogspot.com/2007/05/how-to-consolas-font-in-linux.html
;;;
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

;;; Full Ack
(add-path "full-ack")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;;; Ack Shell Trick
(eval-after-load "shell"
  '(progn
     (define-key shell-mode-map (kbd "C-x p") 'find-file-at-point)))

;;; Smooth Scrolling
(message "applying scrolling settings ...")
(setq scroll-step 1
      scroll-conservatively 10000)

;;; Spelling Setting
;;;
;;; brew install aspell --lang=en
;;;
(message "applying scrolling settings ...")
(setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra"))

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
       (global-set-key [(meta return)] 'ns-toggle-fullscreen)))



;;; Auto-Complete
;;; It is easy to install by using a installation script called etc/install.el that is located in the package directory.
;;;
;;; Type M-x load-file RET
;;;
(message "loading Auto-Complete ...")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs-cfg/emacs.d/ac-dict")
(ac-config-default)

;;; Snippet settings
(message "loading yasnippet customizations ...")
(add-path "yasnippet")
(require 'yasnippet)

(yas/initialize)
(yas/load-directory "~/.emacs-cfg/emacs.d/yasnippet/snippets")

(setq yas/extra-mode-hooks
      '(ruby-mode-hook actionscript-mode-hook ox-mode-hook objc-mode-hook cc-mode-hook python-mode-hook))

;;;using YASnippet with AutoComplete
(load "auto-complete-yasnippet.el")
;(require 'auto-complete-yasnippet)
(message "auto-complete-yasnippet load successful")

;;; Git
(message "applying git settings ...")
(require 'git)
(require 'gitsum)

;;; Mercurial
(message "loading Mercurial settings ...")
(add-path "ahg")
(require 'ahg)

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

;;; JS Interpreter
;; (require 'js-comint)
;; (setq inferior-js-program-command "/usr/local/bin/objj")
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
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(autoload 'scheme48-mode "scheme48.el" "Major mode for Scheme48 interaction." t)

;(require 'scheme48)

;; (add-hook ’hack-local-variables-hook
;;            (lambda ()
;;              (if (and (boundp ’scheme48-package)
;;                       scheme48-package) (progn (scheme48-mode)
;;                                                (hack-local-variables-prop-line)))))


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



(autoload 'scheme48-safe-variable "scheme48"
  "Return non-nil when VAR is a valid value of `scheme48-package'.")

(put 'scheme48-package 'safe-local-variable 'scheme48-safe-variable)

(defun maybe-scheme48-mode ()
  "Enter Scheme48 Mode if `scheme48-package' is non-nil.
Hack the local variables after doing so in order to maintain the value
  of the `scheme48-package' variable if it was set in the `-*-' line."
  (if (and (eq major-mode 'scheme-mode)
           (boundp 'scheme48-package)
           scheme48-package)
      (progn
        (scheme48-mode)
        (hack-local-variables))))

(add-hook 'hack-local-variables-hook 'maybe-scheme48-mode)

(add-to-list 'interpreter-mode-alist '("scsh" . scheme48-mode))
(setq scheme-program-name "scsh")
(if window-system (show-paren-mode +1))

(setq auto-mode-alist
      (append '(("\\.scm$" . scheme48-mode)
                ("\\.ss$"  . scheme-mode) ; PLT & Chez
                ("\\.sch$" . scheme-mode) ; Bigloo & Larceny
                ("\\.sc$"  . scheme-mode) ; JMS
                ("\\.asd$" . lisp-mode))  ; ASDF system files
              auto-mode-alist))

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
(add-path "slime48")
;; (require 'slime)
;; (slime-setup)

(autoload 'slime "slime"
  "Start an inferior^_superior Lisp and connect to its Swank server."
  t)

(autoload 'slime-mode "slime"
  "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)."
  t)

;;; slime48

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

;; (add-path "swank-clojure")
;; (require 'swank-clojure-autoload)

;;; The following function runs Slime with Clojure, even if Slime defaults to another Lisp.
;;; The above configuration alone, however, will make Clojure the default, so all that is necessary
;;; to run Slime with Clojure is M-x slime.
(defun run-clojure ()
  "Starts clojure in Slime"
  (interactive)
  (slime 'clojure))

;;; Standard ML
(add-path "sml")
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)
(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))

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

;;; Flymake and Python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;;; Zencoding
;; (message "applying zencoding settings ...")
;; (add-path "zencoding")
;; (require 'zencoding-mode)
;; ;; Auto-start on any markup modes
;; (add-hook 'sgml-mode-hook 'zencoding-mode)

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
(message "applying browser settings ...")
;; (if (and (= emacs-major-version 23)
;;          (eq system-type 'gnu/linux))
;;     (progn
;;       (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
;;       (require 'w3m-load))
;;   (require 'w3m))

(setq browse-url-browser-function 'w3-fetch)
 (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 ;; optional keyboard short-cut
 (global-set-key "\C-xm" 'browse-url-at-point)

;(require 'w3m-search)
;; (add-to-list 'w3m-search-engine-alist
;;              '("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s"))

;;; Smart Tabs
(setq-default tab-width 2)
(setq cua-auto-tabify-rectangles nil)
(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))
(defmacro smart-tabs-advice (function offset)
  (defvaralias offset 'tab-width)
  `(defadvice ,function (around smart-tabs activate)
     (cond
      (indent-tabs-mode
       (save-excursion
         (beginning-of-line)
         (while (looking-at "\t*\\( +\\)\t+")
           (replace-match "" nil nil nil 1)))
       (setq tab-width tab-width)
       (let ((tab-width fill-column)
             (,offset fill-column))
         ad-do-it))
      (t
       ad-do-it))))

;;; Web Development
(setq css-indent-offset 2)
(setq css-indent-level 2)
(smart-tabs-advice js2-indent-line js2-basic-offset)

;;; nXhtml
(add-path "nxhtml")
(load (expand-file-name "~/.emacs-cfg/emacs.d/nxhtml/autostart.el"))
(setq mumamo-chunk-coloring 'no-chunks-colored)

;;; HTML5
;;; Must add the submodule then run 'make relaxng' to get the schemas
(add-path "html5")
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/.emacs-cfg/emacs.d/html5/schemas.xml"))

;(require 'whattf-dt)

;;; js2-mode & Ejacs
(add-path "js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
;(autoload 'js-console "js-console" nil t)
(setq auto-mode-alist
      (append '(("\\.js$" . js2-mode))
              auto-mode-alist))

;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; js2 addons
(add-path "mark-multiple")
(add-path "js2-refactor")
(require 'js2-highlight-vars)
(require 'js2-refactor)

(define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)

;;; JSLint
(require 'flymake-jslint)
(add-hook 'js2-mode-hook
	  (lambda () (flymake-mode 1)))

;;; Javascript
(require 'json)
(add-hook 'js2-mode-hook
   '(lambda ()
   (setq js2-basic-offset 2)
   (setq js2-use-font-lock-faces t)))

;;; Roku BrightScript
;;; https://bitbucket.org/markroddy/brightscript-mode/overview
(add-path "brightscript-mode")
(require 'brightscript-mode)
(setq auto-mode-alist
      (append '(("\\.brs\\'" . brightscript-mode))
              auto-mode-alist))

;;; CoffeeScript
(add-path "coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;;; Ejacs
(add-path "js")
(autoload 'js-console "js-console" nil t)

;;; BNF Fontlock
(message "Loading BNF Mode ...")
(define-generic-mode 'bnf-mode
  () ;; comment char: inapplicable because # must be at start of line
  nil ;; keywords
  '(
    ("^#.*" . 'font-lock-comment-face) ;; comments at start of line
    ("^<.*?>" . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>" . 'font-lock-builtin-face) ;; other nonterminals
    ("::=" . 'font-lock-const-face) ;; "goes-to" symbol
    ("\|" . 'font-lock-warning-face) ;; "OR" symbol
    ("\{:\\|:\}" . 'font-lock-keyword-face) ;; special pybnf delimiters
   )
  '("\\.bnf\\'" "\\.pybnf\\'") ;; filename suffixes
  nil ;; extra function hooks
  "Major mode for BNF highlighting.")

(setq auto-mode-alist
      (append '(("\\.grm\\'" . bnf-mode))
              auto-mode-alist))

;;; Lua
(require 'flymake-lua)
(add-hook 'lua-mode-hook 'flymake-lua-load)

;;; Markdown Mode
(message "applying Markdown settings ...")
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("README" . markdown-mode) auto-mode-alist))

;;; Graphviz DOT Language
(autoload 'graphviz-dot-mode "graphviz-dot-mode.el"
  "Major mode for editing Graphviz DOT files" t)
(setq auto-mode-alist
      (cons '("\\.dot\\'" . graphviz-dot-mode) auto-mode-alist))

;;; AucTeX
;;;
;;; Installed on OS X with
;;; ./configure --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs --with-lispdir=/Applications/Emacs.app/Contents/Resources/site-lisp --with-texmf-dir=/usr/local/texlive/texmf-local
;;;
;;; From http://superuser.com/questions/171681/installing-auctex-1-86-over-emacs-app-in-os-x
;;;
;; (setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
;; (setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
>>>>>>> Stashed changes

;;; Gnu Server Settings
(message "applying gnuserv settings ...")
(cond ((eq system-type 'darwin)
       (autoload 'gnuserv-start "gnuserv-compat"
         "Allow this Emacs process to be a server for client processes." t)
       (gnuserv-start))
      ((eq system-type 'gnu/linux)
       (server-start)))

;;; Chrome Edit Server
(if (locate-library "edit-server")
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start)))


