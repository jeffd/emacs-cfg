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
(add-to-list 'exec-path "/Applications/Racket/bin")
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

(require 'package) ;; You might already have this line
(setq package-user-dir "~/.emacs-cfg/emacs.d/elpa")
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar jeffsPackages
  '(color-theme-solarized
    magit
    ein
    elpy
    ack
    full-ack
    ack-menu
    js2-mode
    go-mode
    go-complete
    go-playground
    go-eldoc
    go-guru
    godoctor
    go-rename
    go-gen-test
    go-errcheck
    go-direx
    exec-path-from-shell
    flycheck-gometalinter
    mark-multiple
    markdown-mode
    gh-md
    paredit
    ;racket-mode
    ac-geiser
    sml-mode
    gnu-apl-mode
    ;quack
    swift-mode
    company-sourcekit
    csharp-mode
    coffee-mode
    slime
    flycheck
    py-autopep8
    yaml-mode
    realgud
    graphviz-dot-mode
    omnisharp
    sphinx-doc
    python-docstring
    graphql-mode
    dockerfile-mode
    protobuf-mode
    go-dlv
    go-fill-struct
    cmake-mode
    cmake-ide
    cmake-font-lock
    cask-mode
    cask
    rvm
    ruby-mode
    ;osx-plist
    markdown-mode+
    magit-filenotify
    json-mode
    inf-ruby
    git-timemachine
    flycheck-clojure
    diff-git
    css-mode
    columnify
    rtags
    w3))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      jeffsPackages)

;;; Will remove when there is a true GNU Operating System
(setq inhibit-start-screen 1)
(setq inhibit-splash-screen 1)

(setq visible-bell (eq system-type 'gnu/linux))

;;; Mac keyboard on Linux
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;;; Mouse clicks on macOS
;;; Via: https://joelkuiper.eu/spellcheck_emacs
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;;; I condem thee to Hell!
(global-set-key (kbd "C-x C-c") nil)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(setq tramp-default-method "ssh")

;;; Custom Vairables
(message "applying gnuserv settings ...")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (godoctor go-fill-struct go-gen-test go-rename full-ack rtags cmake-font-lock cmake-ide cmake-mode go-dlv protobuf-mode dockerfile-mode go-complete go-playground sicp graphql-mode python-docstring sphinx-doc markdown-mode markdown-mode+ flycheck-gometalinter go-mode ack-menu ack company-sourcekit omnisharp realgud csharp-mode w3 tex-math-preview slime-repl rvm ruby-mode osx-plist magit-filenotify json-mode inf-ruby go-errcheck go-eldoc go-direx go-autocomplete git-timemachine gist flycheck-clojure diff-git css-mode columnify color-theme-solarized ac-geiser)))
 '(scheme48-keywords
   (quote
    ((dynamic-wind 0 nil)
     (destructure 1 nil)
     (enum-case 2 nil)
     (environment-define! 2 no-font-lock)
     (environment-set! 2 no-font-lock)
     (guard 1 nil)
     (iterate 3 nil)
     (make-usual-resumer 2 no-font-lock)
     (mvlet 1 nil)
     (mvlet* 1 nil)
     (search-tree-modify! 2 no-font-lock)
     (usual-resumer 0 no-font-lock)
     (with-exception-handler 1 nil)
     (with-handler 1 nil)
     (with-interaction-environment 1 nil)
     (with-nondeterminism 0 nil)
     (call-with-current-input-port 1 nil)
     (call-with-current-noise-port 1 nil)
     (call-with-current-output-port 1 nil)
     (call-with-string-output-port 0 nil)
     (limit-output 2 no-font-lock)
     (recurring-write 2 no-font-lock)
     (silently 0 nil)
     (with-current-ports 3 nil)
     (define-interface 1 nil)
     (define-structure 2 nil)
     (structure 1 nil)
     (structures 1 nil)
     (export 0 nil)
     (atomically 0 nil)
     (atomically! 0 nil)
     (call-ensuring-atomicity 0 nil)
     (call-ensuring-atomicity! 0 nil)
     (ensure-atomicity 0 nil)
     (ensure-atomicity! 0 nil)
     (interrupt-thread 1 no-font-lock)
     (let-fluid 2 nil)
     (let-fluids defun nil)
     (spawn-on-scheduler 1 no-font-lock)
     (with-new-proposal 1 nil)
     (with-current-input-port 2 nil)
     (with-current-output-port 2 nil)
     (awk 3 nil)
     (close-after 2 no-font-lock)
     (if-match 2 nil)
     (with-cwd 1 nil)
     (with-cwd* 1 nil)
     (let-optionals scheme-let-indent nil)
     (let-optionals* scheme-let-indent nil)
     (and-let* 1 nil)
     (let-values 1 nil)
     (let*-values 1 nil))))
 '(texinfo-mode-hook (quote (turn-on-auto-fill))))

;;; Font Settings
;;;
;;; You can get Consolas for Linux by following these instructions
;;; http://igordevlog.blogspot.com/2007/05/how-to-consolas-font-in-linux.html
;;;
(message "applying font settings ...")
;; (if (eq system-type 'darwin)
;;     (set-face-attribute 'default nil
;; 			:family "consolas" :height 130)
;;   (set-frame-font "Consolas-13"))

(if (eq system-type 'darwin)
    (set-face-attribute 'default nil
			:family "Operator Mono Medium" :height 130)
  (set-frame-font "Operator Mono Medium 13"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "#859900" :slant italic))))
 '(font-lock-type-face ((t (:foreground "#b58900" :slant italic)))))

;;; Settings Theme
(message "applying theme settings ...")
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
;(color-theme-comidia)
(color-theme-solarized-dark)

;;; Shell Settings
(message "applying shell settings ...")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;;; Full Ack
;(add-path "full-ack")
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

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; Compile Settings
(require 'smart-compile)

;;; C-x b
;(require 'iswitchb) ; Apparenly Obsolete now
;(iswitchb-mode 1)

(require 'ido) ; But I don't like this
(ido-mode t)

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

;;; Git
(message "applying git settings ...")
(global-set-key (kbd "C-x g") 'magit-status)

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
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(add-hook 'swift-mode-hook
          (lambda ()
            (setq-local tab-width 2)
            (defvar swift-indent-offset)
            (setq-local swift-indent-offset 2)))

(setq swift-indent-offset 2)
(setq swift-indent-hanging-comma-offset 2)

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


;;; Objective-C Settings
(message "applying Xcode settings ...")
(setq auto-mode-alist
      (append '(("\\.mm\\'" . objc-mode)
                ("\\.m\\'" . objc-mode)
                ("\\.j\\'" . objj-mode))
              auto-mode-alist))

;(require 'objj-mode)

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

;;; C++ Settings


(defconst my-cpp-style
  '((indent-tabs-mode . nil)
    (c-basic-offset . 4)
    (c-subword-mode 1)
    (tab-width . 4)
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

(c-add-style "jeff-cpp-style" my-cpp-style)

(setq cmake-ide-build-dir "/Users/jdlouhy/Development/normalvr/Realtime/build")

(defun my-c++-mode-hook ()
  (c-set-style "jeff-cpp-style")
  (auto-fill-mode)
  (setq flycheck-clang-language-standard "c++11"))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; Turn flycheck on everywhere
(global-flycheck-mode)

;; (eval-after-load 'c++-mode
;; '(progn
;;    (flycheck-mode)))

;(require 'rtags) ;; optional, must have rtags installed
;; brew install llvm --with-libcxx --with-clang --without-assertions --with-rtti
;; brew link llvm
(cmake-ide-setup)



;;; Scheme Settings
(message "applying scheme settings ...")
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;(autoload 'scheme48-mode "scheme48.el" "Major mode for Scheme48 interaction." t)

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

;; (defun maybe-scheme48-mode ()
;;   "Enter Scheme48 Mode if `scheme48-package' is non-nil.
;; Hack the local variables after doing so in order to maintain the value
;;   of the `scheme48-package' variable if it was set in the `-*-' line."
;;   (if (and (eq major-mode 'scheme-mode)
;;            (boundp 'scheme48-package)
;;            scheme48-package)
;;       (progn
;;         (scheme48-mode)
;;         (hack-local-variables))))

;; (add-hook 'hack-local-variables-hook 'maybe-scheme48-mode)

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

;;; The following function runs Slime with Clojure, even if Slime defaults to another Lisp.
;;; The above configuration alone, however, will make Clojure the default, so all that is necessary
;;; to run Slime with Clojure is M-x slime.
(defun run-clojure ()
  "Starts clojure in Slime"
  (interactive)
  (slime 'clojure))

;;; Standard ML
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

(elpy-enable)
;;(elpy-use-ipython)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (require 'python-docstring)
                              (python-docstring-mode t)
                              (sphinx-doc-mode t)))

;;; C# Settings
(message "applying C# settings ...")

(defun my-csharp-settings-hook ()
  (setq c-basic-offset 4)
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  (electric-pair-local-mode 1)
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)
  (yas-minor-mode))

(add-hook 'csharp-mode-hook 'my-csharp-settings-hook)
(add-hook 'csharp-mode-hook 'omnisharp-mode)

(setq omnisharp-server-executable-path "~/Development/GitHub/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))


;; (eval-after-load "flycheck"
;;   '(progn
;;      (flycheck-define-checker csharp
;;        "A C# syntax checker for dotnet. By default, it uses the Mono
;; compiler. If you would like to use a different compiler, see
;; `csharp-set-flycheck-command'."
;;        :command ("gmcs" "-target:module" source)
;;        :error-patterns
;;        ;; WinFormsHello.cs(17,9): error CS0246: The type or namespace name `derp' could not be found. Are you missing an assembly reference?
;;        ((error line-start (file-name) "(" line "," column "): error " (message) line-end)
;;         (warning line-start (file-name) "(" line "," column "): warning " (message) line-end))
;;        :modes csharp-mode)
;;      (add-hook 'flycheck-before-syntax-check-hook  #'csharp-set-flycheck-command)))

;; (flycheck-define-checker csharp-unity
;;   "Custom checker for Unity projects"
;;   :modes (csharp-mode)
;;   :command ("python" (eval "/Users/jdlouhy/Development/normalvr/Normcore/make.py") "fast" (eval "/Users/jdlouhy/Development/normalvr/Normcore/") source-original source)
;;   :error-patterns((warning line-start (file-name) "(" line (zero-or-more not-newline) "): " (message) line-end)
;;                   (error line-start (file-name) "(" line (zero-or-more not-newline) "): " (message) line-end)))

(eval-after-load "omnisharp"
  '(progn
     (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)))

;;; w3m
(message "applying browser settings ...")

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

;;; js2-mode & Ejacs
(add-path "js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
;(autoload 'js-console "js-console" nil t)
(setq auto-mode-alist
      (append '(("\\.js$" . js2-mode))
              auto-mode-alist))

;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;(define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)

;;; JSLint(require 'flymake-jslint)
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

;;; Markdown Mode
(message "applying Markdown settings ...")
(setq auto-mode-alist
      (cons '("README" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook
          (lambda () (visual-line-mode t)))

;;; Graphviz DOT Language
(setq auto-mode-alist
      (cons '("\\.dot\\'" . graphviz-dot-mode) auto-mode-alist))

;;; Usually, one wants to use a different font for APL buffers.
;;; This mode includes a face called gnu-apl-default which is
;;; used in various places, such as the help buffers. However,
;;; it's not currently enabled by default in the interactive
;;; session, nor in APL buffers.
(defun em-gnu-apl-init ()
  (setq buffer-face-mode-face 'gnu-apl-default)
  (buffer-face-mode))

(add-hook 'gnu-apl-interactive-mode-hook 'em-gnu-apl-init)
(add-hook 'gnu-apl-mode-hook 'em-gnu-apl-init)

;;; Racket / Geiser
(setq geiser-racket-binary "/Applications/Racket/bin/racket")

;;; Golang

;;; See: https://johnsogg.github.io/emacs-golang

;;; For better autocomplete, ,make sure you install:
;;;   go get -u github.com/nsf/gocode
;;; For definitions
;;;   go get github.com/rogpeppe/godef
;;;
;;; Metalinter:
;;;   go get -u gopkg.in/alecthomas/gometalinter.v2
;;;
;;; Others:
;;;   go get -u golang.org/x/tools/cmd/...
;;;   go get -u github.com/rogpeppe/godef/...
;;;   go get -u github.com/nsf/gocode
;;;   go get -u golang.org/x/tools/cmd/goimports
;;;   go get -u golang.org/x/tools/cmd/guru
;;;   go get -u github.com/dougm/goflymake

(defun auto-complete-for-go ()
  (auto-complete-mode 1))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

;; Snag the user's PATH and GOPATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  (flycheck-mode)
  (auto-complete-mode)
  (auto-complete-for-go)
  (ac-config-default)
  (go-eldoc-setup)

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; Misc go stuff
  (auto-complete-mode 1))                         ; Enable auto-complete mode

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; You’ll also need the following (as recommended in gocode issue 325 https://github.com/nsf/gocode/issues/325):
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
