(require 'flymake)

(setq flymake-js-method 'closure)
(defun flymake-js-toggle-method ()
  (interactive)
  (let ((methods '#1=(spidermonkey jslint . #1#)))
    (setq flymake-js-method (cadr (memq flymake-js-method methods)))
    (flymake-start-syntax-check)))

(defun flymake-js-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (cond ((eq flymake-js-method 'spidermonkey)
           (list "smjs" (list "-s" "-e" (format "load('%s')" (expand-file-name "/path/to/mock.js")) local-file)))
          ((eq flymake-js-method 'rhino)
           (list "java" (list  "-cp" "/usr/local/rhino/js.jar" "org.mozilla.javascript.tools.shell.Main" (expand-file-name "~/.emacs-cfg/emacs.d/jslint.js") local-file)))
          ((eq flymake-js-method 'closure)
           ;;; brew install closure-compiler
           ;;; sudo ln -s /usr/local/bin/closure /bin/closure
           (list "closure" (list "--warning_level" "VERBOSE" "--js_output_file" "/dev/null" "--js" local-file))))))


(eval-after-load "flymake"
  '(progn
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.js\\(on\\)?$" flymake-js-init flymake-simple-cleanup flymake-get-real-file-name))
     (add-to-list 'flymake-err-line-patterns
                  '("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3))
     (add-to-list 'flymake-err-line-patterns
                  '("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$" 1 2 nil 3))
     (add-to-list 'flymake-err-line-patterns
                  '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$" nil 1 2 3))))

(defun my-js-setup-flymake ()
  (flymake-mode 1)
  (local-set-key (kbd "C-c n") 'flymake-goto-next-error)
  (local-set-key (kbd "C-c p") 'flymake-goto-prev-error)
  (local-set-key (kbd "C-c t") 'flymake-js-toggle-method))

(add-hook 'javascript-mode-hook 'my-js-setup-flymake)

(provide 'flymake-jslint)
