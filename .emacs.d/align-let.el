;;; align-let.el --- align expressions in a lisp "let"

;; Copyright 2005, 2006, 2007 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 5
;; Keywords: languages
;; URL: http://www.geocities.com/user42_kevin/align-let/index.html
;; EmacsWiki: AlignLet

;; align-let.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; align-let.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; http://www.gnu.org/licenses/gpl.txt, or you should have one in the file
;; COPYING which comes with GNU Emacs and other GNU programs.  Failing that,
;; write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA.


;;; Commentary:

;; This is a spot of code for aligning the values in a "let" form.  See
;; examples in the docstring below.  It's designed for elisp and scheme but
;; probably works with other lisp variants.
;;
;; The regexp-based align.el might be able to do the same as the code below,
;; but it seemed to hard to get lisp nesting recognised properly.
;;
;; Designed for Emacs 21, works with XEmacs 21 too.

;;; Install:

;; Add to your .emacs
;;
;;     (autoload 'align-let "align-let" nil t)
;;
;; to get M-x align-let.  The suggested keybinding is C-c C-a, which can be
;; made from desired mode hooks with
;;
;;     (autoload 'align-let-keybinding "align-let" nil t)
;;     (add-hook 'emacs-lisp-mode-hook 'align-let-keybinding)
;;     (add-hook 'scheme-mode-hook     'align-let-keybinding)
;;
;; or of course with explicit define-key to some other key.

;;; History:

;; Version 1 - the first version.
;; Version 2 - recognise Scheme named let.
;; Version 3 - tighten up a regexp, add autoloads and keybind function.
;; Version 4 - don't align a value on a new line, handle bare elisp vars.
;; Version 5 - fix for comments between binding forms.


;;; Code:

;;;###autoload
(defun align-let ()
  "Align the value expressions for the variables in a Lisp `let' form.
Point should be within or immediately in front of the let form.  It
changes for instance

    (let ((x 1)
          (foo   2)
          (zz (blah)))
      ...)

to

    (let ((x   1)
          (foo 2)
          (zz  (blah)))
      ...)


When point is somewhere in the middle of the form, possibly nested in an
expression, the beginning is found by looking for a pattern \"(sym ((...\"
or \"(and-let* (\".

The symbols that might introduce the form (`let', `let*', etc) are not hard
coded, this allows `align-let' to adapt to forms specific to various Lisp
dialects.  `and-let*' from Scheme is explicitly recognised, since it can
start with a bare variable rather than a binding form."

  (interactive)
  (save-excursion
    ;; look outwards for a containing let expression
    (let ((limit (save-excursion (beginning-of-defun) (point))))
      (while (not (looking-at "\\s-*\\((\\s-*[^ \t\n(]+\\(\\s-+[^ \t\n(]+\\)?\\s-*(\\)(\\|\\s-*(and-let\\*\\s-+("))
        (if (<= (point) limit)
            (error "Not in a \"let\" form"))
        (condition-case nil
            (up-list -1)
          (error (goto-char (point-min))))))
    (goto-char (or (match-end 1) (match-end 0)))

    ;; here we have point at the first binding, like the `(foo 1)' of
    ;; `(let ((foo 1) ...) ...)'

    ;; find the maximum var name width
    (let ((width 0))
      (save-excursion
        (while (progn
                 ;; ignore bare variables
                 (when (looking-at "\\(\\s-\\|\n\\)*(")
                   (save-excursion
                     (down-list)
                     ;; ignore one-element scheme form `(foo)' as in
                     ;; `(and-let* ((   foo) ...)'
                     (when (condition-case nil (scan-sexps (point) 2)
                             (error nil))
                       (let ((col (current-column)))
                         (forward-sexp)
                         (setq width (max width (- (current-column) col)))))))
                 (forward-sexp)
                 (forward-comment (buffer-size))
                 (not (looking-at ")")))))
      (setq width (1+ width))

      ;; align expressions to that width
      (while (progn
               (when (looking-at "(") ;; ignore bare variables
                 (save-excursion
                   (down-list)

                   (let ((col    (current-column))
                         (endvar (progn ;; position variable name ends
                                   (forward-sexp)
                                   (point))))
                     ;; go to second form, or stay on first for one-element
                     ;; and-let*
                     (condition-case nil (forward-sexp) (error nil))
                     (backward-sexp)

                     ;; if the value is on a different line then leave it alone
                     (unless (and (>= (point) endvar)
                                  (search-backward "\n" endvar t))

                       (let ((this-width (- (current-column) col)))
                         (cond ((> this-width width)
                                (move-to-column (+ col width) t)
                                (let ((beg (point)))
                                  (forward-sexp)
                                  (backward-sexp)
                                  (delete-region beg (point))))
                               ((< this-width width)
                                (insert (make-string (- width this-width)
                                                     ? )))))))))
               (forward-sexp)
               (forward-comment (buffer-size))
               (not (looking-at ")")))))))

;;;###autoload
(defun align-let-keybinding ()
  "Bind C-c C-a to `align-let' in the current local keymap.
This is meant for use from the mode hook of a lisp-like language,
like `emacs-lisp-mode-hook' or `scheme-mode-hook'."
  (define-key (current-local-map) [?\C-c ?\C-a] 'align-let))

;;;###autoload
(custom-add-option 'emacs-lisp-mode-hook 'align-let-keybinding)
;;;###autoload
(custom-add-option 'scheme-mode-hook     'align-let-keybinding)

(provide 'align-let)

;;; align-let.el ends here
