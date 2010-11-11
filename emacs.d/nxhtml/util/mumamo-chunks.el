;;; mumamo-chunks.el --- Chunk dividing routines etc
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-05-13 Thu
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   Cannot open load file: mumamo-chunks.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Chunk dividing routines must be compiled already when calling
;; `define-mumamo-multi-major-mode'.  Since I can't figure out how to
;; do that in the same file I keep those here instead.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (add-to-list 'load-path default-directory))
(eval-when-compile (require 'mumamo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; File wide key bindings

(defun mumamo-multi-mode-map ()
  "Return mumamo multi mode keymap."
  (symbol-value
   (intern-soft (concat (symbol-name mumamo-multi-major-mode) "-map"))))

;; (defun mumamo-multi-mode-hook-symbol ()
;;   "Return mumamo multi mode hook symbol."
;;   (intern-soft (concat (symbol-name mumamo-multi-major-mode) "-hook")))

;;;###autoload
(defun mumamo-define-html-file-wide-keys ()
  "Define keys in multi major mode keymap for html files."
  (let ((map (mumamo-multi-mode-map)))
    (define-key map [(control ?c) (control ?h) ?b] 'nxhtml-browse-file)
    ))
;; (defun mumamo-add-html-file-wide-keys (hook)
;;   (add-hook hook 'mumamo-define-html-file-wide-keys)
;;   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chunk search routines for XHTML things

(defun mumamo-chunk-attr= (pos max attr= attr=is-regex attr-regex submode)
  "This should work similar to `mumamo-possible-chunk-forward'.
See `mumamo-chunk-style=' for an example of use.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-chunk-attr=-new pos max attr= attr=is-regex attr-regex submode))

(defun mumamo-chunk-attr=-new-fw-exc-fun (pos max)
  ;;(msgtrc "(mumamo-chunk-attr=-new-fw-exc-fun %s %s)" pos max)
  (save-match-data
    (let ((here (point))
          first-dq
          next-dq
          (this-chunk (mumamo-get-existing-new-chunk-at pos)))
      (if this-chunk
          (goto-char (overlay-end this-chunk))
        (goto-char (overlay-end mumamo-last-chunk)))
      (setq first-dq (search-forward "\"" max t))
      (unless (bobp)
        (backward-char)
        (condition-case err
            (with-syntax-table (standard-syntax-table)
              (setq next-dq (scan-sexps (point) 1)))
          (error nil)))
      (prog1
          next-dq
        (goto-char here)))))

(defun mumamo-chunk-attr=-new-find-borders-fun (start-border end-border dummy)
  ;;(setq borders (funcall find-borders-fun start-border end-border exc-mode))
  (save-match-data
    (let ((here (point))
          (end2 (when end-border (1- end-border)))
          start2)
      (goto-char start-border)
      (save-match-data
        (setq start2 (search-forward "\"" (+ start-border 200) t)))
      (goto-char here)
      (list start2 end2))))

(defun mumamo-chunk-attr=-new (pos
                               ;;min
                               max
                               attr=
                               attr=is-regex
                               attr-regex
                               submode)
  ;;(message "\n(mumamo-chunk-attr=-new %s %s %s %s %s %s)" pos max attr= attr=is-regex attr-regex submode)
  ;;(mumamo-condition-case err
  (condition-case err
      (save-match-data
        (let ((here (point))
              (next-attr= (progn
                            ;; fix-me:
                            (if (not attr=is-regex)
                                (goto-char (+ pos (length attr=)))
                              (goto-char pos)
                              (skip-chars-forward "a-zA-Z="))
                            (goto-char pos)
                            (if attr=is-regex
                                (re-search-forward attr= max t)
                              (search-forward attr= max t))))
              next-attr-sure
              ;;next-attr=
              start start-border
              end   end-border
              exc-mode
              borders
              exc-start-next
              exc-end-next
              exc-start-next
              exc-end-next
              (tries 0)
              (min (1- pos))
	      look-max
              )
          ;; make sure if we have find prev-attr= or not
          (unless (eq (char-after) ?\")
            (setq next-attr= nil))
          (when next-attr=
            (forward-char)
            (skip-chars-forward "^\"")
            (setq look-max (+ (point) 2)))
          (while (and next-attr=
                      (< min (point))
                      (not next-attr-sure)
                      (< tries 5))
            ;;(msgtrc "attr=-new: min=%s, point=%s" min (point))
            (setq tries (1+ tries))
            ;;(if (not (re-search-backward "<[^?]" (- min 300) t))
            (if (not (re-search-backward "<[^?]\\|\?>" (- min 300) t))
                (setq next-attr= nil)
              ;;(if (looking-at attr-regex)
              (if (let ((here (point)))
                    (prog1
                        (re-search-forward attr-regex look-max t)
                      (goto-char here)))
              ;;(if (mumamo-end-in-code (point) next-attr= 'php-mode)
                  (setq next-attr-sure 'found)
                (unless (bobp)
                  (backward-char)
                  ;;(msgtrc "attr=-new 1: min=%s, point=%s" min (point))
                  (setq next-attr= (if attr=is-regex
                                       (re-search-backward attr= (- min 300) t)
                                     (search-backward attr= (- min 300) t)))))))
          (unless next-attr-sure (setq next-attr= nil))


          ;; find prev change and if inside style= the next change
          (when next-attr=
              (setq exc-start-next (match-beginning 1))
              (setq exc-end-next   (match-end 2))
              (when (>= exc-start-next pos)
                (if (> pos exc-end-next)
                    (progn
                      (setq start (+ (match-end 2) 1))
                      ;;(setq start-border (+ (match-end 2) 2))
                      )
                  (setq exc-mode submode)
                  (setq start (match-beginning 1))
                  (setq start-border (match-beginning 2))
                  (setq end (1+ (match-end 2)))
                  (setq end-border (1- end)))
                ))
          ;; find next change
          (unless end
            (if start
                (goto-char start)
              (goto-char pos)
              (search-backward "<" min t))
            ;;(msgtrc "attr=-new 2: min=%s, point=%s" min (point))
            (setq next-attr= (if attr=is-regex
                                 (re-search-forward attr= max t)
                               (search-forward attr= max t)))
            (when (and next-attr=
                       (search-backward "<" min t))
              (when (looking-at attr-regex)
                (setq end (match-beginning 1)))))
          (when start (assert (>= start pos) t))
          (when end   (assert (<= pos end) t))
          ;;(message "start-border=%s end-border=%s" start-border end-border)
          (when (or start-border end-border)
            (setq borders (list start-border end-border nil)))
          ;; (message "mumamo-chunk-attr=-new: %s"
          ;;          (list start
          ;;                end
          ;;                exc-mode
          ;;                borders
          ;;                nil ;; parseable-by
          ;;                'mumamo-chunk-attr=-new-fw-exc-fun ;; fw-exc-fun
          ;;                'mumamo-chunk-attr=-new-find-borders-fun ;; find-borders-fun
          ;;                ))
          (goto-char here)
          (setq end nil)
          (when (or start end)
            (list start
                  end
                  exc-mode
                  borders
                  nil ;; parseable-by
                  'mumamo-chunk-attr=-new-fw-exc-fun ;; fw-exc-fun
                  'mumamo-chunk-attr=-new-find-borders-fun ;; find-borders-fun
                  ))))
    (error (mumamo-display-error 'mumamo-chunk-attr=-new "%s" (error-message-string err)))
    ))

;;;; xml pi

(defvar mumamo-xml-pi-mode-alist
  '(("php"    . php-mode)
    ("python" . python-mode))
  "Alist used by `mumamo-chunk-xml-pi' to get chunk mode." )

;; Fix-me: make it possible to make the borders part of the php chunk
;; so that parsing of them by nxml may be skipped. Or, rather if the
;; borders are not part of the chunk then assume nxml can not parse
;; the chunk and the borders.
;; (defun mumamo-search-bw-exc-start-xml-pi-1 (pos min lt-chars)
;;   "Helper for `mumamo-chunk-xml-pi'.
;; POS is where to start search and MIN is where to stop.
;; LT-CHARS is just <?.

;; Actual use is in `mumamo-search-bw-exc-start-xml-pi'."
;;   (let ((exc-start (mumamo-chunk-start-bw-str (+ pos 2) min lt-chars))
;;         spec
;;         exc-mode
;;         hit)
;;     (when exc-start
;;       (goto-char exc-start)
;;       (when (and (not (looking-at "xml"))
;;                  (looking-at (rx (0+ (any "a-z")))))
;;         ;; (setq exc-start (match-end 0)) include it in sub chunk instead
;;         (setq exc-start (- exc-start 2))
;;         (setq spec (match-string-no-properties 0))
;;         (setq exc-mode (assoc spec mumamo-xml-pi-mode-alist))
;;         (when exc-mode (setq exc-mode (cdr exc-mode)))
;;         (setq hit t)
;;         )
;;       (when hit
;;         (unless exc-mode
;;           ;;(setq exc-mode 'fundamental-mode)
;;           ;; Fix-me: Better assume php-mode
;;           (setq exc-mode 'php-mode))
;;         (when (<= exc-start pos)
;;           ;;(cons exc-start exc-mode)
;;           (list exc-start exc-mode nil)
;;           )))))

;; (defun mumamo-search-bw-exc-start-xml-pi (pos min)
;;   "Helper for `mumamo-chunk-xml-pi'.
;; POS is where to start search and MIN is where to stop."
;;   (mumamo-search-bw-exc-start-xml-pi-1 pos min "<?"))

(defun mumamo-search-fw-exc-start-xml-pi-new (pos max)
  (let ((here (point))
        start
        spec
        exc-mode
        ret)
    (setq start (search-forward "<?" max t))
    (when (and start
               (looking-at (rx (0+ (any "a-z")))))
      (setq spec (match-string-no-properties 0))
      (unless (string= spec "xml")
        (when (= 0 (length spec))
          (setq spec "php"))
        (setq exc-mode (assoc spec mumamo-xml-pi-mode-alist))
        (if exc-mode
            (setq exc-mode (cdr exc-mode))
          (setq exc-mode 'mumamo-bad-mode))
        (setq ret (list (- start 2) exc-mode nil))))
    (goto-char here)
    ret))

(defun mumamo-xml-pi-end-is-xml-end (pos)
  "Return t if the ?> at pos is end of <?xml."
  (when (> 1000 pos)
;;;     (assert (and (= (char-after pos) ??)
;;;                  (= (char-after (1+ pos)) ?>)))
    (save-excursion
      (save-restriction
        (widen)
        (save-match-data
          (when (search-backward "<" (- pos 150) t)
            (when (looking-at (rx line-start "<\?xml" (1+ space)))
              (mumamo-msgfntfy "mumamo-xml-pi-end-is-xml-end %s => t" pos)
              t)))))))

;; (defun mumamo-search-bw-exc-end-xml-pi (pos min)
;;   "Helper for `mumamo-chunk-xml-pi'.
;; POS is where to start search and MIN is where to stop."
;;   ;; Fix me: merge xml header
;;   (mumamo-msgfntfy "mumamo-search-bw-exc-end-xml-pi %s %s" pos min)
;;   ;;(let ((end-pos (mumamo-chunk-end-bw-str pos min "?>")))
;;   (let ((end-pos (mumamo-chunk-end-bw-str-inc pos min "?>")))
;;     (mumamo-msgfntfy "  end-pos=%s" end-pos)
;;     (when end-pos
;;       (unless (or (mumamo-xml-pi-end-is-xml-end end-pos)
;;                   (= (save-restriction
;;                        (widen)
;;                        (char-after (- end-pos 1)))
;;                      ?<))
;;         (mumamo-msgfntfy "  returning end-pos")
;;         end-pos))))

(defun mumamo-search-fw-exc-end-xml-pi (pos max)
  "Helper for `mumamo-chunk-xml-pi'.
POS is where to start search and MAX is where to stop."
  ;; Fix me: merge xml header
  ;;(let ((end-pos (mumamo-chunk-end-fw-str pos max "?>")))
  (save-match-data
    (let ((end-pos (mumamo-chunk-end-fw-str-inc pos max "?>")))
      (when end-pos
        (unless (mumamo-xml-pi-end-is-xml-end end-pos)
          end-pos)))))

(defun mumamo-search-fw-exc-start-xml-pi-1 (pos max lt-chars)
  "Helper for `mumamo-chunk-xml-pi'.
POS is where to start search and MAX is where to stop.

Used in `mumamo-search-fw-exc-start-xml-pi'.  For an explanation
of LT-CHARS see `mumamo-search-bw-exc-start-xml-pi-1'."
  (goto-char pos)
  (skip-chars-backward "a-zA-Z")
  ;;(let ((end-out (mumamo-chunk-start-fw-str (point) max lt-chars)))
  (let ((end-out (mumamo-chunk-start-fw-str-inc (point) max lt-chars))
        spec
        exc-mode
        hit)
    (when (looking-at "xml")
      (if t ;(= 1 pos)
          (setq end-out (mumamo-chunk-start-fw-str-inc (1+ (point)) max lt-chars))
        (setq end-out nil)))
    (when end-out
      ;; Get end-out:
      (if (looking-at (rx (0+ (any "a-z"))))
          (progn
            ;;(setq end-out (match-end 0))
            (setq end-out (- (match-beginning 0) 2))
            (setq spec (match-string-no-properties 0))
            (setq exc-mode (assoc spec mumamo-xml-pi-mode-alist))
            (if exc-mode
                (setq exc-mode (cdr exc-mode))
              (setq exc-mode 'php-mode))
            (setq end-out (list end-out exc-mode nil))
            )
        (setq end-out nil))
      end-out)))

(defun mumamo-search-fw-exc-start-xml-pi-old (pos max)
  "Helper for `mumamo-chunk-xml-pi'.
POS is where to start search and MAX is where to stop."
  (mumamo-search-fw-exc-start-xml-pi-1 pos max "<?"))

;; Add a find-borders-fun here so that for example src="<?php some
;; code ?>" can be handled.
;;
;; Fix-me: Maybe generalize for other values than <?php
(defun mumamo-find-borders-xml-pi (start end exc-mode)
  (let (start-border
        end-border
        (inc t)
        ;;(begin-mark "<?php")
        (begin-mark "<?")
        (end-mark "?>")
        (here (point)))
    (if (and inc) ;; exc-mode)
        (progn
          (when start
            ;;(setq start-border (+ start (length begin-mark)))
            (goto-char (+ start (length begin-mark)))
            (skip-chars-forward "=a-zA-Z")
            (setq start-border (point))
            )
          (when end
            (setq end-border
                  (- end (length end-mark)))))
      (if (and (not inc) (not exc-mode))
          (progn
            (when start
              (setq start-border
                    (+ start (length end-mark))))
            (when end
              (setq end-border (- end (length begin-mark)))
              ;;(goto-char end)
              ;;(skip-chars-forward "=a-zA-Z")
              ;;(setq end-border (point))
              ))))
    (goto-char here)
    (when (or start-border end-border)
      (list start-border end-border))))

(defun mumamo-chunk-xml-pi (pos max)
  "Find process instruction, <? ... ?>.  Return range and wanted mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-xml-pi-new
                                 'mumamo-search-fw-exc-end-xml-pi
                                 'mumamo-find-borders-xml-pi))


;;;; <style ...>

(defconst mumamo-style-tag-start-regex
  (rx "<style"
      space
      (0+ (not (any ">")))
      "type"
      (0+ space)
      "="
      (0+ space)
      ?\"
      "text/css"
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[")
      ))


(defun mumamo-search-fw-exc-end-inlined-style (pos max)
  "Helper for `mumamo-chunk-inlined-style'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</style>")))

(defun mumamo-search-fw-exc-start-inlined-style (pos max)
  "Helper for `mumamo-chunk-inlined-style'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<style" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 6))
      (when (looking-at mumamo-style-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'css-mode nil)
        ))))

(defun mumamo-chunk-inlined-style (pos max)
  "Find <style>...</style>.  Return range and 'css-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-style
                                 'mumamo-search-fw-exc-end-inlined-style))

;;;; <script ...>

(defconst mumamo-script-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "type"
      (0+ space)
      "="
      (0+ space)
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      (or "'text/javascript'" "\"text/javascript\"")
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-fw-exc-end-inlined-script (pos max)
  "Helper for `mumamo-chunk-inlined-script'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</script>")))

(defun mumamo-search-fw-exc-start-inlined-script (pos max)
  "Helper for `mumamo-chunk-inlined-script'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<script" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-script-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode '(nxml-mode))
        ))))

(defun mumamo-chunk-inlined-script (pos max)
  "Find <script>...</script>.  Return range and 'javascript-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-script
                                 'mumamo-search-fw-exc-end-inlined-script))

;;;; on[a-z]+=\"javascript:"

(defconst mumamo-onjs=-attr=
  (rx
   ;;"on[a-z]+="
   (or "onclick" "ondblclick" "onmousedown" "onmousemove" "onmouseout" "onmouseover" "onmouseup" "onkeydown" "onkeypress" "onkeyup")
   "="))

(defconst mumamo-onjs=-attr-regex
  (rx point
      (or "<" "?>")
      (* (not (any ">")))
      space
      (submatch
       ;;"on" (1+ (any "a-za-z"))
       (or "onclick" "ondblclick" "onmousedown" "onmousemove" "onmouseout" "onmouseover" "onmouseup" "onkeydown" "onkeypress" "onkeyup")
       "=")
      (0+ space)
      ?\"
      (submatch
       (opt "javascript:")
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-onjs=(pos max)
  "Find javascript on...=\"...\".  Return range and 'javascript-mode."
  (mumamo-chunk-attr= pos max mumamo-onjs=-attr= t mumamo-onjs=-attr-regex
                      'javascript-mode))

;;;; py:somthing=\"python\"

(defconst mumamo-py:=-attr= "py:[a-z]+=")

(defconst mumamo-py:=-attr-regex
  (rx point
      (or "<" "?>")
      (* (not (any ">")))
      space
      (submatch
       "py:" (1+ (any "a-za-z"))
       "=")
      (0+ space)
      ?\"
      (submatch
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-py:=(pos max)
  "Find python py:...=\"...\".  Return range and 'python-mode."
  (mumamo-chunk-attr= pos max mumamo-py:=-attr= t mumamo-py:=-attr-regex
                      'python-mode))

(defun mumamo-chunk-py:match (pos max)
  (save-match-data
    (let ((here (point))
          (py:match (progn
                      (goto-char pos)
                      (re-search-forward (rx "py:match"
                                             (1+ space)
                                             (0+ (not (any ">")))
                                             word-start
                                             (submatch "path=")
                                             (0+ space)
                                             ?\"
                                             (submatch
                                              (0+
                                               (not (any "\"")))))
                                         max t)))
          start end borders
          )
      (when py:match
        (setq start (match-beginning 1))
        (setq end   (match-end 2))
        (setq borders (list (match-end 1) (1- end)))
        )
      (goto-char here)
      (when start
        (list start
              end
              'python-mode
              borders
              nil ;; parseable-by
              'mumamo-chunk-attr=-new-fw-exc-fun ;; fw-exc-fun
              'mumamo-chunk-attr=-new-find-borders-fun ;; find-borders-fun
            )))))

;;;; style=

(defconst mumamo-style=start-regex
  (rx "<"
      (0+ (not (any ">")))
      space
      (submatch "style=")
      (0+ space)
      ?\"
      (submatch
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-style=(pos max)
  "Find style=\"...\".  Return range and 'css-mode."
  (mumamo-chunk-attr= pos max "style=" nil mumamo-style=start-regex
                      'css-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HTML w html-mode

(put 'mumamo-alt-php-tags-mode 'permanent-local t)
(define-minor-mode mumamo-alt-php-tags-mode
  "Minor mode for using '(?php' instead of '<?php' in buffer.
When turning on this mode <?php is replace with (?php in the buffer.
If you write the buffer to file (?php is however written as <?php.

When turning off this mode (?php is replace with <?php in the buffer.

The purpose of this minor mode is to work around problems with
using the `nxml-mode' parser in php files.  `nxml-mode' knows
damned well that you can not have the character < in strings and
I can't make it forget that.  For PHP programmers it is however
very convient to use <?php ... ?> in strings.

There is no reason to use this minor mode unless you want XML
validation and/or completion in your php file.  If you do not
want that then you can simply use a multi major mode based on
`html-mode' instead of `nxml-mode'/`nxhtml-mode'.  Or, of course,
just `php-mode' if there is no html code in the file."
  :lighter "<?php "
  (if mumamo-alt-php-tags-mode
      (progn
        ;;(unless mumamo-multi-major-mode (error "Only for mumamo multi major modes"))
        (unless (let ((major-mode (mumamo-main-major-mode)))
                  (derived-mode-p 'nxml-mode))
          ;;(error "Mumamo multi major mode must be based on nxml-mode")
          )
        (unless (memq 'mumamo-chunk-alt-php (caddr mumamo-current-chunk-family))
          (error "Mumamo multi major must have chunk function mumamo-chunk-alt-php"))

        ;; Be paranoid about the file/content write hooks
        (when (<= emacs-major-version 22)
          (with-no-warnings
            (when local-write-file-hooks ;; obsolete, but check!
              (error "Will not do this because local-write-file-hooks is non-nil"))))
        (remove-hook 'write-contents-functions 'mumamo-alt-php-write-contents t)
        (when write-contents-functions
          (error "Will not do this because write-contents-functions is non-nil"))
        (when (delq 'recentf-track-opened-file (copy-sequence write-file-functions))
          (error "Will not do this because write-file-functions is non-nil"))

        (add-hook 'write-contents-functions 'mumamo-alt-php-write-contents t t)
        (put 'write-contents-functions 'permanent-local t)
        (save-restriction
          (let ((here (point)))
            (widen)
            (goto-char (point-min))
            (while (search-forward "<?php" nil t)
              (replace-match "(?php"))
            (goto-char (point-min))
            (while (search-forward "<?=" nil t)
              (replace-match "(?="))
            (goto-char (point-min))
            (while (search-forward "?>" nil t)
                (replace-match "?)"))
            (goto-char here))))
    (save-restriction
      (let ((here (point)))
        (widen)
        (goto-char (point-min))
        (while (search-forward "(?php" nil t)
          (replace-match "<?php"))
        (goto-char (point-min))
        (while (search-forward "(?=" nil t)
          (replace-match "<?="))
        (goto-char (point-min))
        (while (search-forward "?)" nil t)
          (replace-match "?>"))
        (goto-char here)))
    (remove-hook 'write-contents-functions 'mumamo-alt-php-write-contents t)))

(defun mumamo-chunk-alt-php (pos max)
  "Find (?php ... ?), return range and `php-mode'.
Workaround for the problem that I can not tame `nxml-mode' to recognize <?php.

See `mumamo-possible-chunk-forward' for POS and MAX."
  (when mumamo-alt-php-tags-mode
    (mumamo-quick-chunk-forward pos max "(?php" "?)" 'borders 'php-mode)))

(defun mumamo-chunk-alt-php= (pos max)
  "Find (?= ... ?), return range and `php-mode'.
Workaround for the problem that I can not tame `nxml-mode' to recognize <?php.

See `mumamo-possible-chunk-forward' for POS and MAX."
  (when mumamo-alt-php-tags-mode
    (mumamo-quick-chunk-forward pos max "(?=" "?)" 'borders 'php-mode)))


(defun mumamo-alt-php-write-contents ()
  "For `write-contents-functions' when `mumamo-chunk-alt-php' is used."
  (let ((here (point)))
    (save-match-data
      (save-restriction
        (widen)
        (condition-case nil
            (atomic-change-group
              (progn
                (goto-char (point-min))
                (while (search-forward "(?php" nil t)
                  (replace-match "<?php"))
                (goto-char (point-min))
                (while (search-forward "(?=" nil t)
                  (replace-match "<?="))
                (goto-char (point-min))
                (while (search-forward "?)" nil t)
                  (replace-match "?>"))
                (basic-save-buffer-1)
                (signal 'mumamo-error-ind-0 nil)))
          (mumamo-error-ind-0)))
      (set-buffer-modified-p nil))
    (goto-char here))
  ;; saved, return t
  t)

;; Fix-me: does not work with new chunk div
(defun mumamo-whole-line-chunk-fw-exc-end-fun (pos max)
  (let ((here (point)))
    (goto-char pos)
    (prog1
        (line-end-position)
      (goto-char here))))

(defun mumamo-whole-line-chunk (pos max marker mode)
  (let* ((here (point))
         (len-marker (length marker))
         (pattern (rx-to-string `(and bol (0+ blank) ,marker blank) t))
         (whole-line-chunk-borders-fun
          `(lambda (start end dummy)
             (let ((start-border (+ start ,len-marker)))
               (list start-border nil))))
         beg
         end
         ret)
    (goto-char pos)
    (setq beg (re-search-forward pattern max t))
    (when beg
      (setq beg (- beg len-marker 1))
      (setq end (line-end-position))
      (setq ret (list beg
                      end
                      mode
                      (let ((start-border (+ beg len-marker)))
                        (list start-border nil))
                      nil
                      'mumamo-whole-line-chunk-fw-exc-end-fun
                      whole-line-chunk-borders-fun
                      )))
    (goto-char here)
    ret))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mason (not quite ready)
;; http://www.masonhq.com/docs/manual/Devel.html#examples_and_recommended_usage

(defun mumamo-chunk-mason-perl-line (pos max)
  (mumamo-whole-line-chunk pos max "%" 'perl-mode))

(defun mumamo-chunk-mason-perl-single (pos max)
  (mumamo-quick-chunk-forward pos max "<% " " %>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-perl-block (pos max)
  (mumamo-quick-chunk-forward pos max "<%perl>" "</%perl>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-perl-init (pos max)
  (mumamo-quick-chunk-forward pos max "<%init>" "</%init>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-perl-once (pos max)
  (mumamo-quick-chunk-forward pos max "<%once>" "</%once>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-perl-cleanup (pos max)
  (mumamo-quick-chunk-forward pos max "<%cleanup>" "</%cleanup>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-perl-shared (pos max)
  (mumamo-quick-chunk-forward pos max "<%shared>" "</%shared>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-simple-comp (pos max)
  (mumamo-quick-chunk-forward pos max "<& " " &>" 'borders 'text-mode))

(defun mumamo-chunk-mason-args (pos max)
  ;; Fix-me: perl-mode is maybe not the best here?
  (mumamo-quick-chunk-forward pos max "<%args>" "</%args>" 'borders 'perl-mode))

(defun mumamo-chunk-mason-doc (pos max)
  (mumamo-quick-chunk-forward pos max "<%doc>" "</%doc>" 'borders 'mumamo-comment-mode))

(defun mumamo-chunk-mason-text (pos max)
  (mumamo-quick-chunk-forward pos max "<%text>" "</%text>" 'borders 'text-mode))

;; component calls with content

(defun mumamo-chunk-mason-compcont-fw-exc-end-fun (pos max)
  (mumamo-chunk-end-fw-str-inc pos max "</&>"))

(defun mumamo-chunk-mason-compcont-find-borders-fun (start end dummy)
  (when dummy
    (list
     (when start
       (save-match-data
         (let ((here (point))
               ret)
           (goto-char start)
           (when (re-search-forward "[^>]* &>" end t)
             (setq ret (point))
             (goto-char here)
             ret))
         ))
     (when end (- end 4))
     dummy)))

(defun mumamo-chunk-mason-compcont-fw-exc-start-fun (pos max)
  (let ((where (mumamo-chunk-start-fw-str-inc pos max "<&| ")))
    (when where
      (list where 'html-mode nil))))

(defun mumamo-chunk-mason-compcont (pos max)
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-chunk-mason-compcont-fw-exc-start-fun
                                 'mumamo-chunk-mason-compcont-fw-exc-end-fun
                                 'mumamo-chunk-mason-compcont-find-borders-fun))


(provide 'mumamo-chunks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-chunks.el ends here
