;;; mumamo-fun.el --- Multi major mode functions
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-03-09T01:35:21+0100 Sun
;; Version: 0.51
;; Last-Updated: 2008-08-04T17:54:29+0200 Mon
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `cl', `flyspell', `ispell', `mumamo',
;;   `sgml-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Defines some "multi major modes" functions.  See mumamo.el for more
;; information.
;;
;;;; Usage:
;;
;;  See mumamo.el for how to use the multi major mode functions
;;  defined here.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (add-to-list 'load-path default-directory))
(eval-when-compile (require 'mumamo))
(eval-when-compile (require 'sgml-mode))
(eval-and-compile (require 'mumamo-chunks))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PHP, HTML etc.

;;;###autoload
(define-mumamo-multi-major-mode html-mumamo-mode
  "Turn on multiple major modes for (X)HTML with main mode `html-mode'.
This covers inlined style and javascript and PHP."
  ("HTML Family" html-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-alt-php
    mumamo-chunk-alt-php=
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'html-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)
(mumamo-inherit-sub-chunk-family 'html-mumamo-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XHTML w nxml-mode

;;;###autoload
(define-mumamo-multi-major-mode nxml-mumamo-mode
  "Turn on multiple major modes for (X)HTML with main mode `nxml-mode'.
This covers inlined style and javascript and PHP.

See also `mumamo-alt-php-tags-mode'."
  ("nXml Family" nxml-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-alt-php
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'nxml-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mason (not quite ready)
;; http://www.masonhq.com/docs/manual/Devel.html#examples_and_recommended_usage

;;;###autoload
(define-mumamo-multi-major-mode mason-html-mumamo-mode
  "Turn on multiple major modes for Mason using main mode `html-mode'.
This covers inlined style and javascript."
  ("Mason html Family" html-mode
   (
    mumamo-chunk-mason-perl-line
    mumamo-chunk-mason-perl-single
    mumamo-chunk-mason-perl-block
    mumamo-chunk-mason-perl-init
    mumamo-chunk-mason-perl-once
    mumamo-chunk-mason-perl-cleanup
    mumamo-chunk-mason-perl-shared
    mumamo-chunk-mason-simple-comp
    mumamo-chunk-mason-compcont
    mumamo-chunk-mason-args
    mumamo-chunk-mason-doc
    mumamo-chunk-mason-text
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'mason-html-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)
(mumamo-inherit-sub-chunk-family-locally 'mason-html-mumamo-mode 'mason-html-mumamo-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Embperl

(defun mumamo-chunk-embperl-<- (pos max)
  "Find [- ... -], return range and `perl-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "[-" "-]" 'borders 'perl-mode))

(defun mumamo-chunk-embperl-<+ (pos max)
  "Find [+ ... +], return range and `perl-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "[+" "+]" 'borders 'perl-mode))

(defun mumamo-chunk-embperl-<! (pos max)
  "Find [! ... !], return range and `perl-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "[!" "!]" 'borders 'perl-mode))

(defun mumamo-chunk-embperl-<$ (pos max)
  "Find [$ ... $], return range and `perl-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  ;; This is a bit tricky since [$var] etc must be avoided.
  (let* ((begin-mark "[$")
         (end-mark "$]")
         (good-chars '(32 ;space
                       10 ;line feed
                       9  ;tab
                       ))
         (search-fw-exc-start (lambda (pos max)
                                (let ((not-found t)
                                      (next-char nil)
                                      (exc-start (mumamo-chunk-start-fw-str
                                                  pos max begin-mark))
                                      (here (point)))
                                  (while (and not-found
                                              exc-start)
                                    (setq next-char (char-after))
                                    (if (memq next-char good-chars)
                                        (setq not-found nil)
                                      (setq exc-start
                                            (search-forward begin-mark
                                                            max t))))
                                  (when exc-start (list exc-start 'perl-mode)))))
         (search-fw-exc-end (lambda (pos max)
                              (save-match-data
                                (mumamo-chunk-end-fw-str pos max end-mark)))))
    (mumamo-possible-chunk-forward pos max
                                   search-fw-exc-start
                                   search-fw-exc-end)))

;;;###autoload
(define-mumamo-multi-major-mode embperl-html-mumamo-mode
  "Turn on multiple major modes for Embperl files with main mode `html-mode'.
This also covers inlined style and javascript."
    ("Embperl HTML Family" html-mode
     (mumamo-chunk-embperl-<-
      mumamo-chunk-embperl-<+
      mumamo-chunk-embperl-<!
      mumamo-chunk-embperl-<$
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; django

(defun mumamo-chunk-django4(pos max)
  "Find {% comment %}.  Return range and `django-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{% comment %}" "{% endcomment %}" 'borders 'mumamo-comment-mode))

(defun mumamo-chunk-django3(pos max)
  "Find {# ... #}.  Return range and `django-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{#" "#}" 'borders 'mumamo-comment-mode))

(defun mumamo-chunk-django2(pos max)
  "Find {{ ... }}.  Return range and `django-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{{" "}}" 'borders 'django-variable-mode))

(defun mumamo-chunk-django (pos max)
  "Find {% ... %}.  Return range and `django-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk (mumamo-quick-chunk-forward pos max "{%" "%}" 'borders 'django-mode)))
    (when chunk
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-search-fw-exc-start-django (pos max)
  "Helper for `mumamo-chunk-django'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{%"))

(defun mumamo-search-fw-exc-start-django2(pos max)
  "Helper for `mumamo-chunk-django2'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{{"))

(defun mumamo-search-fw-exc-start-django3(pos max)
  "Helper for `mumamo-chunk-django3'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{#"))

(defun mumamo-search-fw-exc-start-django4(pos max)
  "Helper for `mumamo-chunk-django4'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str-inc pos max "{% comment %}"))

(defun mumamo-search-fw-exc-end-django (pos max)
  "Helper for `mumamo-chunk-django'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "%}"))

(defun mumamo-search-fw-exc-end-django2(pos max)
  "Helper for `mumamo-chunk-django2'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "}}"))

(defun mumamo-search-fw-exc-end-django3(pos max)
  "Helper for `mumamo-chunk-django3'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "#}"))

(defun mumamo-search-fw-exc-end-django4(pos max)
  "Helper for `mumamo-chunk-django4'.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-end-fw-str-inc pos max "{% endcomment %}"))

;;;###autoload
(define-mumamo-multi-major-mode django-html-mumamo-mode
  "Turn on multiple major modes for Django with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Django HTML Family" html-mode
   (mumamo-chunk-django4
    mumamo-chunk-django
    mumamo-chunk-django2
    mumamo-chunk-django3
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Genshi / kid

;; {% python ... %}
(defun mumamo-chunk-genshi%(pos max)
  "Find {% python ... %}.  Return range and `genshi-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{% python" "%}" 'borders 'python-mode))

;; ${expr}
(defun mumamo-chunk-genshi$(pos max)
  "Find ${ ... }, return range and `python-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk
         (mumamo-quick-chunk-forward pos max "${" "}" 'borders 'python-mode)))
    (when chunk
      ;; Test for clash with %}
      (let ((sub-mode (nth 2 chunk))
            (start (nth 0 chunk)))
        (if sub-mode
            chunk
          ;;(message "point.1=%s" (point))
          (when (and start
                     (eq ?% (char-before start)))
            ;;(message "point.2=%s" (point))
            ;;(message "clash with %%}, chunk=%s" chunk)
            ;;(setq chunk nil)
            (setcar chunk (1- start))
            )
          ;;(message "chunk.return=%s" chunk)
          chunk)))))

;; Fix-me: Because of the way chunks currently are searched for there
;; is an error when a python chunk is used. This is because mumamo
;; gets confused by the %} ending and the } ending.  This can be
;; solved by running a separate phase to get the chunks first and
;; during that phase match start and end of the chunk.


;; Note: You will currently get fontification errors if you use
;; python chunks

;;   {% python ... %}

;; The reason is that the chunk routines currently do not know when
;; to just look for the } or %} endings.  However this should not
;; affect your editing normally.

;;;###autoload
(define-mumamo-multi-major-mode genshi-html-mumamo-mode
  "Turn on multiple major modes for Genshi with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Genshi HTML Family" html-mode
   (
    ;;mumamo-chunk-genshi%
    mumamo-chunk-genshi$
    mumamo-chunk-py:=
    mumamo-chunk-py:match
    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MJT

;; ${expr}
(defun mumamo-chunk-mjt$(pos max)
  "Find ${ ... }, return range and `javascript-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "${" "}" 'borders 'javascript-mode))

;;;###autoload
(define-mumamo-multi-major-mode mjt-html-mumamo-mode
  "Turn on multiple major modes for MJT with main mode `html-mode'.
This also covers inlined style and javascript."
  ("MJT HTML Family" html-mode
   (
    mumamo-chunk-mjt$
    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smarty

(defun mumamo-chunk-smarty-literal (pos max)
  "Find {literal} ... {/literal}.  Return range and 'html-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{literal}" "{/literal}" 'borders 'html-mode))

(defun mumamo-chunk-smarty-t (pos max)
  "Find {t} ... {/t}.  Return range and 'html-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{t}" "{/t}" 'borders 'text-mode))

(defun mumamo-chunk-smarty-comment (pos max)
  "Find {* ... *}.  Return range and 'mumamo-comment-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{*" "*}" t 'mumamo-comment-mode))

(defun mumamo-chunk-smarty (pos max)
  "Find { ... }.  Return range and 'smarty-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "{" "}" t 'smarty-mode))

;;;###autoload
(define-mumamo-multi-major-mode smarty-html-mumamo-mode
  "Turn on multiple major modes for Smarty with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Smarty HTML Family" html-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    ;;mumamo-chunk-inlined-style
    ;;mumamo-chunk-inlined-script
    mumamo-chunk-smarty-literal
    mumamo-chunk-smarty-t
    mumamo-chunk-smarty-comment
    mumamo-chunk-smarty
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ssjs - server side javascript

;; http://www.sitepoint.com/blogs/2009/03/10/server-side-javascript-will-be-as-common-as-php/
;;
;; It looks like there are different syntaxes, both
;;
;;  <script runat="server">...</script> and <% ... %>.

(defun mumamo-chunk-ssjs-% (pos max)
  "Find <% ... %>.  Return range and 'javascript-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "<%" "%>" 'borders 'javascript-mode))

(defconst mumamo-ssjs-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "runat"
      (0+ space)
      "="
      (0+ space)
      ?\"
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      (or "server" "both" "server-proxy")
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))


(defun mumamo-search-fw-exc-start-inlined-ssjs (pos max)
  "Helper for `mumamo-chunk-inlined-ssjs'.
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
      (when (looking-at mumamo-ssjs-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode)
        ))))

(defun mumamo-chunk-inlined-ssjs (pos max)
  "Find <script runat=...>...</script>.  Return range and 'javascript-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-ssjs
                                 'mumamo-search-fw-exc-end-inlined-script))

;;;###autoload
(define-mumamo-multi-major-mode ssjs-html-mumamo-mode
  "Turn on multiple major modes for SSJS with main mode `html-mode'.
This covers inlined style and javascript."
  ("HTML Family" html-mode
   (mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-inlined-ssjs
    mumamo-chunk-ssjs-%
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'html-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)
(mumamo-inherit-sub-chunk-family 'ssjs-html-mumamo-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gsp

(defun mumamo-chunk-gsp (pos max)
  "Find <% ... %>.  Return range and 'groovy-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "<%" "%>" 'borders 'groovy-mode))

;;;###autoload
(define-mumamo-multi-major-mode gsp-html-mumamo-mode
  "Turn on multiple major modes for GSP with main mode `html-mode'.
This also covers inlined style and javascript."
    ("GSP HTML Family" html-mode
     (mumamo-chunk-gsp
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jsp - Java Server Pages

(defun mumamo-chunk-jsp (pos max)
  "Find <% ... %>.  Return range and 'java-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "<%" "%>" 'borders 'java-mode))

;;;###autoload
(define-mumamo-multi-major-mode jsp-html-mumamo-mode
  "Turn on multiple major modes for JSP with main mode `html-mode'.
This also covers inlined style and javascript."
    ("JSP HTML Family" html-mode
     (mumamo-chunk-jsp
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; eruby

;; Fix-me: Maybe take care of <%= and <%- and -%>, but first ask the
;; ruby people if this is worth doing.
;;
;; See also http://wiki.rubyonrails.org/rails/pages/UnderstandingViews
(defun mumamo-chunk-eruby (pos max)
  "Find <% ... %>.  Return range and 'ruby-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk (mumamo-quick-chunk-forward pos max "<%" '("-?%>" . t) 'borders 'ruby-mode)))
    (when chunk
      ;; Put indentation type on 'mumamo-next-indent on the chunk:
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-eruby= (pos max)
  "Find <% ... %>.  Return range and 'ruby-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk (mumamo-quick-chunk-forward pos max "<%=" '("-?%>" . t) 'borders 'ruby-mode)))
    (when chunk
      ;; Put indentation type on 'mumamo-next-indent on the chunk:
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-eruby=quoted (pos max)
  "Find \"<%= ... %>\".  Return range and 'ruby-mode.
See `mumamo-possible-chunk-forward' for POS and MAX.

This is a workaround for problems with strings."
  (let ((chunk (mumamo-quick-chunk-forward pos max "\"<%=" '("-?%>\"" . t) 'borders 'ruby-mode)))
    (when chunk
      ;; Put indentation type on 'mumamo-next-indent on the chunk:
      ;; Fix-me: use this!
      ;;(setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-eruby-comment (pos max)
  "Find <%# ... %>.  Return range and 'ruby-mode.
See `mumamo-possible-chunk-forward' for POS and MAX.

This is needed since otherwise the end marker is thought to be
part of a comment."
  (mumamo-quick-chunk-forward pos max "<%#" "%>" 'borders 'mumamo-comment-mode))

;; ;;;###autoload
;; (define-mumamo-multi-major-mode eruby-mumamo-mode
;;   "Turn on multiple major mode for eRuby with unspecified main mode.
;; Current major-mode will be used as the main major mode."
;;   ("eRuby Family" nil
;;    (mumamo-chunk-eruby-comment
;;     mumamo-chunk-eruby=
;;     mumamo-chunk-eruby
;;     )))

;;;###autoload
(define-mumamo-multi-major-mode eruby-html-mumamo-mode
  "Turn on multiple major modes for eRuby with main mode `html-mode'.
This also covers inlined style and javascript."
  ("eRuby Html Family" html-mode
   (
    mumamo-chunk-eruby-comment
    mumamo-chunk-eruby=
    mumamo-chunk-eruby
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode eruby-javascript-mumamo-mode
  "Turn on multiple major modes for eRuby with main mode `javascript-mode'."
  ("eRuby Html Family" javascript-mode
   (
    mumamo-chunk-eruby-comment
    mumamo-chunk-eruby=quoted
    mumamo-chunk-eruby
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; heredoc

(defcustom mumamo-heredoc-modes
  '(
    ("HTML" html-mode)
    ("CSS" css-mode)
    ("JAVASCRIPT" javascript-mode)
    ("JAVA" java-mode)
    ("GROOVY" groovy-mode)
    ("SQL" sql-mode)
    )
  "Matches for heredoc modes.
The entries in this list have the form

  (REGEXP MAJOR-MODE-SPEC)

where REGEXP is a regular expression that should match the
heredoc marker line and MAJOR-MODE-SPEC is the major mode spec to
use in the heredoc part.

The major mode spec is translated to a major mode using
`mumamo-major-mode-from-modespec'."
  :type '(repeat
          (list
           regexp
           (function :tag "Major mode")))
  :group 'mumamo-modes)

(defun mumamo-mode-for-heredoc (marker)
  "Return major mode associated with MARKER.
Use first match in `mumamo-heredoc-modes'.
If no match use `text-mode'."
  (let ((mode (catch 'mode
                (save-match-data
                  (dolist (rec mumamo-heredoc-modes)
                    (let ((regexp (nth 0 rec))
                          (mode   (nth 1 rec)))
                      (when (string-match regexp marker)
                        (throw 'mode mode))))))))
    (if mode
        (mumamo-major-mode-from-modespec mode)
      'text-mode)))

(defun mumamo-chunk-heredoc (pos max lang)
  "This should work similar to `mumamo-possible-chunk-forward'.
POS and MAX have the same meaning as there.

LANG is the programming language.
Supported values are 'perl."
  ;; Fix-me: LANG
  ;; Fix-me: use mumamo-end-in-code
  (mumamo-condition-case err
      (let ((old-point (point)))
        (goto-char pos)
        (beginning-of-line)
        (let (next-<<
              (want-<< t)
              heredoc-mark
              end-mark-len
              heredoc-line
              delimiter
              skipped
              (skip-b "")
              start-inner
              end
              exc-mode
              fw-exc-fun
              border-fun
              allow-code-after
              start-outer
              ps
              )
          (goto-char pos)
          (beginning-of-line)
          (case lang
            ('sh
             (setq allow-code-after t)
             (while want-<<
               (setq next-<< (search-forward "<<" max t))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (point)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 2))
               (when (= (char-after) ?-)
                 (setq skip-b "\t*")
                 (unless (eolp) (forward-char)))
               ;; fix-me: space
               (setq skipped (skip-chars-forward " \t"))
               (when (memq (char-after) '(?\" ?\'))
                 (setq delimiter (list (char-after))))
               (if (and (> skipped 0) (not delimiter))
                   (setq heredoc-mark "")
                 (when (looking-at (rx-to-string
                                    `(and (regexp ,(if delimiter
                                                       (concat delimiter "\\([^\n<>;]+\\)" delimiter)
                                                     "\\([^ \t\n<>;]+\\)"))
                                          (or blank line-end))))
                   (setq heredoc-mark  (buffer-substring-no-properties
                                        (match-beginning 1)
                                        (match-end 1)))))
               (when heredoc-mark
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 (setq start-inner (1+ (point-at-eol)))
                 (setq end-mark-len (length heredoc-mark))
                 )))
            ('w32-ps (error "No support for windows power shell yet"))
            ('php
             (while want-<<
               (setq next-<< (search-forward "<<<" max t))
               ;; Check inside string or comment.
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 (setq ps (parse-partial-sexp (line-beginning-position) (- (point) 0)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 3))
               (skip-chars-forward " \t")
               (when (looking-at (concat "\\([^\n;]*\\)[[:blank:]]*\n"))
                 (setq heredoc-mark  (buffer-substring-no-properties
                                      (match-beginning 1)
                                      (match-end 1)))
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 ;; fix-me: nowdoc
                 (when (and (= ?\' (string-to-char heredoc-mark))
                            (= ?\' (string-to-char (substring heredoc-mark (1- (length heredoc-mark))))))
                   (setq heredoc-mark (substring heredoc-mark 1 (- (length heredoc-mark) 1))))
                 (setq end-mark-len (1+ (length heredoc-mark)))
                 (setq start-inner (match-end 0)))))
            ('perl
             (setq allow-code-after t)
             (while want-<<
               (setq next-<< (search-forward "<<" max t))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (point)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 2))
               ;; fix-me: space
               (setq skipped (skip-chars-forward " \t"))
               (when (memq (char-after) '(?\" ?\'))
                 (setq delimiter (list (char-after))))
               (if (and (> skipped 0) (not delimiter))
                   (setq heredoc-mark "") ;; blank line
                 (when (looking-at (rx-to-string
                                    `(and (regexp ,(if delimiter
                                                       (concat delimiter "\\([^\n;]*\\)" delimiter)
                                                     "\\([^ \t\n<>;]+\\)"))
                                          (or blank ";"))))
                   (setq heredoc-mark  (buffer-substring-no-properties
                                        (match-beginning 1)
                                        (match-end 1)))))
               (when heredoc-mark
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 ;;(setq start-inner (1+ (match-end 0)))
                 (setq start-inner (1+ (point-at-eol)))
                 (setq end-mark-len (length heredoc-mark))
                 )))
            ('python
             (unless (eobp) (forward-char))
             (while want-<<
               (setq next-<< (re-search-forward "\"\"\"\\|'''" max t))
               (setq start-outer (- (point) 3))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (- (point) 3)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil)))))
            ('ruby
             (while want-<<
               (setq next-<< (search-forward "<<" max t))
               (if (not next-<<)
                   (setq want-<< nil) ;; give up
                 ;; Check inside string or comment.
                 (setq ps (parse-partial-sexp (line-beginning-position) (point)))
                 (unless (or (nth 3 ps) (nth 4 ps))
                   (setq want-<< nil))))
             (when next-<<
               (setq start-outer (- (point) 2))
               (when (= (char-after) ?-)
                 (setq skip-b "[ \t]*")
                 (forward-char))
               (when (looking-at (concat "[^\n[:blank:]]*"))
                 (setq heredoc-mark  (buffer-substring-no-properties
                                      (match-beginning 0)
                                      (match-end 0)))
                 (setq end-mark-len (length heredoc-mark))
                 (setq heredoc-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 (setq start-inner (match-end 0)))))
            (t (error "next-<< not implemented for lang %s" lang)))
          (when start-inner (assert (<= pos start-inner) t))
          (goto-char old-point)
          (when (or start-inner end)
            (let ((endmark-regexp
                   (case lang
                     ('sh (concat "^" skip-b heredoc-mark "$"))
                     ('php (concat "^" heredoc-mark ";?$"))
                     ('perl (concat "^" heredoc-mark "$"))
                     ('python (concat "^" heredoc-mark "[[:space:]]*"))
                     ('ruby (concat "^" skip-b heredoc-mark "$"))
                     (t (error "mark-regexp not implemented for %s" lang)))))
              (setq border-fun `(lambda (start end exc-mode)
                                  (list
                                   (if ,allow-code-after nil (+ start (- ,start-inner ,start-outer 1)))
                                   (when end (- (1- end) ,end-mark-len)))))
              (setq fw-exc-fun `(lambda (pos max)
                                  (save-match-data
                                    (let ((here (point)))
                                      (goto-char pos)
                                      (prog1
                                          (when (re-search-forward ,endmark-regexp max t)
                                            ;;(- (point) 1 ,(length heredoc-mark))
                                            ;; Extend heredoc chunk
                                            ;; until after newline to
                                            ;; avoid the "syntax-table
                                            ;; (15)" entry on the
                                            ;; newline char in
                                            ;; `sh-mode':
                                            (+ (point) 1)
                                            )
                                        (goto-char here)))))))
            (setq exc-mode (mumamo-mode-for-heredoc heredoc-line))
            (list start-inner end exc-mode nil nil fw-exc-fun nil)
            ;; Fix me: Add overriding for inner chunks (see
            ;; http://www.emacswiki.org/emacs/NxhtmlMode#toc13). Maybe
            ;; make fw-exc-fun a list (or a cons, since overriding is
            ;; probably all that I want to add)? And make the
            ;; corresponding chunk property a list too?
            ;;(list start-outer end exc-mode (list start-inner end) nil fw-exc-fun border-fun 'heredoc)
            (list (if allow-code-after start-inner start-outer)
                  end exc-mode (list start-inner end) nil fw-exc-fun border-fun 'heredoc)
            )))
    (error (mumamo-display-error 'mumamo-chunk-heredoc
                                 "%s" (error-message-string err)))))


;;;; Unix style sh heredoc

(defun mumamo-chunk-sh-heredoc (pos max)
  "Find sh here docs.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((r (mumamo-chunk-heredoc pos max 'sh)))
    r))

;;;###autoload
(define-mumamo-multi-major-mode sh-heredoc-mumamo-mode
  "Turn on multiple major modes for sh heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("SH HereDoc" sh-mode
   (mumamo-chunk-sh-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'sh-heredoc-mumamo-mode)


;;;; PHP heredoc

(defun mumamo-chunk-php-heredoc (pos max)
  "Find PHP here docs.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((r (mumamo-chunk-heredoc pos max 'php)))
    r))

;;;###autoload
(define-mumamo-multi-major-mode php-heredoc-mumamo-mode
  "Turn on multiple major modes for PHP heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("PHP HereDoc" php-mode
   (mumamo-chunk-php-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'php-heredoc-mumamo-mode)
(mumamo-inherit-sub-chunk-family-locally 'php-heredoc-mumamo-mode 'html-mumamo-mode)


;;;; Perl heredoc

(defun mumamo-chunk-perl-heredoc (pos max)
  "Find perl here docs.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((r (mumamo-chunk-heredoc pos max 'perl)))
    r))

;;;###autoload
(define-mumamo-multi-major-mode perl-heredoc-mumamo-mode
  "Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Perl HereDoc" perl-mode
   (mumamo-chunk-perl-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'perl-heredoc-mumamo-mode)

;;;###autoload
(define-mumamo-multi-major-mode cperl-heredoc-mumamo-mode
  "Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Perl HereDoc" cperl-mode
   (mumamo-chunk-perl-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'cperl-heredoc-mumamo-mode)


;;;; Python heredoc

(defun mumamo-chunk-python-heredoc (pos max)
  "Find python here docs.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((r (mumamo-chunk-heredoc pos max 'python)))
    r))

;;;###autoload
(define-mumamo-multi-major-mode python-heredoc-mumamo-mode
  "Turn on multiple major modes for Perl heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Python HereDoc" python-mode
   (mumamo-chunk-python-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'python-heredoc-mumamo-mode)


;;;; Ruby heredoc

(defun mumamo-chunk-ruby-heredoc (pos max)
  "Find Ruby here docs.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((r (mumamo-chunk-heredoc pos max 'ruby)))
    r))

;;;###autoload
(define-mumamo-multi-major-mode ruby-heredoc-mumamo-mode
  "Turn on multiple major modes for Ruby heredoc document.
See `mumamo-heredoc-modes' for how to specify heredoc major modes."
  ("Ruby HereDoc" ruby-mode
   (mumamo-chunk-ruby-heredoc
    )))
(mumamo-inherit-sub-chunk-family 'ruby-heredoc-mumamo-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tex meta

(defconst mumamo-textext-end-regex
  (rx "textext("
      (0+
       (0+ (not (any "\"()")))
       ?\"
       (0+ (not (any "\"")))
       ?\"
       )
      (0+ (not (any "\"()")))
      ")"))

(defun mumamo-textext-test-is-end (pos)
  "Helper for `mumamo-chunk-textext'.
Return POS if POS is at the end of textext chunk."
  (when pos
    (let ((here (point))
          hit)
      (goto-char (+ 2 pos))
      (when (looking-back mumamo-textext-end-regex)
        (setq hit t))
      (goto-char here)
      (when hit pos))))


(defun mumamo-search-fw-textext-start (pos max)
  "Helper for `mumamo-chunk-textext'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "textext(\"")))
    (when where
      (list where 'plain-tex-mode))))

(defun mumamo-search-fw-textext-end (pos max)
  "Helper for `mumamo-chunk-textext'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (let ((end (mumamo-chunk-end-fw-str pos max "\")")))
      (mumamo-textext-test-is-end end))))

(defun mumamo-chunk-textext (pos max)
  "Find textext or TEX chunks.  Return range and 'plain-tex-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-textext-start
                                 'mumamo-search-fw-textext-end))

(defun mumamo-search-fw-verbatimtex-start (pos max)
  "Helper for `mumamo-chunk-verbatimtextext'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "\nverbatimtex")))
    (when where
      (list where 'plain-tex-mode))))

(defun mumamo-search-fw-verbatimtex-end (pos max)
  "Helper for `mumamo-chunk-verbatimtextext'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "\netex")))

(defun mumamo-chunk-verbatimtex (pos max)
  "Find verbatimtex - etex chunks.  Return range and 'plain-tex-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-verbatimtex-start
                                 'mumamo-search-fw-verbatimtex-end))

(defun mumamo-search-fw-btex-start (pos max)
  "Helper for `mumamo-chunk-btex'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "\nverbatimtex")))
    (when where
      (list where 'plain-tex-mode))))

(defun mumamo-search-fw-btex-end (pos max)
  "Helper for `mumamo-chunk-btex'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "\netex")))

(defun mumamo-chunk-btex (pos max)
  "Find btex - etex chunks.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-btex-start
                                 'mumamo-search-fw-btex-end))

;;;###autoload
(define-mumamo-multi-major-mode metapost-mumamo-mode
  "Turn on multiple major modes for MetaPost."
  ("MetaPost TeX Family" metapost-mode
   (mumamo-chunk-textext
    mumamo-chunk-verbatimtex
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OpenLaszlo

(defconst mumamo-lzx-method-tag-start-regex
  (rx "<method"
      (optional
       space
       (0+ (not (any ">"))))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-bw-exc-start-inlined-lzx-method (pos min)
  "Helper for `mumamo-chunk-inlined-lzx-method'.
POS is where to start search and MIN is where to stop."
  (goto-char (+ pos 7))
  (let ((marker-start (search-backward "<method" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-lzx-method-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'javascript-mode))
        ))))


(defun mumamo-search-fw-exc-start-inlined-lzx-method (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-method'.
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
  (let ((exc-start (search-forward "<method" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-lzx-method-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode)
        ))))

(defun mumamo-search-fw-exc-end-inlined-lzx-method (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-method'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</method>")))

(defun mumamo-chunk-inlined-lzx-method (pos max)
  "Find <method>...</method>.  Return range and 'javascript-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-lzx-method
                                 'mumamo-search-fw-exc-end-inlined-lzx-method))

(defconst mumamo-lzx-handler-tag-start-regex
  (rx "<handler"
      (optional
       space
       (0+ (not (any ">"))))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))


(defun mumamo-search-fw-exc-start-inlined-lzx-handler (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-handler'.
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
  (let ((exc-start (search-forward "<handler" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 8))
      (when (looking-at mumamo-lzx-handler-tag-start-regex)
        (goto-char (match-end 0))
        (list (point) 'javascript-mode)
        ))))

(defun mumamo-search-fw-exc-end-inlined-lzx-handler (pos max)
  "Helper for `mumamo-chunk-inlined-lzx-handler'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</handler>")))

(defun mumamo-chunk-inlined-lzx-handler (pos max)
  "Find <handler>...</handler>.  Return range and 'javascript-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-inlined-lzx-handler
                                 'mumamo-search-fw-exc-end-inlined-lzx-handler))


;;;###autoload
(define-mumamo-multi-major-mode laszlo-nxml-mumamo-mode
  "Turn on multiple major modes for OpenLaszlo."
  ("OpenLaszlo Family" nxml-mode
   (mumamo-chunk-inlined-script
    mumamo-chunk-inlined-lzx-method
    mumamo-chunk-inlined-lzx-handler
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; csound

(defun mumamo-search-fw-exc-start-csound-orc (pos max)
  "Helper for `mumamo-chunk-csound-orc'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "<csinstruments>")))
    (when where
      (list where 'csound-orc-mode))))

(defun mumamo-search-fw-exc-end-csound-orc (pos max)
  "Helper for `mumamo-chunk-csound-orc'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</csinstruments>")))

(defun mumamo-chunk-csound-orc (pos max)
  "Find <csinstruments>...</...>.  Return range and 'csound-orc-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-csound-orc
                                 'mumamo-search-fw-exc-end-csound-orc))

(defun mumamo-search-fw-exc-start-csound-sco (pos max)
  "Helper for `mumamo-chunk-csound-sco'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "<csscore>")))
    (when where
      (list where 'csound-sco-mode))))

(defun mumamo-search-fw-exc-end-csound-sco (pos max)
  "Helper for `mumamo-chunk-csound-sco'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</csscore>")))

(defun mumamo-chunk-csound-sco (pos max)
  "Found <csscore>...</csscore>.  Return range and 'csound-sco-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-csound-sco
                                 'mumamo-search-fw-exc-end-csound-sco))

;;;###autoload
(define-mumamo-multi-major-mode csound-sgml-mumamo-mode
  "Turn on mutiple major modes for CSound orc/sco Modes."
  ("CSound orc/sco Modes" sgml-mode
   (mumamo-chunk-csound-sco
    mumamo-chunk-csound-orc
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; noweb

;;;###autoload
(defgroup mumamo-noweb2 nil
  "Customization group for `noweb2-mumamo-mode'."
  :group 'mumamo-modes)

(defcustom mumamo-noweb2-mode-from-ext
  '(
    ("php" . php-mode)
    ("c" . c-mode)
    )
  "File extension regexp to major mode mapping.
Used by `noweb2-mumamo-mode'."
  :type '(repeat
          (cons regexp major-mode-function))
  :group 'mumamo-noweb2)

;; (defvar mumamo-noweb2-found-mode-from-ext nil
;;   "Major modes determined from file names.  Internal use.")

(defvar mumamo-noweb2-code-major-mode nil)
(put 'mumamo-noweb2-code-major-mode 'permanent-local t)

(defun mumamo-noweb2-set-code-major-mode (major)
  (interactive "CCode major mode: ")
  (message "major=%s" major)
  (set (make-local-variable 'mumamo-noweb2-code-major-mode) major)
  (mumamo-remove-all-chunk-overlays)
  ;; fix-me
  )

(defun mumamo-noweb2-chunk-start-fw (pos max)
  "Helper for `mumamo-noweb2-chunk'.
POS is where to start search and MAX is where to stop."
  (let* ((where (mumamo-chunk-start-fw-re pos max "^<<\\(.*?\\)>>="))
         (border-start (when where (match-beginning 0)))
         (exc-mode 'text-mode))
    (when where
      (if mumamo-noweb2-code-major-mode
          (setq exc-mode mumamo-noweb2-code-major-mode)
        (let* ((file-name (buffer-file-name)) ;(match-string-no-properties 1))
               (file-ext (when file-name (file-name-extension file-name))))
          (when file-ext
            (setq exc-mode (catch 'major
                             (dolist (rec mumamo-noweb2-mode-from-ext)
                               (when (string-match (car rec) file-ext)
                                 (throw 'major (cdr rec))))
                             nil)))))
      (list where exc-mode))))


(defun mumamo-noweb2-chunk-end-fw (pos max)
  "Helper for `mumamo-noweb2-chunk'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-re pos max "^@")))

(defun mumamo-noweb2-code-chunk (pos max)
  "Find noweb chunks.  Return range and found mode.
See `mumamo-possible-chunk-forward' for POS and MAX.
`mumamo-noweb2-set-code-major-mode'"
  (save-match-data
    (mumamo-possible-chunk-forward pos max
                                   'mumamo-noweb2-chunk-start-fw
                                   'mumamo-noweb2-chunk-end-fw)))


;;;###autoload
(define-mumamo-multi-major-mode noweb2-mumamo-mode
  "Multi major mode for noweb files."
  ("noweb Family" latex-mode
   (mumamo-noweb2-code-chunk)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Template-Toolkit



;; (setq auto-mode-alist
;;       (append '(("\\.tt2?$" . tt-mode))  auto-mode-alist ))

;;(require 'tt-mode)
(defun mumamo-chunk-tt (pos max)
  "Find [% ... %], return range and `tt-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX.

This is for Template Toolkit.
See URL `http://dave.org.uk/emacs/' for `tt-mode'."
  (mumamo-quick-chunk-forward pos max "[%" "%]" t 'tt-mode))

(define-mumamo-multi-major-mode tt-html-mumamo-mode
  "Turn on multiple major modes for TT files with main mode `nxhtml-mode'.
TT = Template-Toolkit.

This also covers inlined style and javascript."
    ("TT HTML Family" html-mode
     (mumamo-chunk-tt
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Asp

;;;; asp <%@language="javscript"%>

(defvar mumamo-asp-default-major 'asp-js-mode)
(make-variable-buffer-local 'mumamo-asp-default-major)
(put 'mumamo-asp-default-major 'permanent-local t)

(defconst mumamo-asp-lang-marker
  (rx "<%@"
      (0+ space)
      "language"
      (0+ space)
      "="
      (0+ space)
      "\""
      (submatch (1+ (not (any "\""))))
      "\""
      (0+ space)))

(defun mumamo-search-fw-exc-start-jsp (pos max)
  ;; fix-me
  )
(defun mumamo-chunk-asp (pos max)
  "Find <% ... %>.  Return range and 'asp-js-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  ;; Fix-me: this is broken!
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-exc-start-asp
                                 'mumamo-search-fw-exc-end-jsp))


;;;; asp <% ...>

(defun mumamo-chunk-asp% (pos max)
  "Find <% ... %>.  Return range and 'asp-js-mode or 'asp-vb-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let* ((chunk (mumamo-quick-chunk-forward pos max "<%" "%>" 'borders 'java-mode))
         (beg (nth 0 chunk))
         (here (point))
         glang)
    (when chunk
      (goto-char beg)
      (if (looking-at mumamo-asp-lang-marker)
          (progn
            (setq glang (downcase (match-string 1)))
            (cond
             ((string= glang "javascript")
              (setq mumamo-asp-default-major 'asp-js-mode))
             ((string= glang "vbscript")
              (setq mumamo-asp-default-major 'asp-vb-mode))
             )
            (setcar (nthcdr 2 chunk) 'mumamo-comment-mode))
        (setcar (nthcdr 2 chunk) mumamo-asp-default-major))
      chunk)))

;;;; asp <script ...>

(defconst mumamo-asp-script-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "language"
      (0+ space)
      "="
      (0+ space)
      ?\"
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      ;; "text/javascript"
      (submatch
       (or "javascript" "vbscript"))
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-asp-search-fw-exc-start-inlined-script (pos max)
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
        (exc-mode 'asp-vb-mode)
        (lang "vbscript"))
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-asp-script-tag-start-regex)
        (goto-char (match-end 0))
        (setq lang (downcase (match-string-no-properties 1)))
        (cond
         ((string= lang "javascript")
          (setq exc-mode 'asp-js-mode))
         ((string= lang "vbscript")
          (setq exc-mode 'asp-vb-mode)))
        (list (point) exc-mode)
        ))))

(defun mumamo-asp-chunk-inlined-script (pos max)
  "Find <script language=...  runat=...>...</script>.  Return 'asp-js-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-asp-search-fw-exc-start-inlined-script
                                 'mumamo-search-fw-exc-end-inlined-script))

;;;###autoload
(define-mumamo-multi-major-mode asp-html-mumamo-mode
  "Turn on multiple major modes for ASP with main mode `html-mode'.
This also covers inlined style and javascript."
  ("ASP Html Family" html-mode
   (mumamo-chunk-asp%
    mumamo-asp-chunk-inlined-script
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Org-mode

(defcustom mumamo-org-submodes
  '(
    (ditaa picture-mode)
    )
  "Alist for conversion of org #+BEGIN_SRC specifier to major mode.
Works kind of like `mumamo-major-modes'.

Note that adding '-mode' to the specifier is tried before this
list.

This may be used for example for org-babel \(see URL
`http://orgmode.org/worg/org-contrib/babel/')."
  :type '(alist
          :key-type (symbol :tag "Symbol in #BEGIN_SRC specifier")
          :value-type (repeat (choice
                               (command :tag "Major mode")
                               (symbol :tag "Major mode (not yet loaded)")))
          )
  :group 'mumamo-modes)

;;(mumamo-org-mode-from-spec 'css)
;;(mumamo-org-mode-from-spec 'abcdef)
(defun mumamo-org-mode-from-spec (major-spec)
  "Translate MAJOR-SPEC to a major mode.
Translate MAJOR-SPEC used in #BEGIN_SRC to a major mode.

See `mumamo-org-submodes' for an explanation."
  (mumamo-major-mode-from-spec major-spec mumamo-org-submodes))

(defun mumamo-chunk-org-html (pos max)
  "Find #+BEGIN_HTML ... #+END_HTML, return range and `html-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "#+BEGIN_HTML" "#+END_HTML" nil 'html-mode))

(defun mumamo-search-fw-org-src-start (pos max)
  "Helper for `mumamo-chunk-org-src'.
POS is where to start search and MAX is where to stop."
  (let ((where (mumamo-chunk-start-fw-str pos max "#+BEGIN_SRC")))
    (when where
      (let ((exc-mode (let ((here (point)))
                        (goto-char where)
                        (prog1
                            (read (current-buffer))
                          (goto-char here)))))
        (setq exc-mode (mumamo-org-mode-from-spec exc-mode))
        (list where exc-mode)))))

(defun mumamo-search-fw-org-src-end (pos max)
  "Helper for `mumamo-chunk-org-src'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "#+END_SRC")))

(defun mumamo-chunk-org-src (pos max)
  "Find #+BEGIN_SRC ... #+END_SRC, return range and choosen major mode.
See `mumamo-possible-chunk-forward' for POS and MAX.

See Info node `(org) Literal Examples' for how to specify major
mode."
  (mumamo-possible-chunk-forward pos max
                                 'mumamo-search-fw-org-src-start
                                 'mumamo-search-fw-org-src-end))

;;;###autoload
(define-mumamo-multi-major-mode org-mumamo-mode
  "Turn on multiple major modes for `org-mode' files with main mode `org-mode'.
** Note about HTML subchunks:
Unfortunately this only allows `html-mode' (not `nxhtml-mode') in
sub chunks."
    ("Org Mode + Html" org-mode
     (mumamo-chunk-org-html
      mumamo-chunk-org-src
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mako

;; See http://www.makotemplates.org/docs/syntax.html

;;; Comments mode
;; Fix-me: move to mumamo.el
(defconst mumamo-comment-font-lock-keywords
  (list
   (cons "\\(.*\\)" (list 1 font-lock-comment-face))
   ))
(defvar mumamo-comment-font-lock-defaults
  '(mumamo-comment-font-lock-keywords t t))

(define-derived-mode mumamo-comment-mode nil "Comment chunk"
  "For comment blocks."
  (set (make-local-variable 'font-lock-defaults) mumamo-comment-font-lock-defaults))



(defun mumamo-chunk-mako-<% (pos max)
  "Find <% ... %> and <%! ... %>. Return range and `python-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk (mumamo-possible-chunk-forward pos max
                                              'mumamo-mako-<%-fw-start
                                              'mumamo-mako-<%-fw-end
                                              'mumamo-mako-<%-find-borders
                                              )))
    (when chunk
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-mako-<%-find-borders (start end exc-mode)
  (when exc-mode
    (list
     (when start
       (+ start
          (if (eq ?! (char-after (+ start 2)))
              3
            2)))
     (when end (- end 2))
     exc-mode)))

(defun mumamo-mako-<%-fw-start (pos max)
  (let ((here (point))
        start
        ret)
    (goto-char pos)
    (setq start
          (re-search-forward "<%!?\\(?:[ \t]\\|$\\)" max t))
    (when start
      (setq ret (match-beginning 0)))
    (goto-char here)
    (when ret
      (list ret 'python-mode))))

(defun mumamo-mako-<%-fw-end (pos max)
  (save-match-data
    (mumamo-chunk-end-fw-str-inc pos max "%>"))) ;; ok



(defun mumamo-chunk-mako-% (pos max)
  "Find % python EOL.  Return range and `python-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((chunk (mumamo-whole-line-chunk pos max "%" 'python-mode)))
    (when chunk
      (setcdr (last chunk) '(mumamo-template-indentor))
      chunk)))

(defun mumamo-chunk-mako-one-line-comment (pos max)
  "Find ## comment EOL.  Return range and `python-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-whole-line-chunk pos max "##" 'mumamo-comment-mode))

(defun mumamo-chunk-mako-<%doc (pos max)
  (mumamo-quick-chunk-forward pos max "<%doc>" "</%doc>" 'borders 'mumamo-comment-mode))

(defun mumamo-chunk-mako-<%include (pos max)
  (mumamo-quick-chunk-forward pos max "<%include" "/>" 'borders 'html-mode))

(defun mumamo-chunk-mako-<%inherit (pos max)
  (mumamo-quick-chunk-forward pos max "<%inherit" "/>" 'borders 'html-mode))

(defun mumamo-chunk-mako-<%namespace (pos max)
  (mumamo-quick-chunk-forward pos max "<%namespace" "/>" 'borders 'html-mode))

(defun mumamo-chunk-mako-<%page (pos max)
  (mumamo-quick-chunk-forward pos max "<%page" "/>" 'borders 'html-mode))

;; Fix-me: this is not correct
(defun mumamo-chunk-mako-<%def (pos max)
  (mumamo-quick-chunk-forward pos max "<%def" "</%def>" 'borders 'html-mode))

(defun mumamo-chunk-mako$(pos max)
  "Find ${ ... }, return range and `python-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (mumamo-quick-chunk-forward pos max "${" "}" 'borders 'python-mode))

;;;###autoload
(define-mumamo-multi-major-mode mako-html-mumamo-mode
  "Turn on multiple major modes for Mako with main mode `html-mode'.
This also covers inlined style and javascript."
;; Fix-me: test case
;;
;; Fix-me: Add chunks for the tags, but make sure these are made
;; invisible to nxml-mode parser.
;;
;; Fix-me: Maybe finally add that indentation support for one-line chunks?
  ("Mako HTML Family" html-mode
   (
    mumamo-chunk-mako-one-line-comment
    mumamo-chunk-mako-<%doc
    mumamo-chunk-mako-<%include
    mumamo-chunk-mako-<%inherit
    mumamo-chunk-mako-<%namespace
    mumamo-chunk-mako-<%page

    mumamo-chunk-mako-<%def
    ;;mumamo-chunk-mako-<%namesp:name
    ;;mumamo-chunk-mako-<%call
    ;;mumamo-chunk-mako-<%text

    mumamo-chunk-mako-<%
    mumamo-chunk-mako-%
    mumamo-chunk-mako$

    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(mumamo-inherit-sub-chunk-family-locally 'mako-html-mumamo-mode 'mako-html-mumamo-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XSL

;;;###autoload
(define-mumamo-multi-major-mode xsl-nxml-mumamo-mode
  "Turn on multi major mode for XSL with main mode `nxml-mode'.
This covers inlined style and javascript."
  ("XSL nXtml Family" nxml-mode
   (
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    )))

;;;###autoload
(define-mumamo-multi-major-mode xsl-sgml-mumamo-mode
  "Turn on multi major mode for XSL with main mode `sgml-mode'.
This covers inlined style and javascript."
  ("XSL SGML Family" sgml-mode
   (
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Markdown

(defun mumamo-chunk-markdown-html-1 (pos max)
  (save-restriction
    (goto-char pos)
    (narrow-to-region (point) (or max (point-max)))
    (save-match-data
      (let ((here (point)))
        (when (re-search-forward (rx (* space)
                                     (submatch "<")
                                     (* (any "a-z"))
                                     (or ">" (any " \t\n")))
                                 nil t)
          (let ((beg (match-beginning 1))
                (end))
            (goto-char beg)
            (condition-case err
                (progn
                  (while (not (sgml-skip-tag-forward 1)))
                  (setq end (point)))
              (error (message "mumamo-chunk-markdown-html-1: %s" err)))
            (goto-char here)
            (when (and beg end)
              (cons beg end))))))))

(defun mumamo-chunk-markdown-html-fw-exc-fun (pos max)
  (let ((beg-end (mumamo-chunk-markdown-html-1 pos max)))
    (cdr beg-end)))

(defun mumamo-chunk-markdown-html (pos max)
  "Find a chunk of html code in `markdown-mode'.
Return range and `html-mode'.
See `mumamo-possible-chunk-forward' for POS and MAX."
  (let ((beg-end (mumamo-chunk-markdown-html-1 pos max)))
    (when beg-end
      (let ((beg (car beg-end))
            (end (cdr beg-end)))
        (list beg end 'html-mode
              nil ;; borders
              nil ;; parseable y
              'mumamo-chunk-markdown-html-fw-exc-fun
              nil ;; find-borders fun
              )))))

;;;###autoload
(define-mumamo-multi-major-mode markdown-html-mumamo-mode
  "Turn on multi major markdown mode in buffer.
Main major mode will be `markdown-mode'.
Inlined html will be in `html-mode'.

You need `markdown-mode' which you can download from URL
`http://jblevins.org/projects/markdown-mode/'."
  ("Markdown HTML Family" markdown-mode
   (
    mumamo-chunk-markdown-html
    )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Latex related

(defun mumamo-latex-closure-chunk (pos max)
  (mumamo-quick-chunk-forward pos max "\\begin{clojure}" "\\end{clojure}" 'borders 'clojure-mode))

;;;###autoload
(define-mumamo-multi-major-mode latex-clojure-mumamo-mode
  "Turn on multi major mode latex+clojure.
Main major mode will be `latex-mode'.
Subchunks will be in `clojure-mode'.

You will need `clojure-mode' which you can download from URL
`http://github.com/jochu/clojure-mode/tree'."
  ("Latex+clojur Family" latex-mode
   (
    mumamo-latex-closure-chunk
    )))

(add-to-list 'auto-mode-alist '("\\.lclj\\'" . latex-clojure-mumamo-mode))


(defun mumamo-latex-haskell-chunk (pos max)
  (mumamo-quick-chunk-forward pos max "\\begin{code}" "\\end{code}" 'borders 'haskell-mode))

;;;###autoload
(define-mumamo-multi-major-mode latex-haskell-mumamo-mode
  "Turn on multi major mode latex+haskell.
Main major mode will be `latex-mode'.
Subchunks will be in `haskell-mode'.

You will need `haskell-mode' which you can download from URL
`http://projects.haskell.org/haskellmode-emacs/'."
  ("Latex+haskell Family" latex-mode
   (
    mumamo-latex-haskell-chunk
    )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Python + ReST

;; From Martin Soto

(defun mumamo-python-rst-long-string-chunk (pos max)
 "Find Python long strings.  Return range and 'mumamo-comment-mode.
See `mumamo-possible-chunk-forward' for POS and MAX."
 ;;(mumamo-quick-chunk-forward pos max "\"\"\"((" "))\"\"\"" nil 'rst-mode nil))
 (mumamo-quick-chunk-forward pos max "\"\"\"" "\"\"\"" 'borders 'rst-mode))

;;;###autoload
(define-mumamo-multi-major-mode python-rst-mumamo-mode
 "Turn on multiple major modes for Python with RestructuredText docstrings."
 ("Python ReST Family" python-mode
  (
   mumamo-python-rst-long-string-chunk
   )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Amrita

(eval-when-compile (require 'amrita nil t))

(defcustom mumamo-amrita-mode-alist nil
  "Alist used by `mumamo-chunk-amrita-fold' to get chunk mode.
This is used only if the major mode can not by guessed.  See
`mumamo-search-fw-amrita-fold' for more info."
  :type '(alist :key-type string :value-type function)
  :group 'mumamo-modes)

(defun mumamo-search-fw-amrita-fold (pos max)
  (when (require 'amrita nil t)
    (let ((here (point))
          (hit-pos -1)
          start
          (patt (rx word-start
                    "fold::"
                    (submatch (+ (any alpha)))
                    (+ space)
                    "{"
                    (* not-newline)
                    line-end
                    ))
          spec
          chunk-mode)
      (save-match-data
        (while (and (not start) hit-pos)
          (setq hit-pos (re-search-forward patt nil max))
          (when hit-pos
            ;; check if not in string
            (if (mumamo-end-in-code pos hit-pos amrita-mode-syntax-table)
                (setq start (match-end 0))
              (goto-char (match-end 1)))))
        (when start
          (setq spec (match-string-no-properties 1))
          ;; Guess the major mode name
          (setq chunk-mode (intern-soft (concat spec "-mode")))
          (unless (commandp chunk-mode)
            (setq chunk-mode (assoc spec mumamo-amrita-mode-alist))
            (if chunk-mode
                (setq chunk-mode (cdr chunk-mode))
              (setq chunk-mode 'mumamo-bad-mode)))))
      (goto-char here)
      (when start
        (list start chunk-mode nil)))))

(defun mumamo-search-fw-exc-end-amrita-fold (pos max)
  "Helper for `mumamo-chunk-amrita-fold.
POS is where to start search and MAX is where to stop.

Note: This simply matches {}-pairs and will fail if there is a
non-matching pair inside a string."
  (save-match-data
    (let ((here (point))
          ;; {} par level, we start after first {
          (level 1))
      (goto-char pos)
      (while (and (> level 0)
                  (re-search-forward "[{}]" max t))
        (setq level (+ level
                       (if (eq (char-before) ?\{)
                           1 -1))))
      (prog1
          (when (= 0 level) (1- (point)))
        (goto-char here)))))

(defun mumamo-chunk-amrita-fold (pos max)
  "Find Amrita fold::PROGLANG chnks."
  (mumamo-possible-chunk-forward pos max
                                 ;;'mumamo-search-fw-exc-start-xml-pi-new
                                 'mumamo-search-fw-amrita-fold
                                 ;;'mumamo-search-fw-exc-end-xml-pi
                                 'mumamo-search-fw-exc-end-amrita-fold
                                 ;;'mumamo-find-borders-xml-pi
                                 nil
                                 ))

;;;###autoload
(define-mumamo-multi-major-mode amrita-mumamo-mode
 "Turn on multiple major modes for Amrita.
Fix-me: This does not yet take care of inner chunks."
 ("Amrita Family" amrita-mode
  (
   mumamo-chunk-amrita-fold
   )))

(provide 'mumamo-fun)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-fun.el ends here
