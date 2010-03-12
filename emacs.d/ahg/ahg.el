;;; ahg.el --- Alberto's Emacs interface for Mercurial (Hg)

;; Copyright (C) 2008 Alberto Griggio

;; Author: Alberto Griggio <agriggio@users.sourceforge.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; A simple Emacs interface for the Mercurial (Hg) Distributed SCM.
;;; Installation: put this file where Emacs can find it, and then add the line
;;; (require 'ahg)
;;; to your .emacs

(require 'diff-mode)
(require 'easymenu)
(require 'log-edit)
(require 'ewoc)
(require 'cl)

;;-----------------------------------------------------------------------------
;; ahg-version
;;-----------------------------------------------------------------------------

(defvar ahg-version-string "0.99")

(defun ahg-version ()
  "Shows aHg version number."
  (interactive)
  (message "aHg version %s" ahg-version-string))

;;-----------------------------------------------------------------------------
;; the global aHg menu and keymap
;;-----------------------------------------------------------------------------

(easy-menu-add-item nil '("tools")
                    '("aHg"
                      ["Status" ahg-status t]
                      ["Log Summary" ahg-short-log t]
                      ["Detailed Log" ahg-log t]
                      ["Revision DAG" ahg-glog t]
                      ["Commit Current File" ahg-commit-cur-file t]
                      ["View Changes of Current File" ahg-diff-cur-file t]
                      ["View Change Log of Current File" ahg-log-cur-file t]
                      ["--" nil nil]
                      ("Mercurial Queues"
                       ["New Patch" ahg-qnew t]
                       ["View Qdiff" ahg-qdiff t]
                       ["Refresh Current Patch" ahg-qrefresh t]
                       ["Go to Patch..." ahg-qgoto t]
                       ["Pop All Patches" ahg-qpop-all t]
                       ["Show Name of Current Patch" ahg-qtop t]
                       ["List All Patches" ahg-mq-list-patches t]
                       ["Delete Patch..." ahg-qdelete t]
                       ["Convert Current Patch to Changeset"
                        ahg-mq-convert-patch-to-changeset t]
                       ["Edit series File" ahg-mq-edit-series t])
                      ["--" nil nil]
                      ["Execute Hg Command" ahg-do-command t]
                      ["Help on Hg Command" ahg-command-help t])
                    "PCL-CVS")

(defvar ahg-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'ahg-status)
    (define-key map "l" 'ahg-short-log)
    (define-key map "L" 'ahg-log)
    (define-key map "G" 'ahg-glog)
    (define-key map "g" 'ahg-glog)
    (define-key map "!" 'ahg-do-command)
    (define-key map "h" 'ahg-command-help)
    (define-key map "c" 'ahg-commit-cur-file)
    (define-key map "=" 'ahg-diff-cur-file)
    (define-key map (kbd "C-l") 'ahg-log-cur-file)
    (define-key map "Q"
      (let ((qmap (make-sparse-keymap)))
        (define-key qmap "n" 'ahg-qnew)
        (define-key qmap "=" 'ahg-qdiff)
        (define-key qmap "r" 'ahg-qrefresh)
        (define-key qmap "g" 'ahg-qgoto)
        (define-key qmap "p" 'ahg-qpop-all)
        (define-key qmap "t" 'ahg-qtop)
        (define-key qmap "d" 'ahg-qdelete)
        (define-key qmap "f" 'ahg-mq-convert-patch-to-changeset)
        (define-key qmap "l" 'ahg-mq-list-patches)
        (define-key qmap "e" 'ahg-mq-edit-series)
        qmap))
    map))

;;-----------------------------------------------------------------------------
;; Customization
;;-----------------------------------------------------------------------------

(defgroup ahg nil "aHg Mercurial Frontend" :group 'tools)

(defcustom ahg-hg-command "hg"
  "Command to use for invoking Mercurial."
  :group 'ahg :type 'string)

(defcustom ahg-global-key-prefix "hg"
  "Prefix of globally-available aHg commands."
  :group 'ahg :type 'string
  :set (function (lambda (symbol value)
                   (when (boundp symbol) (global-unset-key (eval symbol)))
                   (global-set-key (set symbol value) ahg-global-map))))

(defcustom ahg-do-command-insert-header t
  "If non-nil, `ahg-do-command' will insert a header line in the
command output." :group 'ahg :type 'boolean)

(defcustom ahg-do-command-show-buffer-immediately t
  "If non-nil, `ahg-do-command' will immediately switch to the buffer with the
command output, instead of waiting for the command to finish."
  :group 'ahg :type 'boolean)

(defcustom ahg-auto-refresh-status-buffer t
  "If non-nil, automatically refresh the *aHg status* buffer when certain
operations (e.g. add, remove, commit) are performed."
  :group 'ahg :type 'boolean)

(defcustom ahg-restore-window-configuration-on-quit t
  "If non-nil, when `ahg-buffer-quit' will restore the window configuration."
  :group 'ahg :type 'boolean)

(defcustom ahg-diff-use-git-format t
  "If non-nil, aHg commands that output a diff will use the git format."
  :group 'ahg :type 'boolean)

(defcustom ahg-yesno-short-prompt t
  "If non-nil, use short form (y or n) when asking for confimation to the user."
  :group 'ahg :type 'boolean)

(defcustom ahg-i18n t
  "If non-nil, use i18n when calling Mercurial.
Note: disabling i18n is done by unsetting the LANG environment variable
when calling hg. This might not always work."
  :group 'ahg :type 'boolean)

(defcustom ahg-subprocess-coding-system nil
  "If non-nil, coding system used when reading output of hg commands."
  :group 'ahg :type 'symbol)

(defface ahg-status-marked-face
  '((default (:inherit font-lock-preprocessor-face)))
  "Face for marked files in aHg status buffers." :group 'ahg)

(defface ahg-status-modified-face
  '((default (:inherit font-lock-function-name-face)))
  "Face for modified files in aHg status buffers." :group 'ahg)

(defface ahg-status-added-face
  '((default (:inherit font-lock-type-face)))
  "Face for added files in aHg status buffers." :group 'ahg)

(defface ahg-status-removed-face
  '((default (:inherit font-lock-constant-face)))
  "Face for removed files in aHg status buffers." :group 'ahg)

(defface ahg-status-clean-face
  '((default (:inherit default)))
  "Face for clean files in aHg status buffers." :group 'ahg)

(defface ahg-status-deleted-face
  '((default (:inherit font-lock-string-face)))
  "Face for deleted files in aHg status buffers." :group 'ahg)

(defface ahg-status-ignored-face
  '((default (:inherit font-lock-comment-face)))
  "Face for ignored files in aHg status buffers." :group 'ahg)

(defface ahg-status-unknown-face
  '((default (:inherit font-lock-variable-name-face)))
  "Face for unknown files in aHg status buffers." :group 'ahg)

(defface ahg-short-log-revision-face
  '((default (:inherit font-lock-function-name-face)))
  "Face for revision field in aHg short log buffers." :group 'ahg)

(defface ahg-short-log-date-face
  '((default (:inherit font-lock-string-face)))
  "Face for date field in aHg short log buffers." :group 'ahg)

(defface ahg-log-revision-face
  '((default (:inherit font-lock-variable-name-face)))
  "Face for revision field in aHg log buffers." :group 'ahg)

(defface ahg-log-field-face
  '((default (:inherit font-lock-function-name-face)))
  "Face for fields in aHg log buffers." :group 'ahg)

(defface ahg-log-branch-face
  '((default (:inherit font-lock-keyword-face)))
  "Face for tags and branches in aHg log buffers." :group 'ahg)

(defface ahg-short-log-user-face
  '((default (:inherit font-lock-type-face)))
  "Face for user field in aHg short log buffers." :group 'ahg)

(defface ahg-header-line-face
  '((default (:inherit font-lock-comment-face)))
  "Face for header lines in aHg buffers." :group 'ahg)

(defface ahg-header-line-root-face
  '((default (:inherit font-lock-constant-face)))
  "Face for repository path in header lines of aHg buffers." :group 'ahg)

;;-----------------------------------------------------------------------------
;; Variable definitions for faces
;;-----------------------------------------------------------------------------

(defvar ahg-status-marked-face 'ahg-status-marked-face)
(defvar ahg-status-modified-face 'ahg-status-modified-face)
(defvar ahg-status-added-face 'ahg-status-added-face)
(defvar ahg-status-removed-face 'ahg-status-removed-face)
(defvar ahg-status-clean-face 'ahg-status-clean-face)
(defvar ahg-status-deleted-face 'ahg-status-deleted-face)
(defvar ahg-status-ignored-face 'ahg-status-ignored-face)
(defvar ahg-status-unknown-face 'ahg-status-unknown-face)
(defvar ahg-short-log-revision-face 'ahg-short-log-revision-face)
(defvar ahg-short-log-date-face 'ahg-short-log-date-face)
(defvar ahg-log-field-face 'ahg-log-field-face)
(defvar ahg-log-branch-face 'ahg-log-branch-face)
(defvar ahg-log-revision-face 'ahg-log-revision-face)
(defvar ahg-short-log-user-face 'ahg-short-log-user-face)
(defvar ahg-header-line-face 'ahg-header-line-face)
(defvar ahg-header-line-root-face 'ahg-header-line-root-face)
(defvar ahg-invisible-face
  (progn
    (copy-face 'default 'ahg-invisible-face)
    (set-face-foreground 'ahg-invisible-face (face-background 'default))
    'ahg-invisible-face))


;;-----------------------------------------------------------------------------
;; hg root
;;-----------------------------------------------------------------------------

(defun ahg-root ()
  "Returns the root of the tree handled by Mercurial, or nil if
the current dir is not under hg."
  (with-temp-buffer
    (when (= (ahg-call-process "root") 0)
      (buffer-substring-no-properties (point-min) (1- (point-max))))))

;;-----------------------------------------------------------------------------
;; hg identify
;;-----------------------------------------------------------------------------

(defun ahg-identify (&optional root)
  (interactive)
  (with-temp-buffer
    (let* ((args (list "-nibt"))
           (status
           (if root
               (let ((default-directory (file-name-as-directory root)))
                 (ahg-call-process "identify" args))
             (ahg-call-process "identify" args))))
      (when (= status 0)
        (concat
         "Id: "
         (buffer-substring-no-properties (point-min) (1- (point-max))))))))

(defun ahg-summary-info (root)
  (with-temp-buffer
    (let ((status (let ((default-directory (file-name-as-directory root)))
                    (ahg-call-process "summary" nil))))
      (if (= status 0)
          (buffer-substring-no-properties (point-min) (1- (point-max)))
        ;; if hg summary is not available, fall back to hg identify
        (ahg-identify root)))))

;;-----------------------------------------------------------------------------
;; hg status
;;-----------------------------------------------------------------------------

(defvar ahg-face-status-hash
  (let* ((test (define-hash-table-test 'ahg-str-hash 'string= 'sxhash))
         (h (make-hash-table :test 'ahg-str-hash)))
    (puthash "M" ahg-status-modified-face h)
    (puthash "A" ahg-status-added-face h)
    (puthash "R" ahg-status-removed-face h)
    (puthash "C" ahg-status-clean-face h)
    (puthash "=" ahg-status-clean-face h) ;; for Mercurial > 0.9.5
    (puthash "!" ahg-status-deleted-face h)
    (puthash "I" ahg-status-ignored-face h)
    (puthash "?" ahg-status-unknown-face h)
    h))

(defvar ahg-status-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'ahg-status-visit-file-other-window)
    map))

(defun ahg-face-from-status (status-code)
  (gethash status-code ahg-face-status-hash 'default))


(define-derived-mode ahg-status-mode nil "aHg-status"
  "Major mode for *hg status* buffers.

Commands:
\\{ahg-status-mode-map}
"
  (buffer-disable-undo) ;; undo info not needed here
  (toggle-read-only t)
  (font-lock-mode nil)
  (define-key ahg-status-mode-map " " 'ahg-status-toggle-mark)
  (define-key ahg-status-mode-map "m" 'ahg-status-mark)
  (define-key ahg-status-mode-map "u" 'ahg-status-unmark)
  (define-key ahg-status-mode-map (kbd "M-DEL") 'ahg-status-unmark-all)
  (define-key ahg-status-mode-map "c" 'ahg-status-commit)
  (define-key ahg-status-mode-map "a" 'ahg-status-add)
  (define-key ahg-status-mode-map "A" 'ahg-status-addremove)
  (define-key ahg-status-mode-map "=" 'ahg-status-diff)
  (define-key ahg-status-mode-map "D" 'ahg-status-diff-all)
  (define-key ahg-status-mode-map "r" 'ahg-status-remove)
  (define-key ahg-status-mode-map "g" 'ahg-status-refresh)
  (define-key ahg-status-mode-map "q" 'ahg-buffer-quit)
  (define-key ahg-status-mode-map "U" 'ahg-status-undo)
  (define-key ahg-status-mode-map "!" 'ahg-status-do-command)
  (define-key ahg-status-mode-map "l" 'ahg-status-short-log)
  (define-key ahg-status-mode-map "L" 'ahg-status-log)
  (define-key ahg-status-mode-map "G" 'ahg-status-glog)
  (define-key ahg-status-mode-map "f" 'ahg-status-visit-file)
  (define-key ahg-status-mode-map "\r" 'ahg-status-visit-file)
  (define-key ahg-status-mode-map "o" 'ahg-status-visit-file-other-window)
  (define-key ahg-status-mode-map "h" 'ahg-command-help)
  (define-key ahg-status-mode-map "$" 'ahg-status-shell-command)
  (define-key ahg-status-mode-map "F" 'ahg-status-dired-find)
  (let ((showmap (make-sparse-keymap)))
    (define-key showmap "A" 'ahg-status-show-all)
    (define-key showmap "m" 'ahg-status-show-modified)
    (define-key showmap "a" 'ahg-status-show-added)
    (define-key showmap "r" 'ahg-status-show-removed)
    (define-key showmap "d" 'ahg-status-show-deleted)
    (define-key showmap "c" 'ahg-status-show-clean)
    (define-key showmap "u" 'ahg-status-show-unknown)
    (define-key showmap "i" 'ahg-status-show-ignored)
    (define-key ahg-status-mode-map "s" showmap))
  (let ((qmap (make-sparse-keymap)))
    (define-key qmap "n" 'ahg-qnew)
    (define-key qmap "=" 'ahg-qdiff)
    (define-key qmap "r" 'ahg-qrefresh)
    (define-key qmap "g" 'ahg-qgoto)
    (define-key qmap "p" 'ahg-qpop-all)
    (define-key qmap "t" 'ahg-qtop)
    (define-key qmap "d" 'ahg-qdelete)
    (define-key qmap "f" 'ahg-mq-convert-patch-to-changeset)
    (define-key qmap "l" 'ahg-mq-list-patches)
    (define-key qmap "e" 'ahg-mq-edit-series)
    (define-key ahg-status-mode-map "Q" qmap))
  (easy-menu-add ahg-status-mode-menu ahg-status-mode-map))

(easy-menu-define ahg-status-mode-menu ahg-status-mode-map "aHg Status"
  '("aHg Status"
    ["Visit File" ahg-status-visit-file [:keys "f" :active t]]
    ["Visit File (Other Window)" ahg-status-visit-file-other-window
     [:keys "o" :active t]]
    ["--" nil nil]
    ["Commit" ahg-status-commit [:keys "c" :active t]]
    ["Add" ahg-status-add [:keys "a" :active t]]
    ["Remove" ahg-status-remove [:keys "r" :active t]]
    ["Add/Remove" ahg-status-addremove [:keys "A" :active t]]
    ["Undo" ahg-status-undo [:keys "U" :active t]]
    ["Hg Command" ahg-status-do-command [:keys "!" :active t]]
    ["Shell Command" ahg-status-shell-command [:keys "$" :active t]]
    ["--" nil nil]
    ["Toggle Mark" ahg-status-toggle-mark [:keys " " :active t]]
    ["Mark" ahg-status-mark [:keys "m" :active t]]
    ["Unmark" ahg-status-unmark [:keys "u" :active t]]
    ["Unmark All" ahg-status-unmark-all [:keys (kbd "M-DEL") :active t]]
    ["Show Marked in Dired" ahg-status-dired-find [:keys "F" :active t]]
    ["--" nil nil]
    ["Log Summary" ahg-short-log [:keys "l" :active t]]
    ["Detailed Log" ahg-log [:keys "L" :active t]]
    ["Revision DAG" ahg-glog [:keys "G" :active t]]
    ["Diff" ahg-status-diff [:keys "=" :active t]]
    ["Diff Marked" ahg-status-diff-all [:keys "D" :active t]]
    ["--" nil nil]
    ("Show"
     ["All" ahg-status-show-all [:keys "sA" :active t]]
     ["Modified" ahg-status-show-modified [:keys "sm" :active t]]
     ["Added" ahg-status-show-added [:keys "sa" :active t]]
     ["Removed" ahg-status-show-removed [:keys "sr" :active t]]
     ["Deleted" ahg-status-show-deleted [:keys "sd" :active t]]
     ["Clean" ahg-status-show-clean [:keys "sc" :active t]]
     ["Unknown" ahg-status-show-unknown [:keys "su" :active t]]
     ["Ignored" ahg-status-show-ignored [:keys "si" :active t]]
     )
    ["--" nil nil]
    ("Mercurial Queues"
     ["New Patch" ahg-qnew [:keys "Qn" :active t]]
     ["View Qdiff" ahg-qdiff [:keys "Q=" :active t]]
     ["Refresh Current Patch" ahg-qrefresh [:keys "Qr" :active t]]
     ["Go to Patch..." ahg-qgoto [:keys "Qg" :active t]]
     ["Pop All Patches" ahg-qpop-all [:keys "Qp" :active t]]
     ["Show Name of Current Patch" ahg-qtop [:keys "Qt" :active t]]
     ["List All Patches" ahg-mq-list-patches [:keys "Ql" :active t]]
     ["Delete Patch..." ahg-qdelete [:keys "Qd" :active t]]
     ["Convert Current Patch to Changeset"
      ahg-mq-convert-patch-to-changeset [:keys "Qf" :active t]]
     ["Edit series File" ahg-mq-edit-series [:keys "Qe" :active t]])
    ["--" nil nil]
    ["Help on Hg Command" ahg-command-help [:keys "h" :active t]]
    ["--" nil nil]
    ["Refresh" ahg-status-refresh [:keys "g" :active t]]
    ["Quit" ahg-buffer-quit [:keys "q" :active t]]
    ))


(defun ahg-status (&rest extra-switches)
  "Run hg status. When called non-interactively, it is possible
to pass extra switches to hg status."
  (interactive)
  (let ((buf (get-buffer-create "*aHg-status*"))
        (curdir default-directory)
        (show-message (interactive-p)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq default-directory (file-name-as-directory curdir))
      (set (make-local-variable 'ahg-root) (ahg-root))
      (ahg-push-window-configuration))
    (ahg-generic-command
     "status" extra-switches
     (lexical-let ((no-pop ahg-status-no-pop)
                   (point-pos ahg-status-point-pos))
       (lambda (process status)
         (ahg-status-sentinel process status no-pop point-pos))) buf
         nil (not show-message))))

(defun ahg-status-pp (data)
  "Pretty-printer for data elements in a *hg status* buffer."
  ;; data is a cons cell: (marked flag (hg status . filename))
  (let ((marked (car data))
        (status-code (cadr data))
        (filename (cddr data)))
    (if marked
        (progn
          (insert (propertize "*" 'face (cons 'foreground-color "#00CC00")))
          (insert (propertize (concat status-code " " filename)
                              'face ahg-status-marked-face)))
      (insert (propertize (concat " " status-code " " filename)
                          'face (ahg-face-from-status status-code)
                          'mouse-face 'highlight
                          'keymap ahg-status-line-map)))))


(defun ahg-status-toggle-mark ()
  (interactive)
  (let* ((node (ewoc-locate ewoc))
         (data (and node (ewoc-data node)))
         (inhibit-read-only t))
    (when data
      (if (car data) (ewoc-set-data node (cons nil (cdr data)))
        (ewoc-set-data node (cons t (cdr data))))
      (ewoc-invalidate ewoc node)
      (setq node (ewoc-next ewoc node))
      (when node (goto-char (ewoc-location node))))))

(defun ahg-status-do-mark (yes)
  (let* ((node (ewoc-locate ewoc))
         (data (and node (ewoc-data node)))
         (inhibit-read-only t))
    (when data
      (if yes (ewoc-set-data node (cons t (cdr data)))
        (ewoc-set-data node (cons nil (cdr data))))
      (ewoc-invalidate ewoc node)
      (setq node (ewoc-next ewoc node))
      (when node (goto-char (ewoc-location node))))))

(defun ahg-status-mark () (interactive) (ahg-status-do-mark t))
(defun ahg-status-unmark () (interactive) (ahg-status-do-mark nil))

(defun ahg-status-unmark-all ()
  (interactive)
  (ewoc-map (lambda (d) (when (car d) (setcar d nil) t)) ewoc))

(defun ahg-status-show-all () (interactive) (ahg-status "-A"))
(defun ahg-status-show-modified () (interactive) (ahg-status "-m"))
(defun ahg-status-show-added () (interactive) (ahg-status "-a"))
(defun ahg-status-show-removed () (interactive) (ahg-status "-r"))
(defun ahg-status-show-deleted () (interactive) (ahg-status "-d"))
(defun ahg-status-show-clean () (interactive) (ahg-status "-c"))
(defun ahg-status-show-unknown () (interactive) (ahg-status "-u"))
(defun ahg-status-show-ignored () (interactive) (ahg-status "-i"))

(defun ahg-status-get-marked (action-if-empty &optional filter)
  "Returns the list of marked nodes. If such list is empty, behave according to
ACTION-IF-EMPTY: if nil, do nothing. If 'all, return all nodes, if 'cur return
the singleton list with the node at point."
  (let ((marked (ewoc-collect ewoc
                              (if filter (lambda (d)
                                           (and (car d) (funcall filter d)))
                                'car))))
    (if (null marked)
      (cond ((eq action-if-empty nil) nil)
            ((eq action-if-empty 'all)
             (ewoc-collect ewoc (if filter filter 'identity)))
            ((eq action-if-empty 'cur)
             (let ((n (ewoc-locate ewoc)))
               (when (and n (or (null filter) (funcall filter (ewoc-data n))))
                 (list (ewoc-data n))))))
      marked)))

(defun ahg-status-commit ()
  (interactive)
  (let ((files (ahg-status-get-marked nil)))
    (ahg-commit (mapcar 'cddr files))))

(defun ahg-status-add ()
  (interactive)
  (let ((files (ahg-status-get-marked
                'all (lambda (data) (string= (cadr data) "?")))))
    (if (ahg-y-or-n-p (format "Add %d files to hg? " (length files)))
        (ahg-generic-command
         "add" (mapcar 'cddr files)
         (lexical-let ((howmany (length files))
                       (aroot (ahg-root)))
           (lambda (process status)
             (if (string= status "finished\n")
                 (with-current-buffer
                     (process-buffer process)
                   (ahg-status-maybe-refresh aroot)
                   (kill-buffer nil)
                   (message "Added %d files" howmany))
               (ahg-show-error process)))))
      (message "hg add aborted"))))

(defun ahg-status-remove ()
  (interactive)
  (let ((files (ahg-status-get-marked
                 'all (lambda (data)
                        (let ((f (cadr data)))
                          (or (string= f "A") (string= f "C")))))))
    (if (ahg-y-or-n-p (format "Remove %d files from hg? " (length files)))
        (ahg-generic-command
         "remove" (mapcar 'cddr files)
         (lexical-let ((howmany (length files))
                       (aroot (ahg-root)))
           (lambda (process status)
             (if (string= status "finished\n")
                 (with-current-buffer (process-buffer process)
                   (ahg-status-maybe-refresh aroot)
                   (kill-buffer nil)
                   (message "Removed %d files" howmany))
               (ahg-show-error process)))))
      (message "hg remove aborted"))))

(defun ahg-status-addremove ()
  (interactive)
  (let ((files (ahg-status-get-marked
                'all (lambda (data)
                       (or (string= (cadr data) "?")
                           (string= (cadr data) "!"))))))
    (if (ahg-y-or-n-p (format "Add/Remove %d files to/from hg? "
                              (length files)))
        (ahg-generic-command
         "addremove" (mapcar 'cddr files)
         (lexical-let ((howmany (length files))
                       (aroot (ahg-root)))
           (lambda (process status)
             (if (string= status "finished\n")
                 (with-current-buffer
                     (process-buffer process)
                   (ahg-status-maybe-refresh aroot)
                   (kill-buffer nil)
                   (message "Added/Removed %d files" howmany))
               (ahg-show-error process)))))
      (message "hg addremove aborted"))))


(defun ahg-status-refresh ()
  (interactive)
  (let ((ahg-status-point-pos (ahg-line-point-pos)))
    (call-interactively 'ahg-status)))


(defun ahg-status-maybe-refresh (root)
  (when ahg-auto-refresh-status-buffer
    (let ((buf (ahg-get-status-buffer root)))
      (when buf
        (let ((ahg-status-no-pop t)
              (ahg-status-point-pos
               (with-current-buffer buf (ahg-line-point-pos))))
          (ahg-status))))))


(defun ahg-status-diff (&optional all)
  "Shows changes of the current revision wrt. its parent. If ALL is t,
shows changes of all marked files. Otherwise, shows changes of
the file on the current line."
  (interactive)
  (let ((files
         (if all (ahg-status-get-marked 'all)
           (let ((n (ewoc-locate ewoc))) (when n (list (ewoc-data n))))))
;;         (ahg-status-get-marked (if all 'all 'cur)))
        (buf (get-buffer-create "*aHg diff*"))
        (curdir default-directory)
        (inhibit-read-only t)
        (args (when ahg-diff-use-git-format '("--git"))))
    (with-current-buffer buf
      (setq default-directory (file-name-as-directory curdir)))
    (cond ((null files) (message "aHg diff: no file selected."))
          (t      
           (with-current-buffer buf
             (erase-buffer)
             (ahg-push-window-configuration))
           (ahg-generic-command "diff" (append args (mapcar 'cddr files))
                                (lambda (process status)
                                  (if (string= status "finished\n")
                                      (progn
                                        (pop-to-buffer (process-buffer process))
                                        (ahg-diff-mode)
                                        (set-buffer-modified-p nil)
                                        (goto-char (point-min)))
                                    (ahg-show-error process)))
                                buf)))
     ))

(defun ahg-status-diff-all ()
  (interactive)
  (ahg-status-diff t))

(defun ahg-status-undo ()
  (interactive)
  (let ((files (ahg-status-get-marked nil)))
    (if (ahg-y-or-n-p
         (if files (format "Undo changes on %d files? " (length files))
           "Undo all changes? "))
        (ahg-generic-command
         "revert" (if files (mapcar 'cddr files) '("--all"))
         (lexical-let ((aroot (ahg-root)))
           (lambda (process status)
             (if (string= status "finished\n")
                 (with-current-buffer
                     (process-buffer process)
                   (ahg-status-maybe-refresh aroot)
                   (kill-buffer nil))
               (ahg-show-error process)))))
      (message "hg revert aborted"))))                         


(defun ahg-get-status-ewoc (root)
  "Returns an *hg status* buffer for ROOT. The buffer's major mode is
ahg-status, and it has an ewoc associated with it."
  (let ((buf (ahg-get-status-buffer root t))
        (inhibit-read-only t)
        (header (concat
                 (propertize "hg status for " 'face ahg-header-line-face)
                 (propertize root 'face ahg-header-line-root-face) "\n"))
        (footer (concat "\n"
                        (make-string (1- (window-width (selected-window))) ?-)
                        "\n" (ahg-summary-info root))))
    (with-current-buffer buf
      (erase-buffer)
      (let ((ew (ewoc-create 'ahg-status-pp
                             header footer)))
        (ahg-status-mode)
        (set (make-local-variable 'ewoc) ew)
        (setq default-directory (file-name-as-directory root))
        ew))))

(defun ahg-get-status-buffer (&optional root create)
  (unless root (setq root (ahg-root)))
  (let ((name (concat "*hg status: "
                      (file-name-as-directory (expand-file-name root)) "*")))
    (funcall (if create 'get-buffer-create 'get-buffer) name)))

(defvar ahg-status-no-pop nil)
(defvar ahg-status-point-pos nil)

(defun ahg-status-sentinel (process status &optional no-pop point-pos)
  (with-temp-message (or (current-message) "")
    (if (string= status "finished\n")
        ;; everything was ok, we can show the status buffer
        (let* ((buf (process-buffer process))
               (root (with-current-buffer buf ahg-root))
               (ew (ahg-get-status-ewoc root))
               (outbuf (ewoc-buffer ew))
               (cfg (with-current-buffer buf ahg-window-configuration)))
          (with-current-buffer buf
            (goto-char (point-min))
            (while (not (eobp))
              (ewoc-enter-last
               ew
               (cons nil
                     (cons (buffer-substring (point) (1+ (point)))
                           (buffer-substring (+ (point) 2) (point-at-eol)))))
              (forward-line 1)))
          (kill-buffer buf)
          (unless no-pop
            (pop-to-buffer outbuf))
          (set (make-local-variable 'ahg-window-configuration) cfg)
          (let ((inhibit-read-only t)
                (node (ewoc-nth ew 0)))
            (when node (goto-char (ewoc-location node))))
          (when point-pos
            (with-current-buffer outbuf
              (ahg-goto-line-point point-pos))))
      ;; error, we signal it and pop to the buffer
      (ahg-show-error process))))


(defun ahg-status-visit-file (&optional other-window)
  (interactive)
  (let* ((node (ewoc-locate ewoc))
         (data (and node (ewoc-data node))))
    (when data
      (if other-window
          (find-file-other-window (cddr data))
        (find-file (cddr data))))))

(defun ahg-status-visit-file-other-window ()
  (interactive)
  (ahg-status-visit-file t))


(defun ahg-status-do-command ()
  (interactive)
  (let ((files (ahg-status-get-marked nil)))
    (if files
        (let ((ahg-do-command-prompt "Hg command on selected files: ")
              (ahg-do-command-extra-args (mapcar 'cddr files)))
          (call-interactively 'ahg-do-command))
      (call-interactively 'ahg-do-command))))

(defun ahg-status-short-log ()
  (interactive)
  (let* ((files (ahg-status-get-marked nil))
         (ahg-file-list-for-log-command (if files (mapcar 'cddr files) nil)))
    (call-interactively 'ahg-short-log)))

(defun ahg-status-log ()
  (interactive)
  (let* ((files (ahg-status-get-marked nil))
         (ahg-file-list-for-log-command (if files (mapcar 'cddr files) nil)))
    (call-interactively 'ahg-log)))


(defun ahg-status-glog ()
  (interactive)
  (let* ((files (ahg-status-get-marked nil))
         (ahg-file-list-for-log-command (if files (mapcar 'cddr files) nil)))
    (call-interactively 'ahg-glog)))


(defun ahg-status-shell-command (command files refresh)
  "Run a shell command COMMAND on the marked files.
If there are no marked files, run COMMAND on the file at point.
This function uses `dired-do-shell-command' to do the work, and
so it supports the special meaning of `*' and `?' in COMMAND.
See the help for `dired-do-shell-command' for details.
If REFRESH is non-nil, refresh the status buffer after executing COMMAND.
When called interactively, REFRESH is non-nil if a prefix argument is given."
  (interactive
   (let ((fnames (ahg-status-get-marked 'cur)))
     (list
      (let ((minibuffer-local-completion-map
             (copy-keymap minibuffer-local-map)))
        (define-key minibuffer-local-completion-map "\t"
          'minibuffer-complete)
        (completing-read
         (apply 'format "Shell command on %s file%s: "
                (if (and (= (length fnames) 1) (null (caar fnames)))
                    (list "current" (format " (%s)" (cddar fnames)))
                  '("selected" "s")))
         (ahg-dynamic-completion-table ahg-complete-shell-command)))
      (mapcar 'cddr fnames)
      (if ahg-auto-refresh-status-buffer
          (not current-prefix-arg) current-prefix-arg))))
  (dired-do-shell-command command nil files)
  (when refresh (ahg-status-refresh)))


(defun ahg-status-dired-find (files)
  "Show the highlighted files in a dired buffer.
Uses find-dired to get them into nicely."
  (interactive (list (ahg-status-get-marked 'cur)))
  (find-dired
   default-directory
   (concat
    (mapconcat
     (lambda (f)
       (concat "-path \"*/" (cddr f) "\" -or"))
     files " ")
    " -false")))

;;-----------------------------------------------------------------------------
;; hg commit
;;-----------------------------------------------------------------------------

(defun ahg-commit-callback ()
  (interactive)
  (let ((msg (ahg-parse-commit-message)))
    (let ((args (append (list "-m" msg)
                        (log-edit-files))))
      (ahg-generic-command
       "commit" args
       (lexical-let ((aroot (ahg-root))
                     (n (length (log-edit-files))))
         (lambda (process status)
           (if (string= status "finished\n")
               (progn
                 (ahg-status-maybe-refresh aroot)
                 (message "Successfully committed %s."
                          (if (> n 0)
                              (format "%d file%s" n (if (> n 1) "s" ""))
                            "all modified files"))
                 (kill-buffer (process-buffer process)))
             (ahg-show-error process)))))))
  (kill-buffer (current-buffer)))

(defun ahg-commit (files)
  "Run hg commit. Pops up a buffer for editing the log file."
  (let ((buf (generate-new-buffer "*aHg-log*")))
    (ahg-log-edit
     'ahg-commit-callback
     (lexical-let ((flist files)) (lambda () flist))
     buf)))

(defun ahg-commit-cur-file ()
  "Run hg commit on the current file only."
  (interactive)
  (cond ((eq major-mode 'ahg-status-mode)
         (call-interactively 'ahg-status-commit))
        ((buffer-file-name)
         (when (and (buffer-modified-p)
                    (ahg-y-or-n-p "Save buffer before committing? "))
           (save-buffer))
         (ahg-commit (list (buffer-file-name))))
        (t (message "hg commit: no file found, aborting."))))

;;-----------------------------------------------------------------------------
;; hg update
;;-----------------------------------------------------------------------------

(defvar ahg-update-to-rev-get-revision-function nil)

(defun ahg-update-to-rev (rev force)
  "Update to the given revision. If force is non-nil, pass -C
flag to hg update."
  (interactive
   (let* ((rev (funcall ahg-update-to-rev-get-revision-function))
          (ans (and rev (ahg-y-or-n-p (format "Update to revision %s? " rev))))
          (overw (and ans (if (ahg-uncommitted-changes-p)
                              (ahg-y-or-n-p "Overwrite local changes? ")
                            t))))
     (list (and ans rev) overw)))
  (when rev
    (ahg-generic-command
     "update" (if force (list "-C" "-r" rev) (list "-r" rev))
     (lexical-let ((aroot default-directory)
                   (rev rev))
       (lambda (process status)
         (if (string= status "finished\n")
             (progn
               (ahg-status-maybe-refresh aroot)
               (message "Updated to revision %s" rev)
               (kill-buffer (process-buffer process)))
           (ahg-show-error process)))))))


;;-----------------------------------------------------------------------------
;; hg log
;;-----------------------------------------------------------------------------

(defvar ahg-short-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?g] 'ahg-short-log)
    (define-key map [?s] 'ahg-status)
    (define-key map [?=] 'ahg-short-log-view-diff)
    (define-key map [?D] 'ahg-short-log-view-diff-select-rev)
    (define-key map [? ] 'ahg-short-log-view-details)
    (define-key map [?\r] 'ahg-short-log-update-to-rev)
    (define-key map [?r] 'ahg-short-log-goto-revision)
    (define-key map [?n] 'ahg-short-log-next)
    (define-key map [?p] 'ahg-short-log-previous)
    (define-key map [?q] 'ahg-buffer-quit)
    (define-key map [?!] 'ahg-do-command)
    (define-key map [?h] 'ahg-command-help)
    map)
  "Keymap used in `ahg-short-log-mode'.")

(defvar ahg-short-log-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ahg-short-log-view-details-mouse)
    (define-key map [mouse-2] 'ahg-short-log-view-diff-mouse)
    map))

;; (defvar ahg-short-log-revision-face font-lock-function-name-face)
;; (defvar ahg-short-log-date-face font-lock-string-face)
;; (defvar ahg-short-log-user-face font-lock-type-face)

(defconst ahg-short-log-start-regexp "^ +\\([0-9]+\\) |")

(define-derived-mode ahg-short-log-mode fundamental-mode "ahg-short-log"
  "Major mode to display hg shortlog output.

Commands:
\\{ahg-short-log-mode-map}
"
  (buffer-disable-undo) ;; undo info not needed here
  (use-local-map ahg-short-log-mode-map)
  (font-lock-mode nil)
;;  (hl-line-mode t)
  (easy-menu-add ahg-short-log-mode-menu ahg-short-log-mode-map))

(easy-menu-define ahg-short-log-mode-menu ahg-short-log-mode-map "aHg Short Log"
  '("aHg Short Log"
    ["View Revision Diff" ahg-short-log-view-diff [:keys "=" :active t]]
    ["View Revision Diff with Other..." ahg-short-log-view-diff-select-rev
     [:keys "D" :active t]]
    ["View Revision Details" ahg-short-log-view-details [:keys " " :active t]]
    ["Update to Revision" ahg-short-log-update-to-rev [:keys "\r" :active t]]
    ["--" nil nil]
    ["Status" ahg-status [:keys "s" :active t]]
    ["Hg Command" ahg-do-command [:keys "!" :active t]]
    ["--" nil nil]
    ["Next Revision" ahg-short-log-next [:keys "n" :active t]]
    ["Previous Revision" ahg-short-log-previous [:keys "p" :active t]]
    ["Go to Revision..." ahg-short-log-goto-revision [:keys "r" :active t]]
    ["--" nil nil]
    ["Help on Hg Command" ahg-command-help [:keys "h" :active t]]
    ["--" nil nil]
    ["Refresh" ahg-short-log [:keys "g" :active t]]
    ["Quit" ahg-buffer-quit [:keys "q" :active t]]
    ))


(defun ahg-short-log-pp (data)
  "Pretty-printer for short log revisions."
  ;; data is a 4-elements list
  (labels ((trim (n s) (if (> (length s) n) (substring s 0 n) s)))
    (let* ((width (window-width (selected-window)))
           (p1 (car data))
           (p2 (cadr data))
           (p3 (caddr data))
           (p4 (cadddr data))
           (s (format "%7s | %s | %8s | %s"
                      (propertize p1 'face ahg-short-log-revision-face)
                      (propertize p2 'face ahg-short-log-date-face)
                      (propertize (trim 8 p3) 'face ahg-short-log-user-face)
                      p4))
           (pad (if (< (length s) width)
                    (make-string (- width (length s)) ? )
                  "")))
      (insert (propertize (concat s pad) 'mouse-face 'highlight
                          'keymap ahg-short-log-line-map)))))
        
(defun ahg-short-log-insert-contents (ewoc contents)
  (let ((lines (split-string contents "\n")))
    (labels ((format-line (line)
               (if (and line (> (length line) 0))
                    (let* ((p1 (string-match " " line))
                           (p2 (string-match " " line (1+ p1)))
                           (p3 (string-match " " line (1+ p2)))
                           (data (list
                                  (substring line 0 p1)
                                  (substring line (1+ p1) p2)
                                  (substring line (1+ p2) p3)
                                  (substring line (1+ p3)))))
                      (ewoc-enter-last ewoc data)))))
      (mapcar 'format-line lines))))


(defun ahg-short-log-next (n)
  "Move to the next changeset line"
  (interactive "p")
  (ewoc-goto-next ewoc n))

(defun ahg-short-log-previous (n)
  "Move to the previous changeset line"
  (interactive "p")
  (ewoc-goto-prev ewoc n))

(defun ahg-short-log-revision-at-point ()
  (let ((node (ewoc-locate ewoc)))
    (and node (car (ewoc-data node)))))

(defun ahg-short-log-view-diff ()
  (interactive)
  (let* ((r1 (ahg-short-log-revision-at-point))
         (r2 (ahg-first-parent-of-rev r1)))
    (ahg-diff r2 r1)))

(defun ahg-short-log-view-diff-select-rev (rev)
  (interactive "sEnter revision to compare against: ")
  (let ((r1 (ahg-short-log-revision-at-point))
        (r2 rev))
    (ahg-diff r2 r1)))

(defun ahg-short-log-view-details ()
  "View details of the given revision."
  (interactive)
  (let ((rev (ahg-short-log-revision-at-point)))
    (ahg-log rev nil)))


(defun ahg-short-log-view-details-mouse (event)
  (interactive "e")
  (save-window-excursion
    (select-window (posn-window (event-end event)) t)
    (goto-char (posn-point (event-end event)))
    (ahg-short-log-view-details)))


(defun ahg-short-log-view-diff-mouse (event)
  (interactive "e")
  (save-window-excursion
    (select-window (posn-window (event-end event)) t)
    (goto-char (posn-point (event-end event)))
    (ahg-short-log-view-diff)))


(defun ahg-short-log-goto-revision (rev)
  "Move point to the revision REV. If REV is not found in the log buffer,
do nothing."
  (interactive "P")
  (when (interactive-p)
    (unless rev
      (setq rev (read-string "Goto revision: "))))
  (let ((n (ewoc-nth ewoc 0)))
    (while (and n (not (string= rev (car (ewoc-data n)))))
      (setq n (ewoc-next ewoc n)))
    (when n (ewoc-goto-node ewoc n))))


(defun ahg-short-log-update-to-rev ()
  (interactive)
  (let ((ahg-update-to-rev-get-revision-function
         'ahg-short-log-revision-at-point))
    (call-interactively 'ahg-update-to-rev)))


(defun ahg-args-add-revs (r1 r2 &optional disjoint)
  (let (command-list)
    (when r1
      (when (numberp r1)
        (setq r1 (number-to-string r1))))
    (when r2
      (when (numberp r2)
        (setq r2 (number-to-string r2))))
    (cond ((and r1 r2 (> (length r1) 0) (> (length r2) 0))
           (setq command-list
                 (if disjoint
                     (append command-list (list "-r" r1 "-r" r2))
                   (append command-list (list "-r" (format "%s:%s" r1 r2))))))
          ((and r1 (> (length r1) 0)) (setq command-list
                                 (append command-list (list "-r" r1))))
          ((and r2 (> (length r2) 0)) (setq command-list
                                            (append command-list
                                                    (list "-r" r2)))))
    command-list))


;; helper function used by ahg-short-log, ahg-log and ahg-log-cur-file to
;; get arguments from the user
(defvar ahg-log-default-revisions '("tip" . "0"))
(defun ahg-log-read-args (is-on-selected-files read-extra-flags)
  (let ((retval
         (append
          (list (read-string
                 (concat "hg log"
                         (if is-on-selected-files " (on selected files)" "")
                         ", R1: ") (car ahg-log-default-revisions))
                (read-string
                 (concat "hg log"
                         (if is-on-selected-files " (on selected files)" "")
                         ", R2: ") (cdr ahg-log-default-revisions)))
          (when read-extra-flags
            (list (read-string
                   (concat "hg log"
                           (if is-on-selected-files " (on selected files)" "")
                           ", extra switches: ") ""))))))
    ;; remember the values entered for the next call
    (setq ahg-log-default-revisions (cons (car retval) (cadr retval)))
    retval))


(defun ahg-short-log-create-ewoc ()
  (let* ((width (window-width (selected-window)))
         (header (concat
                  (propertize "hg log for " 'face ahg-header-line-face)
                  (propertize default-directory 'face ahg-header-line-root-face)
                  "\n\n" (propertize (make-string width ?-) 'face 'bold) "\n"
                  (propertize "    Rev |    Date    |  Author  | Summary\n"
                              'face 'bold)
                  (propertize (make-string width ?-) 'face 'bold)))
         (footer (propertize (make-string width ?-) 'face 'bold))
         (ew (ewoc-create 'ahg-short-log-pp header footer)))
    ew))

(defvar ahg-file-list-for-log-command nil)

(defun ahg-short-log (r1 r2 &optional extra-flags)
  "Run hg log, in a compressed format.
This displays the log in a tabular view, one line per
changeset. The format of each line is: Revision | Date | User | Summary.
R1 and R2 specify the range of revisions to
consider. When run interactively, the user must enter their
values (which default to tip for R1 and 0 for R2). If called with
a prefix argument, prompts also for EXTRA-FLAGS."
  (interactive
   (ahg-log-read-args ahg-file-list-for-log-command current-prefix-arg))  
  (let ((buffer (get-buffer-create
                 (concat "*hg log (summary): " (ahg-root) "*")))
        (command-list (ahg-args-add-revs r1 r2)))  
    (setq command-list
          (append command-list
                  (list "--template"
                        "{rev} {date|shortdate} {author|user} {desc|firstline}\\n")
                  (when extra-flags (split-string extra-flags))))
    (when ahg-file-list-for-log-command
      (setq command-list (append command-list ahg-file-list-for-log-command)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ahg-push-window-configuration)))
    (ahg-generic-command
     "log" command-list
     (lambda (process status)
       (if (string= status "finished\n")
           (with-current-buffer (process-buffer process)
             (pop-to-buffer (current-buffer))
             (ahg-short-log-mode)
             (let ((contents (buffer-substring-no-properties
                              (point-min) (point-max)))
                   (inhibit-read-only t))
               (erase-buffer)
               (setq truncate-lines t)
               (let ((ew (ahg-short-log-create-ewoc)))
                 (ahg-short-log-insert-contents ew contents)
                 (goto-line 6)
                 (toggle-read-only 1)
                 (set (make-local-variable 'ewoc) ew))))
         (ahg-show-error process)))
     buffer)))


(defvar ahg-log-font-lock-keywords
  '(("^hg \\<[a-z]+\\> for" . ahg-header-line-face)
    ("^hg \\<[a-z]+\\> for \\(.*\\)" 1 ahg-header-line-root-face)
    ("^changeset:" . ahg-log-field-face)
    ("^tag:" . ahg-log-field-face)
    ("^user:" . ahg-log-field-face)
    ("^date:" . ahg-log-field-face)
    ("^summary:" . ahg-log-field-face)
    ("^files:" . ahg-log-field-face)
    ("^branch:" . ahg-log-field-face)
    ("^parent:" . ahg-log-field-face)
    ("^description:" . ahg-log-field-face)
    ("^\\(changeset\\|parent\\): +\\(.+\\)$" 2 ahg-log-revision-face)
    ("^\\(tag\\|branch\\): +\\(.+\\)$" 2 ahg-log-branch-face)
    ("^user: +\\(.+\\)$" 1 ahg-short-log-user-face)
    ("^date: +\\(.+\\)$" 1 ahg-short-log-date-face)
    )
  "Keywords in `ahg-log-mode' mode.")

(define-derived-mode ahg-log-mode nil "ahg-log"
  "Major mode to display hg log output.

Commands:
\\{ahg-log-mode-map}
"
  (buffer-disable-undo) ;; undo info not needed here
  (toggle-read-only t)
  (define-key ahg-log-mode-map [?g] 'ahg-log)
  (define-key ahg-log-mode-map [?s] 'ahg-status)
  (define-key ahg-log-mode-map [?=] 'ahg-log-view-diff)
  (define-key ahg-log-mode-map [?D] 'ahg-log-view-diff-select-rev)
  (define-key ahg-log-mode-map [?n] 'ahg-log-next)
  (define-key ahg-log-mode-map "\t" 'ahg-log-next)
  (define-key ahg-log-mode-map [?p] 'ahg-log-previous)
  (define-key ahg-log-mode-map [S-iso-lefttab] 'ahg-log-previous)
  (define-key ahg-log-mode-map [?q] 'ahg-buffer-quit)
  (define-key ahg-log-mode-map [?!] 'ahg-do-command)
  (define-key ahg-log-mode-map [?h] 'ahg-command-help)
  (define-key ahg-log-mode-map "\r" 'ahg-log-update-to-rev)
  (set (make-local-variable 'font-lock-defaults)
       (list 'ahg-log-font-lock-keywords t nil nil))
  (easy-menu-add ahg-log-mode-menu ahg-log-mode-map))

(easy-menu-define ahg-log-mode-menu ahg-log-mode-map "aHg Log"
  '("aHg Log"
    ["View Revision Diff" ahg-log-view-diff [:keys "=" :active t]]
    ["View Revision Diff with Other..." ahg-log-view-diff-select-rev
     [:keys "D" :active t]]
    ["Update to Revision" ahg-log-update-to-rev [:keys "\r" :active t]]
    ["--" nil nil]
    ["Status" ahg-status [:keys "s" :active t]]
    ["Hg Command" ahg-do-command [:keys "!" :active t]]
    ["--" nil nil]
    ["Next Revision" ahg-log-next [:keys "\t" :active t]]
    ["Previous Revision" ahg-log-previous [:keys "p" :active t]]
    ["--" nil nil]
    ["Help on Hg Command" ahg-command-help [:keys "h" :active t]]
    ["--" nil nil]
    ["Refresh" ahg-log [:keys "g" :active t]]
    ["Quit" ahg-buffer-quit [:keys "q" :active t]]
    ))


(defconst ahg-log-start-regexp "^changeset: +\\([0-9]+:[0-9a-f]+\\)")
(defun ahg-log-next (n)
  "Move to the next changeset header of the next diff hunk"
  (interactive "p")
  (end-of-line)
  (re-search-forward ahg-log-start-regexp nil t n)
  (beginning-of-line))

(defun ahg-log-previous (n)
  "Move to the previous changeset header of the previous diff hunk"
  (interactive "p")
  (end-of-line)
  (re-search-backward ahg-log-start-regexp)
  (re-search-backward ahg-log-start-regexp nil t n))

(defun ahg-log-view-diff ()
  (interactive)
  (let* ((r1 (ahg-log-revision-at-point t))
         (r2 (ahg-first-parent-of-rev r1)))
    (ahg-diff r2 r1)))

(defun ahg-log-view-diff-select-rev (rev)
  (interactive "sEnter revision to compare against: ")
  (let ((r1 (ahg-log-revision-at-point t))
        (r2 rev))
    (ahg-diff r2 r1)))

(defun ahg-log-revision-at-point (&optional short-id)
  (save-excursion
    (end-of-line)
    (re-search-backward ahg-log-start-regexp)
    (let ((rev (match-string-no-properties 1)))
      (when rev
        (if short-id (car (split-string rev ":"))
          (cadr (split-string rev ":")))))))


(defun ahg-log-goto-revision (rev)
  "Move point to the revision REV. If REV is not found in the log
buffer, do nothing."
  (let ((rev-pos))
    (save-excursion
      (when
          (re-search-forward (concat "^changeset: +" rev) nil t)
        (setq rev-pos (point))))
    (when rev-pos
      (goto-char rev-pos))))

(defvar ahg-dir-name-for-log-command nil)
  
(defun ahg-log (r1 r2 &optional extra-flags)
  "Run hg log. R1 and R2 specify the range of revisions to
consider. When run interactively, the user must enter their
values (which default to tip for R1 and 0 for R2). If called with
a prefix argument, prompts also for EXTRA-FLAGS."
  (interactive
   (ahg-log-read-args ahg-file-list-for-log-command current-prefix-arg))
  (let ((buffer (get-buffer-create
                 (concat "*hg log (details): " (ahg-root) "*")))
        (command-list (ahg-args-add-revs r1 r2))
        (template "{rev}:{node|short}\\n{branches}\\n{tags}\\n{parents}\\n{author}\\n{date|date}\\n{files}\\n\\t{desc|tabindent}\\n"))  
    (setq command-list (append command-list
                               (list "-v" "--template" template)
                               (when extra-flags (split-string extra-flags))))
    (when ahg-file-list-for-log-command
      (setq command-list (append command-list ahg-file-list-for-log-command)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ahg-push-window-configuration)))
    (ahg-generic-command
     "log" command-list
     (lexical-let ((dn (or ahg-dir-name-for-log-command default-directory)))
       (lambda (process status)
         (if (string= status "finished\n")
             (progn
               (pop-to-buffer (process-buffer process))
               (ahg-format-log-buffer)
               (ahg-log-mode)
               (goto-char (point-min))
               (let ((inhibit-read-only t))
                 (insert
                  (propertize "hg log for " 'face ahg-header-line-face)
                  (propertize dn 'face ahg-header-line-root-face)
                  "\n\n")))
           (ahg-show-error process))))
     buffer)))

(defun ahg-format-log-buffer ()
  (goto-char (point-min))
  (let ((inhibit-read-only t))
    (labels ((next () (beginning-of-line) (forward-line 1)))
      (while (not (eobp))
        ;; first line, changeset
        (insert "changeset:   ")
        (next)
        ;; second line, branch
        (if (looking-at "^$")
            (delete-char 1)
          (insert "branch:      ")
          (next))
        ;; third line, tags
        (if (looking-at "^$")
            (delete-char 1) ;; remove empty line
          ;; otherwise, insert a "tag: " entry for each entry in the list
          (let ((tags (split-string (buffer-substring-no-properties
                                     (point-at-bol) (point-at-eol))))
                (kill-whole-line t))
            (kill-line)
            (mapcar (lambda (tag) (insert "tag:         " tag "\n")) tags)))
        ;; fourth line, parents
        (if (looking-at "^$")
            (delete-char 1) 
          (let ((parents (split-string (buffer-substring-no-properties
                                        (point-at-bol) (point-at-eol))))
                (kill-whole-line t))
            (kill-line)
            (mapcar (lambda (p) (insert "parent:      " p "\n")) parents)))
        ;; fifth line, user
        (insert "user:        ")
        (next)
        ;; sixth line, date
        (insert "date:        ")
        (next)
        ;; seventh line, files
        (insert "files:       ")
        (next)
        ;; rest is the description
        (insert "description:\n")
        ;; each line in the description starts with a '\t'
        (while (and (looking-at "^\\(\t\\|$\\)") (not (eobp)))
          (unless (looking-at "^$")
            (delete-char 1)) ;; remove the \t in front of the line
          (next))
        (insert "\n\n")
        ))))

(defun ahg-log-cur-file (&optional prefix)
  "Shows changelog of the current file. When called interactively
with a prefix argument, prompt for a revision range. If the
prefix argument is the list (16) (corresponding to C-u C-u),
prompts also for extra flags."
  (interactive "P")
  (cond ((eq major-mode 'ahg-status-mode) (call-interactively 'ahg-status-log))
        ((buffer-file-name)
         (let ((ahg-file-list-for-log-command (list (buffer-file-name)))
               (ahg-dir-name-for-log-command (buffer-file-name)))
           (if prefix
               (apply 'ahg-log   
                      (ahg-log-read-args nil (equal current-prefix-arg '(16))))
             (ahg-log "tip" "0"))))
        (t (message "hg log: no file found, aborting."))))

(defun ahg-log-update-to-rev ()
  (interactive)
  (let ((ahg-update-to-rev-get-revision-function 'ahg-log-revision-at-point))
    (call-interactively 'ahg-update-to-rev)))

;;-----------------------------------------------------------------------------
;; graph log
;;-----------------------------------------------------------------------------

(defvar ahg-glog-font-lock-keywords
  '(("^hg revision DAG for" . ahg-header-line-face)
    ("^hg revision DAG for \\(.*\\)" 1 ahg-header-line-root-face)
    ("^\\([+|@o\\\\/ -]\\)+" . 'bold)
    ("^\\([+|@o\\\\/ -]\\)+\\([0-9]+\\)" 2 ahg-short-log-revision-face)
    ("^\\([+|@o\\\\/ -]\\)+[0-9]+  \\([0-9a-f]+\\)" 2 ahg-log-revision-face)
    ("^\\([+|@o\\\\/ -]\\)+[0-9]+  [0-9a-f]+  \\([^ ]+\\)"
     2 ahg-short-log-date-face)
    ("^\\([+|@o\\\\/ -]\\)+[0-9]+  [0-9a-f]+  [^ ]+  \\([^ ]+\\)" 2
     ahg-short-log-user-face)
    ("^\\([+|@o\\\\/ -]\\)+[0-9]+  [0-9a-f]+  [^ ]+  [^ ]+  \\(\\[[^]]+\\]\\)"
     2 ahg-log-branch-face) ;; tags
    ("^\\([+|@o\\\\/ -]\\)+[0-9]+  [0-9a-f]+  [^ ]+  [^ ]+  \\(\\[\\]\\)" 2
     ahg-invisible-face) ;; empty tags
    ("^\\([+|@o\\\\/ -]\\)+[0-9]+  [0-9a-f]+  [^ ]+  [^ ]+  [^ ]+ \\(([^)]+)\\)$"
     2 ahg-log-branch-face) ;; branches
    ("^\\([+|@o\\\\/ -]\\)+[0-9]+  [0-9a-f]+  [^ ]+  [^ ]+  [^ ]+ \\(()\\)$" 2
     ahg-invisible-face) ;; empty branches    
    )
  "Keywords in `ahg-glog-mode' mode.")

(define-derived-mode ahg-glog-mode nil "ahg-glog"
  "Major mode to display hg glog output.

Commands:
\\{ahg-glog-mode-map}
"
  (buffer-disable-undo) ;; undo info not needed here
        (setq truncate-lines t)
  (toggle-read-only t)
  (define-key ahg-glog-mode-map [?g] 'ahg-glog)
  (define-key ahg-glog-mode-map [?s] 'ahg-status)
  (define-key ahg-glog-mode-map [?=] 'ahg-glog-view-diff)
  (define-key ahg-glog-mode-map [?D] 'ahg-glog-view-diff-select-rev)
  (define-key ahg-glog-mode-map [?n] 'ahg-glog-next)
  (define-key ahg-glog-mode-map "\t" 'ahg-glog-next)
  (define-key ahg-glog-mode-map [?p] 'ahg-glog-previous)
  (define-key ahg-glog-mode-map [S-iso-lefttab] 'ahg-glog-previous)  
  (define-key ahg-glog-mode-map [?q] 'ahg-buffer-quit)
  (define-key ahg-glog-mode-map [?!] 'ahg-do-command)
  (define-key ahg-glog-mode-map [?h] 'ahg-command-help)
  (define-key ahg-glog-mode-map "\r" 'ahg-glog-update-to-rev)
  (define-key ahg-glog-mode-map [? ] 'ahg-glog-view-details)
  (set (make-local-variable 'font-lock-defaults)
       (list 'ahg-glog-font-lock-keywords t nil nil))
  (set-face-foreground 'ahg-invisible-face (face-background 'default))
  (easy-menu-add ahg-glog-mode-menu ahg-glog-mode-map))

(easy-menu-define ahg-glog-mode-menu ahg-glog-mode-map "aHg Rev DAG"
  '("aHg Rev DAG"
    ["View Revision Details" ahg-glog-view-details [:keys " " :active t]]
    ["View Revision Diff" ahg-glog-view-diff [:keys "=" :active t]]
    ["View Revision Diff with Other..." ahg-glog-view-diff-select-rev
     [:keys "D" :active t]]
    ["Update to Revision" ahg-glog-update-to-rev [:keys "\r" :active t]]
    ["--" nil nil]
    ["Status" ahg-status [:keys "s" :active t]]
    ["Hg Command" ahg-do-command [:keys "!" :active t]]
    ["--" nil nil]
    ["Next Revision" ahg-glog-next [:keys "\t" :active t]]
    ["Previous Revision" ahg-glog-previous [:keys "p" :active t]]
    ["--" nil nil]
    ["Help on Hg Command" ahg-command-help [:keys "h" :active t]]
    ["--" nil nil]
    ["Refresh" ahg-glog [:keys "g" :active t]]
    ["Quit" ahg-buffer-quit [:keys "q" :active t]]
    ))


(defconst ahg-glog-start-regexp "^\\([+|@o\\\\/ -]\\)+\\([0-9]+\\)")

(defun ahg-glog-next (n)
  (interactive "p")
  (end-of-line)
  (re-search-forward ahg-glog-start-regexp nil t n)
  (beginning-of-line))

(defun ahg-glog-previous (n)
  (interactive "p")
  (end-of-line)
  (re-search-backward ahg-glog-start-regexp)
  (re-search-backward ahg-glog-start-regexp nil t n))

(defun ahg-glog-view-diff ()
  (interactive)
  (let* ((r1 (ahg-glog-revision-at-point))
         (r2 (ahg-first-parent-of-rev r1)))
    (ahg-diff r2 r1)))

(defun ahg-glog-view-diff-select-rev (rev)
  (interactive "sEnter revision to compare against: ")
  (let ((r1 (ahg-glog-revision-at-point))
        (r2 rev))
    (ahg-diff r2 r1)))

(defun ahg-glog-revision-at-point ()
  (save-excursion
    (end-of-line)
    (re-search-backward ahg-glog-start-regexp)
    (match-string-no-properties 2)))

(defun ahg-glog-view-details ()
  "View details of the given revision."
  (interactive)
  (let ((rev (ahg-glog-revision-at-point)))
    (ahg-log rev nil)))
  
(defun ahg-glog (r1 r2 &optional extra-flags)
  "Run hg glog. R1 and R2 specify the range of revisions to
consider. When run interactively, the user must enter their
values (which default to tip for R1 and 0 for R2). If called with
a prefix argument, prompts also for EXTRA-FLAGS."
  (interactive
   (ahg-log-read-args ahg-file-list-for-log-command current-prefix-arg))
  (let ((buffer (get-buffer-create
                 (concat "*hg glog: " (ahg-root) "*")))
        (command-list (ahg-args-add-revs r1 r2)))  
    (setq command-list
          (append command-list
                  (list "--template"
                        "{rev}  {node|short}  {date|shortdate}  {author|user}  [{tags}] ({branches})\\n  {desc|firstline}\\n\\n")
                  (when extra-flags (split-string extra-flags))))
    (when ahg-file-list-for-log-command
      (setq command-list (append command-list ahg-file-list-for-log-command)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ahg-push-window-configuration)))
    (ahg-generic-command
     "glog" command-list
     (lexical-let ((dn (or ahg-dir-name-for-log-command default-directory)))
       (lambda (process status)
         (if (string= status "finished\n")
             (progn
               (pop-to-buffer (process-buffer process))
               (ahg-glog-mode)
               (goto-char (point-min))
               (let ((inhibit-read-only t))
                 (insert
                  (propertize "hg revision DAG for " 'face ahg-header-line-face)
                  (propertize dn 'face ahg-header-line-root-face)
                  "\n\n")))
           (ahg-show-error process))))
     buffer)))

(defun ahg-glog-update-to-rev ()
  (interactive)
  (let ((ahg-update-to-rev-get-revision-function 'ahg-glog-revision-at-point))
    (call-interactively 'ahg-update-to-rev)))

;;-----------------------------------------------------------------------------
;; hg diff
;;-----------------------------------------------------------------------------

(define-derived-mode ahg-diff-mode diff-mode "aHg Diff"
  "Special Diff mode for aHg buffers.

Commands:
\\{ahg-diff-mode-map}
"
  (toggle-read-only t)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((fs (or (diff-hunk-file-names) (diff-hunk-file-names t)))
          goodpath)
      (when fs
        (dolist (p fs goodpath)
          (unless goodpath
            (when (string-match "^\\(a\\|b\\)/" p)
              (setq goodpath (substring p (match-end 0))))))
        (when goodpath
          (let ((hint (concat (ahg-root) "/" goodpath)))
            (set (make-local-variable 'diff-remembered-defdir)
                 default-directory)
            (diff-tell-file-name nil hint)))
        )))
  (define-key ahg-diff-mode-map "q" 'ahg-buffer-quit)
  (easy-menu-add-item nil '("Diff") '["--" nil nil])
  (easy-menu-add-item nil '("Diff") '["Quit" ahg-buffer-quit
                                      [:keys "q" :active t]]))


(defun ahg-diff (&optional r1 r2 files)
  (interactive "P")
  (when (interactive-p)
    (unless r1
      (setq r1 (read-string "hg diff, R1: " "tip"))
      (setq r2 (read-string "hg diff, R2: " ""))))
  (let ((buffer (get-buffer-create "*aHg diff*"))
        (command-list (ahg-args-add-revs r1 r2 t))
        (curdir default-directory))
    (with-current-buffer buffer
      (setq default-directory (file-name-as-directory curdir)))
    (when ahg-diff-use-git-format
      (setq command-list (cons "--git" command-list)))
    (when files
      (setq command-list (append command-list files)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ahg-push-window-configuration)))
    (ahg-generic-command "diff" command-list
                         (lambda (process status)
                           (if (string= status "finished\n")
                               (progn
                                 (pop-to-buffer (process-buffer process))
                                 (ahg-diff-mode)
                                 (goto-char (point-min)))
                             (ahg-show-error process)))
                         buffer)))

(defun ahg-diff-cur-file (ask-other-rev)
  (interactive "P")
  (cond ((eq major-mode 'ahg-status-mode) (call-interactively 'ahg-status-diff))
        ((eq major-mode 'ahg-short-log-mode)
         (call-interactively 'ahg-short-log-view-diff))
        ((eq major-mode 'ahg-log-mode) (call-interactively 'ahg-log-view-diff))
        ((buffer-file-name)
         (ahg-diff (when ask-other-rev
                     (read-string "Enter revision to compare against: "))
                   nil (list (buffer-file-name))))
        (t (message "hg diff: no file found, aborting."))))

;;-----------------------------------------------------------------------------
;; hg command
;;-----------------------------------------------------------------------------

(defun ahg-complete-command-name (command)
  (with-temp-buffer
    (let ((process-environment (cons "LANG=" process-environment)))
      (if (= (ahg-call-process "debugcomplete") 0)
          ;; first, we try the built-in autocompletion of mercurial
          (let (out)
            (goto-char (point-min))
            (while (not (eobp))
              (setq out
                    (cons
                     (buffer-substring-no-properties
                      (point-at-bol) (point-at-eol))
                     out))
              (forward-line 1))
            (if out (nreverse out) (list command)))
        ;; if this fails, we return the string itself as a match
        (list command)))))

(defvar ahg-complete-command-directory-for-filename-completion nil)

(defun ahg-complete-command (command)
  ;; we split the string, and treat the last word as a filename
  (let* ((idx (string-match "\\([^ ]+\\)$" command))
         (matches
          (cond ((= idx 0) (ahg-complete-command-name command))
                ((and (= idx 5) (string= (substring command 0 idx) "help "))
                 (ahg-complete-command-name (substring command idx)))
                (t
                 (let ((default-directory
                         (or
                          ahg-complete-command-directory-for-filename-completion
                          default-directory)))
                   (mapcar
                    (lambda (n)
                      (if (file-directory-p n) (file-name-as-directory n) n))
                    (file-expand-wildcards
                     (concat (substring command idx) "*")))))))
         (prev (substring command 0 idx)))
    (mapcar (function (lambda (a) (concat prev a))) matches)))


(defvar ahg-do-command-prompt "Hg command: ")
(defvar ahg-do-command-extra-args nil)

(defun ahg-do-command (cmdstring)
  "Execute an arbitrary hg command CMDSTRING.
CMDSTRING should not include 'hg' as first argument, as this is
implicit.

When run from an *aHg-status* buffer, the selected files are
passed as extra arguments. In this case, if there is a `*' in
CMDSTRING, surrounded by whitespace, the list of files is
substituted there. Otherwise, the list of files is appended to
CMDSTRING.

With a non-nil prefix arg, upon successful completion the status
buffer is automatically refreshed. (If called from an MQ patches buffer,
that buffer is refreshed instead.)"
  (interactive (list
                (let ((minibuffer-local-completion-map
                       (copy-keymap minibuffer-local-map)))
                  (define-key minibuffer-local-completion-map "\t"
                    'minibuffer-complete)
                  (completing-read ahg-do-command-prompt
                                   (ahg-dynamic-completion-table
                                    ahg-complete-command)))))
  (let* ((args (split-string cmdstring " "))
         (cmdname (car args))
         (cmdargs (mapconcat 'identity (cdr args) " "))
         (interpolate (member "*" (cdr args)))
         (buffer (get-buffer-create (concat "*hg command: "
                                            (ahg-root) "*")))
         (curdir default-directory)
         (should-refresh current-prefix-arg))
    (when ahg-do-command-extra-args
      (let ((extra (mapconcat 'identity ahg-do-command-extra-args " ")))
        (setq cmdargs
              (if interpolate
                  (replace-regexp-in-string
                   "\\([*]\\).*\\'" extra cmdargs nil nil 1)
                (concat cmdargs " " extra)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (ahg-command-mode)
        (erase-buffer)
        (setq default-directory (file-name-as-directory curdir))
        (ahg-push-window-configuration)))
    (when ahg-do-command-show-buffer-immediately
      (pop-to-buffer buffer))
    (when ahg-do-command-insert-header
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert
           (propertize
            (concat "output of 'hg " cmdname " " cmdargs "' on ")
            'face ahg-header-line-face)
           (propertize default-directory 'face ahg-header-line-root-face)
           "\n" (make-string (1- (window-width (selected-window))) ?-)
           "\n\n"))))
    (ahg-generic-command
     cmdname (list cmdargs)
     (lexical-let ((should-refresh should-refresh)
                   (aroot (ahg-root))
                   (is-mq (eq major-mode 'ahg-mq-patches-mode)))
       (lambda (process status)
         (if (string= status "finished\n")
             (progn
               (when should-refresh
                 (if is-mq
                     (ahg-mq-patches-maybe-refresh aroot)
                   (ahg-status-maybe-refresh aroot)))
               (pop-to-buffer (current-buffer))
               (goto-char (point-min)))
           (ahg-show-error process))))
     buffer t ;; use-shell
            nil ;; no-show-message
            t ;; report-untrusted
            )))

;;-----------------------------------------------------------------------------
;; hg help
;;-----------------------------------------------------------------------------

(defun ahg-command-help (command)
  (interactive
   (list (completing-read "Help on hg command: "
                          (ahg-dynamic-completion-table
                           ahg-complete-command-name))))
  (let ((buffer (get-buffer-create "*hg help*")))
    (with-current-buffer buffer (let ((inhibit-read-only t)) (erase-buffer)))
    (ahg-generic-command
     "help" (list command)
     (lambda (process status)
       (if (string= status "finished\n")
           (progn
             (pop-to-buffer (process-buffer process))
             (help-mode)
             (local-set-key "!" 'ahg-do-command)
             (let ((mymap (copy-keymap
                           (cdr (assq 'view-mode minor-mode-map-alist)))))
               (define-key mymap "h" 'ahg-command-help)
               (add-to-list 'minor-mode-overriding-map-alist
                            (cons 'view-mode mymap)))
             (goto-char (point-min)))
         (ahg-show-error process)))
     buffer)))

;;-----------------------------------------------------------------------------
;; MQ support
;;-----------------------------------------------------------------------------

(defun ahg-mq-log-callback (cmdname &optional extraargs)
  "Callback function to edit log messages for mq commands."
  (interactive)
  (let ((msg (ahg-parse-commit-message)))
    (let ((args (append (list "-m" msg)
                        extraargs
                        (log-edit-files))))
      (ahg-generic-command
       cmdname args
       (lexical-let ((aroot (ahg-root))
                     (n (length (log-edit-files)))
                     (cmdn cmdname))
         (lambda (process status)
           (if (string= status "finished\n")
               (progn
                 (ahg-status-maybe-refresh aroot)
                 (message "mq command %s successful." cmdn)
                 (kill-buffer (process-buffer process)))
             (ahg-show-error process)))))))
  (kill-buffer (current-buffer)))

(defun ahg-complete-mq-patch-name (patchname)
  "Function to complete patch names, according to the result of the
hg qseries command."
  (with-temp-buffer
    (let ((process-environment (cons "LANG=" process-environment))) 
      (if (= (ahg-call-process "qseries") 0)
          (let (out)
            (goto-char (point-min))
            (while (not (or (looking-at "^$") (eobp)))
              (let* ((curname (buffer-substring-no-properties
                               (point-at-bol) (point-at-eol)))
                     (ok (compare-strings patchname 0 nil curname 0 nil)))
                (when (or (eq ok t) (= (- ok) (1+ (length patchname))))
                  (setq out (cons curname out)))
                (beginning-of-line)
                (forward-line 1)))
            (nreverse out))
        ;; here, we silently ignore errors, and return an empty completion
        nil))))

(defun ahg-qnew (patchname force edit-log-message)
  "Create a new mq patch PATCHNAME. If FORCE is non-nil, use the -f switch.
If EDIT-LOG-MESSAGE is non-nil, pop a buffer to enter a commit
message to use instead of the default one. When called
interactively, the name of the patch and the FORCE flag are read
from the minibuffer, and EDIT-LOG-MESSAGE is non-nil only if
called with a prefix argument. If FORCE is true and the function
is called interactively from a aHg status buffer, only the
selected files will be incorporated into the patch."
  (interactive
   (list (read-string "Patch name: ")
         (and (ahg-uncommitted-changes-p)
              (ahg-y-or-n-p "Import outstanding changes into patch? "))
         current-prefix-arg))
  (let ((files (when (eq major-mode 'ahg-status-mode)
                 (mapcar 'cddr (ahg-status-get-marked nil)))))
    (if edit-log-message
        (ahg-log-edit
         (lexical-let ((force force))
           (lambda () (interactive)
             (ahg-mq-log-callback "qnew" (when force (list "-f")))))
         (lexical-let ((flist (cons patchname files))) (lambda () flist))
         (generate-new-buffer "*aHg-log*"))
      ;; else
      (ahg-generic-command
       "qnew" (append (when force (list "-f")) (list patchname) files)
       (lexical-let ((aroot (ahg-root)))
         (lambda (process status)
           (if (string= status "finished\n")
               (progn
                 (ahg-status-maybe-refresh aroot)
                 (ahg-mq-patches-maybe-refresh aroot)
                 (message "mq command qnew successful.")
                 (kill-buffer (process-buffer process)))
             (ahg-show-error process)))))
       )))

(defun ahg-qrefresh (get-log-message)
  "Refreshes the current mq patch. If GET-LOG-MESSAGE is non-nil,
a buffer will pop up to enter the commit message. When called
interactively, GET-LOG-MESSAGE is non-nil only if called with a
prefix arg. If called interactively from a aHg status buffer,
only the selected files will be refreshed."
  (interactive "P")
  (let ((buf (when get-log-message (generate-new-buffer "*aHg-log*")))
        (files (when (eq major-mode 'ahg-status-mode)
                 (mapcar 'cddr (ahg-status-get-marked nil)))))
    (if get-log-message
        (let* ((patchname
                (with-temp-buffer
                  (when (= (ahg-call-process "qtop") 0)
                    (buffer-substring-no-properties
                     (point-min) (1- (point-max))))))
               (msg (when patchname
                      (concat "Refreshing mq patch: " patchname)))
               (content
                (with-temp-buffer
                  (when
                      (= (if patchname
                             (ahg-call-process "qheader" (list patchname))
                           (ahg-call-process "qheader")) 0)
                    (buffer-substring-no-properties
                     (point-min) (1- (point-max)))))))
          (ahg-log-edit
           (lambda () (interactive) (ahg-mq-log-callback "qrefresh"))
           (lexical-let ((flist files)) (lambda () flist))
           buf msg content))
      (ahg-generic-command
       "qrefresh" files
       (lexical-let ((aroot (ahg-root)))
         (lambda (process status)
           (if (string= status "finished\n")
               (progn
                 (ahg-status-maybe-refresh aroot)
;;                 (ahg-mq-patches-maybe-refresh aroot)
                 (message "mq command qrefresh successful.")
                 (kill-buffer (process-buffer process)))
             (ahg-show-error process))))))))

(defun ahg-qgoto (patchname force)
  "Puts the given mq patch PATCHNAME on the top of the stack. If
FORCE is non-nil, discard local changes (passing -f to hg). When
called interactively, PATCHNAME and FORCE are read from the minibuffer.
"
  (interactive
   (list (completing-read
          "Go to patch: "
          (ahg-dynamic-completion-table ahg-complete-mq-patch-name))
         (and (ahg-uncommitted-changes-p)
              (ahg-y-or-n-p "Overwrite local changes? "))))
  (let ((args (if force (list "-f" patchname) (list patchname))))
    (ahg-generic-command
     "qgoto" args
     (lexical-let ((aroot (ahg-root)))
       (lambda (process status)
         (if (string= status "finished\n")
             (progn
               (ahg-status-maybe-refresh aroot)
               (ahg-mq-patches-maybe-refresh aroot)
               (let ((msg
                      (with-current-buffer (process-buffer process)
                        (goto-char (point-max))
                        (forward-char -1)
                        (beginning-of-line)
                        (buffer-substring-no-properties
                         (point-at-bol) (point-at-eol)))))
                 (message msg))
               (kill-buffer (process-buffer process)))
           (ahg-show-error process)))))))

(defun ahg-qpop-all (force)
  "Pops all patches off the mq stack. If FORCE is non-nil,
discards any local changes. When called interactively, FORCE is
read from the minibuffer.
"
  (interactive
   (list (and (ahg-uncommitted-changes-p)
              (ahg-y-or-n-p "Forget local changes? "))))
  (let ((args (if force (list "-f" "-a") (list "-a"))))
    (ahg-generic-command
     "qpop" args
     (lexical-let ((aroot (ahg-root)))
       (lambda (process status)
         (if (string= status "finished\n")
             (progn
               (ahg-status-maybe-refresh aroot)
               (ahg-mq-patches-maybe-refresh aroot)
               (let ((msg
                      (with-current-buffer (process-buffer process)
                        (goto-char (point-max))
                        (forward-char -1)
                        (beginning-of-line)
                        (buffer-substring-no-properties
                         (point-at-bol) (point-at-eol)))))
                 (message msg))
               (kill-buffer (process-buffer process)))
           (ahg-show-error process)))))))

(defun ahg-qtop ()
  "Shows the name of the mq patch currently at the top of the stack."
  (interactive)
  (ahg-generic-command
   "qtop" nil
   (lambda (process status)
     (if (string= status "finished\n")
         (progn
           (let ((msg
                  (with-current-buffer (process-buffer process)
                    (goto-char (point-max))
                    (forward-char -1)
                    (beginning-of-line)
                    (buffer-substring-no-properties
                     (point-at-bol) (point-at-eol)))))
             (message msg))
           (kill-buffer (process-buffer process)))
       (ahg-show-error process)))))

(defun ahg-qdelete (patchname)
  "Deletes the given patch PATCHNAME. When called interactively,
read the name from the minibuffer."
  (interactive
   (list
    (completing-read
     "Delete patch: "
     (ahg-dynamic-completion-table ahg-complete-mq-patch-name))))
  (ahg-generic-command
   "qdelete" (list patchname)
   (lexical-let ((patchname patchname)
                 (aroot (ahg-root)))
     (lambda (process status)
       (if (string= status "finished\n")
           (progn
             (ahg-mq-patches-maybe-refresh aroot)
             (message "Deleted mq patch %s" patchname)
             (kill-buffer (process-buffer process)))
         (ahg-show-error process))))))


(defun ahg-mq-convert-patch-to-changeset-callback ()
  (interactive)
  (ahg-generic-command ;; first, we refresh the patch with the new log message
   "qrefresh" (list "-m" (ahg-parse-commit-message))
   (lexical-let ((log-buffer (current-buffer)))
     (lambda (process status)
       (kill-buffer log-buffer)
       (if (string= status "finished\n")
           (progn 
             (ahg-generic-command ;; if successful, we then try to convert it to
                                  ;; a regular changeset.
              "qdelete" (list "--rev" "tip")
              (lexical-let ((aroot (ahg-root)))
                (lambda (process status)
                  (if (string= status "finished\n")
                      (progn
                        (ahg-status-maybe-refresh aroot)
                        (ahg-mq-patches-maybe-refresh aroot)
                        (kill-buffer (process-buffer process)))
                    ;; note that if this second command fails, we still have
                    ;; changed the log message... This is not nice, but at the
                    ;; moment I don't know how to fix it
                    (ahg-show-error process)))))
             (kill-buffer (process-buffer process)))
         (ahg-show-error process))))))

(defun ahg-mq-convert-patch-to-changeset ()
  "Tell mq to stop managing the current patch and convert it to a regular
mercurial changeset. The patch must be applied and at the base of the stack.
Pops a buffer for entering the commit message."
  (interactive)
  (let* ((buf (generate-new-buffer "*aHg-log*"))
         (patchname
          (with-temp-buffer
            (when (= (ahg-call-process "qtop") 0)
              (buffer-substring-no-properties
               (point-min) (1- (point-max))))))
         (msg (when patchname
                (concat "Converting mq patch: " patchname
                        " to regular changeset")))
         (content
          (with-temp-buffer
            (when (= (if patchname
                         (ahg-call-process "qheader" (list patchname))
                       (ahg-call-process "qheader")) 0)
              (buffer-substring-no-properties (point-min) (1- (point-max)))))))
    (ahg-log-edit
     'ahg-mq-convert-patch-to-changeset-callback
     (lambda () nil)
     buf
     msg content)))


(defun ahg-qdiff (files)
  "Shows a diff which includes the current mq patch as well as any
changes which have been made in the working directory since the
last refresh."
  (interactive (list (when (eq major-mode 'ahg-status-mode) 
                       (mapcar 'cddr (ahg-status-get-marked nil)))))
  (let ((buf (get-buffer-create "*aHg diff*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ahg-push-window-configuration)))
    (ahg-generic-command
     "qdiff" (if ahg-diff-use-git-format (append (list "--git") files) files)
     (lexical-let ((aroot (file-name-as-directory (ahg-root))))
       (lambda (process status)
         (if (string= status "finished\n")
             (progn
               (pop-to-buffer (process-buffer process))
               (setq default-directory aroot)
               (ahg-diff-mode)
               (goto-char (point-min)))
           (ahg-show-error process))))
     buf)))


(defun ahg-mq-edit-series ()
  (interactive)
  ;; first, check whether there is any patch applied. If so, ask the user
  ;; whethe (s)he wants to pop all patches before editing series
  (let* ((some-patches-applied
         (with-temp-buffer
           (when (= (ahg-call-process "tip" (list "--template" "{tags}")) 0)
             (let ((tags (split-string (buffer-string))))
               (member "qtip" tags)))))
         (pop (and some-patches-applied
                   (ahg-y-or-n-p "Pop all patches before editing series? ")))
         (edit-series (lambda (root)
                        (find-file-other-window
                         (concat (file-name-as-directory root)
                                 ".hg/patches/series")))))
      (if pop
          (ahg-generic-command
           "qpop" (list "--all")
           (lexical-let ((aroot (ahg-root))
                         (edit-series edit-series))
             (lambda (process status)
               (if (string= status "finished\n")
                   (progn
                     (ahg-status-maybe-refresh aroot)
                     (ahg-mq-patches-maybe-refresh aroot)
                     (funcall edit-series aroot)
                     (kill-buffer (process-buffer process)))
                 (ahg-show-error process)))))
        (funcall edit-series (ahg-root)))))


(defvar ahg-mq-patches-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q] 'ahg-buffer-quit)
    (define-key map [?g] 'ahg-mq-patch-list-refresh)
    (define-key map [?D] 'ahg-mq-patches-delete-patch)
    (define-key map [?!] 'ahg-mq-do-command)
    (define-key map [?h] 'ahg-command-help)
    (define-key map [?=] 'ahg-mq-patches-view-patch)
    (define-key map [?\r] 'ahg-mq-patches-goto-patch)
    (define-key map [?p] 'ahg-qpop-all)
    (define-key map [?n] 'ahg-qnew)
    (define-key map [?e] 'ahg-mq-edit-series)
    (define-key map [?f] 'ahg-mq-patches-convert-patch-to-changeset)
    (define-key map [?r] 'ahg-mq-patches-qrefresh)
    map)
  "Keymap used in `ahg-mq-patches-mode'.")

(defvar ahg-mq-patches-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ahg-mq-patches-view-patch-mouse)
    (define-key map [mouse-2] 'ahg-mq-patches-goto-patch-mouse)
    map))

(easy-menu-define ahg-mq-patches-mode-menu ahg-mq-patches-mode-map
  "aHg MQ Patches"
  '("aHg MQ Patches"
    ["View Patch" ahg-mq-patches-view-patch [:keys "=" :active t]]
    ["Go to Patch" ahg-mq-patches-goto-patch [:keys "\r" :active t]]
    ["New Patch..." ahg-qnew [:keys "n" :active t]]
    ["Delete Patch" ahg-mq-patches-delete-patch [:keys "D" :active t]]
    ["Pop All Patches" ahg-qpop-all [:keys "p" :active t]]
    ["Edit series File" ahg-mq-edit-series [:keys "e" :active t]]
    ["--" nil nil]
    ["Refresh Current Patch" ahg-mq-patches-qrefresh [:keys "r" :active t]]
    ["Convert Current Patch to Changeset"
     ahg-mq-patches-convert-patch-to-changeset [:keys "f" :active t]]
    ["--" nil nil]
    ["Hg Command" ahg-mq-do-command [:keys "!" :active t]]
    ["Help on Hg Command" ahg-command-help [:keys "h" :active t]]
    ["--" nil nil]
    ["Refresh" ahg-mq-patch-list-refresh [:keys "g" :active t]]
    ["Quit" ahg-buffer-quit [:keys "q" :active t]]
    ))

(define-derived-mode ahg-mq-patches-mode fundamental-mode "ahg-mq-patches"
  "Major mode to display mq patch queues.

Commands:
\\{ahg-mq-patches-mode-map}
"
  (buffer-disable-undo) ;; undo info not needed here
  (use-local-map ahg-mq-patches-mode-map)
  (font-lock-mode nil)
  ;;(hl-line-mode t)
  (setq truncate-lines t)
  (toggle-read-only t)
  (easy-menu-add ahg-mq-patches-mode-menu ahg-mq-patches-mode-map)
  )

(defun ahg-mq-patch-pp (data)
  "Pretty-printer for mq patches patch list."
  ;; data is a 4-elements list: index, applied, patch name, guards
  (let* ((s (format "% 6d |  %s  | %s %s" (car data)
                   (if (cadr data) "*" " ") (caddr data)
                   (if (not (string= (car (cadddr data)) "unguarded"))
                       (cadddr data) "")))
         (width (window-width (selected-window)))
         (pad (if (< (length s) width)
                  (make-string (- width (length s)) ? ) "")))
    (insert (propertize (concat s pad)
                        'mouse-face 'highlight
                        'keymap ahg-mq-patches-line-map))))

(defun ahg-mq-patches-insert-contents (ewoc patches applied guards)
  (let ((idx 0))
    (mapcar
     (lambda (patch)
       (when (not (string-match patch "[ \t]+"))
         (let ((data (list idx
                           (member patch applied)
                           patch
                           (cdr (assoc patch guards)))))
           (setq idx (1+ idx))
           (ewoc-enter-last ewoc data))))
     patches)))

(defun ahg-mq-patches-create-ewoc ()
  (let* ((width (window-width (selected-window)))
         (r (propertize (make-string width ?-) 'face 'bold))
         (header (concat
                  (propertize "mq patch queue for " 'face ahg-header-line-face)
                  (propertize default-directory 'face ahg-header-line-root-face)
                  "\n\n" r "\n"
                  (propertize " Index | App | Patch (Guards)\n" 'face 'bold) r))
         (footer r)
         (ew (ewoc-create 'ahg-mq-patch-pp header footer)))
    ew))

(defun ahg-mq-show-patches-buffer (buf patches applied guards curdir no-pop
                                       point-pos)
  (with-current-buffer buf
    (ahg-mq-patches-mode)    
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq default-directory (file-name-as-directory curdir))
      (ahg-push-window-configuration)
      (goto-char (point-min))
      (let ((ew (ahg-mq-patches-create-ewoc)))
        (ahg-mq-patches-insert-contents ew patches applied guards)
        (set (make-local-variable 'ewoc) ew)))
      (toggle-read-only t)
      (if point-pos
          (ahg-goto-line-point point-pos)
        (goto-char (point-min))
        (forward-line 1))
      (set-buffer-modified-p nil)
      (message " "))
  (unless no-pop
    (pop-to-buffer buf)))


(defun ahg-mq-get-patches-buffer (root &optional dont-create)
  (let* ((name (format "*aHg mq patches for: %s*" root))
         (buf (if dont-create (get-buffer name) (get-buffer-create name))))
    (when buf
      (with-current-buffer buf
        (setq default-directory (file-name-as-directory root))))
    buf))


(defvar ahg-mq-list-patches-no-pop nil)
(defvar ahg-mq-patches-buffer-point nil)

(defun ahg-mq-list-patches (&optional root)
  "List all mq patches in the queue, showing also information
about which are currently applied."
  (interactive)
  (unless root (setq root (ahg-root)))
  (let ((buf (ahg-mq-get-patches-buffer root))
        (msg (when (interactive-p)
               (format "aHg: getting patch queue for %s..." root)))
        (oldcolumns (getenv "COLUMNS")))
    (when msg (message msg))
    (setenv "COLUMNS" "100000")
    (ahg-generic-command
     "qseries" nil
     (lexical-let ((buf buf)
                   (curdir default-directory)
                   (aroot root)
                   (no-pop ahg-mq-list-patches-no-pop)
                   (point-pos ahg-mq-patches-buffer-point)
                   (msg msg))
       (lambda (process status) ;; parse output of hg qseries
         (if (string= status "finished\n")
             (let ((patches
                    (with-current-buffer (process-buffer process)
                      (split-string (buffer-string) "\n"))))
               (kill-buffer (process-buffer process))
               (setenv "COLUMNS" "100000")
               (ahg-generic-command
                "qapplied" nil
                (lexical-let ((buf buf)
                              (patches patches)
                              (curdir curdir)
                              (no-pop no-pop)
                              (point-pos point-pos)
                              (msg msg))
                  (lambda (process status) ;; parse output of hg qapplied
                    (if (string= status "finished\n")
                        (let ((applied
                               (with-current-buffer (process-buffer process)
                                 (split-string (buffer-string) "\n"))))
                          (kill-buffer (process-buffer process))
                          ;; now, list guards as well
                          (ahg-generic-command
                           "qguard" (list "-l")
                           (lexical-let ((buf buf)
                                         (patches patches)
                                         (applied applied)
                                         (curdir curdir)
                                         (no-pop no-pop)
                                         (point-pos point-pos)
                                         (msg msg))
                             (lambda (process status)
                               (if (string= status "finished\n")
                                   (let
                                       ((guards
                                         (with-current-buffer
                                             (process-buffer process)
                                           (mapcar
                                            (lambda (s) (split-string s ": "))
                                            (split-string
                                             (buffer-string) "\n")))))
                                     (kill-buffer (process-buffer process))
                                     ;; and show the buffer
                                     (ahg-mq-show-patches-buffer
                                      buf patches applied guards curdir
                                      no-pop point-pos)
                                     (when msg (message "%sdone" msg)))
                                 ;; error in hg qguard
                                 (kill-buffer buf)
                                 (ahg-show-error process)))) nil nil t))
                      ;; error in hg qapplied
                      (kill-buffer buf)
                      (ahg-show-error process)))) nil nil t))
           ;; error in hg qseries
           (kill-buffer buf)
           (ahg-show-error process))))
     nil nil t)
    ;; restore the COLUMNS env var
    (setenv "COLUMNS" oldcolumns)
    ))


(defun ahg-mq-patch-list-refresh ()
  (interactive)
  (let ((ahg-mq-patches-buffer-point (ahg-line-point-pos)))
    (call-interactively 'ahg-mq-list-patches)))


(defun ahg-mq-patches-maybe-refresh (root)
  (when ahg-auto-refresh-status-buffer
    (let ((buf (ahg-mq-get-patches-buffer root t))
          (default-directory (file-name-as-directory root)))
      (when buf
        (let ((ahg-mq-list-patches-no-pop t)
              (ahg-mq-patches-buffer-point
               (with-current-buffer buf (ahg-line-point-pos))))
          (ahg-mq-list-patches root))))))


(defun ahg-mq-patches-view-patch ()
  "Display the patch at point in the patch list buffer."
  (interactive)
  (let ((patch (ahg-mq-patches-patch-at-point))
        (root (ahg-root)))
    (when patch
      (find-file-other-window
       (concat (file-name-as-directory root) ".hg/patches/" patch))
      (ahg-diff-mode))))

(defun ahg-mq-patches-patch-at-point ()
  (let ((node (ewoc-locate ewoc)))
    (and node (caddr (ewoc-data node)))))


(defun ahg-mq-patches-goto-patch (force)
  "Puts the patch at point in the patch list buffer on top of the
stack of applied patches."
  (interactive "P")
  (let* ((patch (ahg-mq-patches-patch-at-point))
         (ok (and patch (ahg-y-or-n-p (format "Go to patch %s? " patch)))))
    (when ok
      (ahg-qgoto patch force))))


(defun ahg-mq-patches-view-patch-mouse (event)
  (interactive "e")
  (save-window-excursion
    (select-window (posn-window (event-end event)) t)
    (goto-char (posn-point (event-end event)))
    (ahg-mq-patches-view-patch)))


(defun ahg-mq-patches-goto-patch-mouse (event)
  (interactive "e")
  (save-window-excursion
    (select-window (posn-window (event-end event)) t)
    (goto-char (posn-point (event-end event)))
    (ahg-mq-patches-goto-patch nil)))


(defun ahg-mq-patches-delete-patch ()
  "Deletes the patch at point in the patch list buffer."
  (interactive)
  (let* ((patch (ahg-mq-patches-patch-at-point))
         (ok (and patch (ahg-y-or-n-p (format "Delete patch %s? " patch)))))
    (when ok
      (ahg-qdelete patch))))


(defun ahg-mq-get-current-patch ()
  (with-temp-buffer
    (when (= (ahg-call-process "qtop") 0)
      (buffer-substring-no-properties (point-min) (1- (point-max))))))

(defun ahg-mq-patches-qrefresh (get-log-message)
  (interactive "P")
  (let ((curpatch (ahg-mq-get-current-patch)))
    (if curpatch
        (and (ahg-y-or-n-p (format "Refresh current patch (%s)? " curpatch))
             (ahg-qrefresh get-log-message))
      (message "aHg Error: unable to determine current patch!"))))


(defun ahg-mq-patches-convert-patch-to-changeset ()
  (interactive)
  (let ((curpatch (ahg-mq-get-current-patch)))
    (if curpatch
        (and (ahg-y-or-n-p (format "Convert current patch (%s) to changeset? "
                                   curpatch))
             (ahg-mq-convert-patch-to-changeset))
      (message "aHg Error: unable to determine current patch!"))))


(defun ahg-mq-do-command ()
  "Call `ahg-do-command', but set `default-directory' to the MQ patches dir,
so that filename completion works on patch names."
  (interactive)
  (let ((ahg-complete-command-directory-for-filename-completion
         (concat (file-name-as-directory (ahg-root)) ".hg/patches/")))
    (call-interactively 'ahg-do-command)))

;;-----------------------------------------------------------------------------
;; Various helper functions
;;-----------------------------------------------------------------------------

(defun ahg-first-parent-of-rev (rev)
  (with-temp-buffer
    (let ((process-environment (cons "LANG=" process-environment)))
      (if (= (ahg-call-process "parents"
                               (list "-r" rev "--template" "{rev}")) 0)
          (buffer-string)
        (if (string-to-number rev)
            (number-to-string (1- (string-to-number rev)))
          0)))))

(defun ahg-buffer-quit ()
  (interactive)
  (let ((buf (current-buffer)))
    (ahg-pop-window-configuration)
    (kill-buffer buf)))

(defun ahg-generic-command (command args sentinel
                                    &optional buffer use-shell
                                              no-show-message
                                              report-untrusted)
  "Executes then given hg command, with the given
arguments. SENTINEL is a sentinel function. BUFFER is the
destination buffer. If nil, a new buffer will be used."
  (unless buffer (setq buffer (generate-new-buffer "*ahg-command*")))
  (with-current-buffer buffer
    (setq mode-line-process
          (list (concat ":" (propertize "%s" 'face '(:foreground "#DD0000"))))))
  (unless no-show-message (message "aHg: executing hg '%s' command..." command))
  (let ((lang (getenv "LANG")))
    (unless ahg-i18n (setenv "LANG"))
    (let ((process
           (apply (if use-shell 'start-process-shell-command 'start-process)
                  (concat "*ahg-command-" command "*") buffer
                  ahg-hg-command
                  (append
                   (unless report-untrusted
                     (list "--config" "ui.report_untrusted=0"))
                   (list command) args))))
      (when ahg-subprocess-coding-system
        (set-process-coding-system process ahg-subprocess-coding-system))
      (set-process-sentinel
       process
       (lexical-let ((sf sentinel)
                     (cmd command)
                     (no-show-message no-show-message))
         (lambda (p s)
           (unless no-show-message
             (message "aHg: executing hg '%s' command...done"
                      cmd))
           (setq mode-line-process nil)
           (funcall sf p s))))
      )
    (setenv "LANG" lang)
    ))

(defun ahg-show-error (process)
  "Displays an error message for the given process."
  (let ((buf (process-buffer process)))
    (pop-to-buffer buf)
    (goto-char (point-min))
    (ahg-command-mode)
    (message "aHg command exited with non-zero status: %s"
             (mapconcat 'identity (process-command process) " "))))

(define-derived-mode ahg-command-mode nil "aHg command"
  "Major mode for aHg commands.

Commands:
\\{ahg-command-mode-map}
"
  (buffer-disable-undo) ;; undo info not needed here
  (toggle-read-only t)
  (font-lock-mode nil)
  (define-key ahg-command-mode-map "h" 'ahg-command-help)
  (define-key ahg-command-mode-map "q" 'ahg-buffer-quit)
  (define-key ahg-command-mode-map "!" 'ahg-do-command)
  (easy-menu-add ahg-command-mode-menu ahg-command-mode-map))

(easy-menu-define ahg-command-mode-menu ahg-command-mode-map "aHg Command"
  '("aHg Command"
    ["Execute Hg Command" ahg-do-command [:keys "!" :active t]]
    ["Help on Hg Command" ahg-command-help [:keys "h" :active t]]
    ["Quit" ahg-buffer-quit [:keys "q" :active t]]))


(defun ahg-push-window-configuration ()
  (set (make-local-variable 'ahg-window-configuration)
       (current-window-configuration))
  (put 'ahg-window-configuration 'permanent-local t))

(defun ahg-pop-window-configuration ()
  (when (and ahg-restore-window-configuration-on-quit
             (boundp 'ahg-window-configuration))
    (set-window-configuration ahg-window-configuration)))

(defun ahg-y-or-n-p (prompt)
  (if ahg-yesno-short-prompt
      (y-or-n-p prompt)
    (yes-or-no-p prompt)))

(defun ahg-uncommitted-changes-p (&optional root)
  (with-temp-buffer
    (let ((global-opts (when root (list "-R" root))))
      (when (= (ahg-call-process "id" (list "-n") global-opts) 0)
        (= (char-before (1- (point-max))) ?+)))))

(defun ahg-line-point-pos ()
  (cons (line-number-at-pos) (- (point) (point-at-bol))))

(defun ahg-goto-line-point (lp)
  ;;(goto-char (point-min))
  (goto-line (car lp))
  (forward-char (min (cdr lp) (- (point-at-eol) (point-at-bol)))))


(defun ahg-call-process (cmd &optional args global-opts)
  (apply 'call-process (append (list ahg-hg-command nil t nil
                                     "--config" "ui.report_untrusted=0")
                               global-opts (list cmd) args)))

(defmacro ahg-dynamic-completion-table (fun)
  (if (fboundp 'completion-table-dynamic)
      `(completion-table-dynamic (quote ,fun))
    `(dynamic-completion-table ,fun)))


(defun ahg-complete-shell-command (command)
  ;; we split the string, and treat the last word as a filename
  (let* ((idx (string-match "\\([^ ]+\\)$" command))
         (matches (file-expand-wildcards (concat (substring command idx) "*")))
         (prev (substring command 0 idx)))
    (mapcar (function (lambda (a) (concat prev a))) matches)))

;;-----------------------------------------------------------------------------
;; log-edit related functions
;;-----------------------------------------------------------------------------

(defun ahg-log-edit-hook (&optional extra-message content)
  (goto-char (point-max))
  (let ((user
         (with-temp-buffer
           (if (and
                (= (ahg-call-process "showconfig" (list "ui.username")) 0)
                (> (point-max) (point-min)))
               (buffer-substring-no-properties (point-min) (1- (point-max)))
             "")))
        (branch
         (with-temp-buffer
           (if (and
                (= (ahg-call-process "branch") 0)
                (> (point-max) (point-min)))
               (buffer-substring-no-properties (point-min) (1- (point-max)))
             "")))
        (changed (log-edit-files))
        (root-regexp (concat "^" (regexp-quote default-directory))))
    (insert
     (format "%s

HG: Enter commit message.  Lines beginning with 'HG:' are removed.
%sHG: --
HG: user: %s
HG: root: %s
HG: branch: %s
HG: committing %s
HG: Press C-c C-c when you are done editing."
             (or content "")
             (if extra-message
                 (concat
                  (mapconcat (lambda (s) (concat "HG: " s))
                             (split-string extra-message "\n") "\n") "\n")
               "")
             user
             default-directory
             branch
             (if changed
                 (mapconcat
                  (lambda (s) (replace-regexp-in-string root-regexp "" s))
                  changed " ")
               "ALL CHANGES (run 'ahg-status' for the details)")))
  (goto-char (point-min))))

(defun ahg-log-edit (callback file-list-function buffer &optional msg content)
  "Sets up a log-edit buffer for committing hg changesets."
  (let ((log-edit-hook
         (list (if (or msg content)
                   (lexical-let ((msg msg)
                                 (content content))
                         (lambda () (ahg-log-edit-hook msg content)))
                 'ahg-log-edit-hook))))
    (log-edit
     callback
     t
     (if (version< emacs-version "23.0") file-list-function
       (list (cons 'log-edit-listfun file-list-function)))
     buffer)))

(defun ahg-parse-commit-message ()
  "Returns the contents of the current buffer, discarding lines
starting with 'HG:'."
  (let ((out))
    (goto-char (point-min))
    (while (not (eobp))
      (unless (looking-at "^HG:")
        (setq out
              (cons
               (buffer-substring-no-properties (point-at-bol) (point-at-eol))
               out)))
      (forward-line 1))
    (mapconcat 'identity (nreverse out) "\n")))


(provide 'ahg)
