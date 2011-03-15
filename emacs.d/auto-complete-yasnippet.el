(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate."
  :group 'auto-complete)

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate."
  :group 'auto-complete)

(defun ac-yasnippet-table-hash (table)
  (cond
   ((fboundp 'yas/snippet-table-hash)
    (yas/snippet-table-hash table))
   ((fboundp 'yas/table-hash)
    (yas/table-hash table))))

(defun ac-yasnippet-table-parent (table)
  (cond
   ((fboundp 'yas/snippet-table-parent)
    (yas/snippet-table-parent table))
   ((fboundp 'yas/table-parent)
    (car (yas/table-parent table)))
   ((fboundp 'yas/table-parents)
    (car (yas/table-parents table)))
   )
  )

(defun ac-yasnippet-candidate-1 (table)
  (with-no-warnings
    (let ((hashtab (ac-yasnippet-table-hash table))
          (parent (ac-yasnippet-table-parent table))
          (ac-prefix (ac-yasnippet-prefix))
          candidates)
      (maphash (lambda (key value)
                 (push key candidates))
               hashtab)
      (when ac-prefix
        (setq candidates (all-completions ac-prefix (nreverse candidates)))
        )
      (when parent
        (setq candidates
              (append candidates (ac-yasnippet-candidate-1 parent)))
        )
      candidates)))

(defun ac-yasnippet-candidates ()
  (with-no-warnings
    (if (fboundp 'yas/get-snippet-tables)
        ;; >0.6.0
        (apply 'append (mapcar 'ac-yasnippet-candidate-1 (yas/get-snippet-tables major-mode)))
      (let ((table
             (if (fboundp 'yas/snippet-table)
                 ;; <0.6.0
                 (yas/snippet-table major-mode)
               ;; 0.6.0
               (yas/current-snippet-table))))
        (if table
            (ac-yasnippet-candidate-1 table))))))

(defun ac-yasnippet-document (complete)
  "* Completes Documentation for Yasnippet"
  (let (templates only-one (ret ""))
    (setq templates (mapcar #'cdr 
                            (mapcan #'(lambda (table)
                                        (yas/fetch table complete))
                                    (yas/get-snippet-tables))))
    (mapc #'(lambda (template)
              (setq ret (format "%s%s\n%s\n" ret (yas/template-name template) (yas/template-content template))))
          templates)
    (when (string-match "\n\\'" ret)
      (setq ret (replace-match "" nil nil ret)))
    ret))

(defun ac-yasnippet-prefix ()
  "* Gets Yasnippet Prefix."
  (if (looking-back "[^ ]*")
      (match-string 0)
    nil
    )
  )

(ac-define-source yasnippet
  '((depends yasnippet)
    (candidates . ac-yasnippet-candidates)
    (action . yas/expand)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face)
    (document . ac-yasnippet-document)
    (requires . 3)
    (prefix . "\\(\\<[^ ]*\\)")
    (symbol . "a")))