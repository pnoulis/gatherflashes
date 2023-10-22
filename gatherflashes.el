(defun insert-flash-card(title)
  "Create a new flash-card"
  (interactive "stitle: ")
  (org-insert-heading '(4) nil t)
  (insert title)
  (newline-and-indent)
  (org-insert-property-drawer)
  (forward-line -1)
  (org-fold-hide-drawer-toggle)
  (org-entry-put (point) "CATEGORY" "flash")
  (org-entry-put (point) "REVISION_DATE" (org-read-date 't nil))
  (end-of-line 4)
  (org-insert-subheading 1)
  (insert "Answer")
  (newline-and-indent)
  )

(defun at-flash-card-p()
  "Non-nil when point is at a flash-card heading")

(defun get-flash-cards(file)
  "Return the flash-cards found within FILE if any."
  (interactive)
  ;; visit file
  ;; give the buffer a random name
  ;;(read-file-contents)
  ;; create a buffer
  ;; copy file into buffer, that is not the same as visiting
  ;; (insert-file-contents filename) -> (absolute filename)
  (set-buffer (get-buffer-create "gatherflashes<VISITING_FILE>"))
  (erase-buffer)
  (insert-file-contents file)
  (let ((AST (org-element-parse-buffer))
        (headlines '()))
    ;; get a list of all flash cards
    (goto-char (point-max))
    (newline)
    ;; (insert (format "%s" AST))
    (newline)
    (newline)
    (org-element-map AST 'headline
      (lambda (hl)
        ;; (insert (format "%s" hl))
        ;; (insert (format "%s" (org-element-property :title hl)))
        (when (is-flashcard-p hl)
          (setq headlines (append headlines (list (get-flashcard-bounds hl))))
          )))
    (with-current-buffer (get-buffer-create "tmp2")
      (dolist (flashcard headlines)
        (insert-buffer-substring "gatherflashes<VISITING_FILE>" (nth 0 flashcard) (nth 1 flashcard))
        )
      )
    ))
    
(defun is-flashcard-p (headline)
  (string= "flash" (org-element-property :CATEGORY headline)))

(defun get-flashcard-bounds (flashcard)
  (list (org-element-property :begin flashcard)
        (org-element-property :end flashcard)))
