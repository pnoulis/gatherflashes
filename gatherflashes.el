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
  "Produce a list of flash-cards found within FILE"
  ;; visit file
  ;; give the buffer a random name
  ;;(read-file-contents)
  ;; 
  ;; create a buffer
  ;; copy file into buffer, that is not the same as visiting
  ;; (insert-file-contents filename) -> (absolute filename)
  (set-buffer (get-buffer-create "gatherflashes<VISITING_FILE>"))
  (erase-buffer)
  (insert-file-contents file)
  ;; get a list of all flash cards
  )


(defun gather-flash-cards)
