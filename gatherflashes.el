(defun insert-flash-card()
  "Creates a new flash-card"
  (interactive)
  (org-insert-heading-respect-content)
  (org-insert-property-drawer)
  (org-entry-put (point) "CATEGORY" "flash")
  (end-of-line))



