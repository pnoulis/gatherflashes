(defun insert-flashcard(title)
  "Inserts a new FLASHCARD formatted as an
org-mode headline into the current buffer"
  (interactive "stitle: ")
  (org-insert-heading '(4) nil t)
  (insert title)
  (newline-and-indent)
  (org-insert-property-drawer)
  (forward-line -1)
  (org-fold-hide-drawer-toggle)
  (org-entry-put (point) "CATEGORY" "flash")
  (org-entry-put (point) "REVISION_DATE" (format-time-string "%FT%T" (org-read-date 't 't)))
  (end-of-line 4)
  (org-insert-subheading 1)
  (insert "Answer")
  (newline-and-indent)
  )

(defun get-flashcards (file)
  "Return a LIST of FLASHCARDS found within FILE"
  (with-temp-buffer
    (insert-file-contents file)
    (let ((flashcards '()))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (when (is-flashcard-p headline)
            (setq flashcards (append flashcards (list (prune-flashcard-node headline))))))
        nil nil 'headline)
      flashcards)))

(defun prune-flashcard-node (flashcard)
  "This function ASSUMES that (current-buffer) contains the FLASHCARD
at positions (org-element-property :begin), (org-element-property :end).

Given a FLASHCARD node in the AST produced by org-element-parse-buffer
return a new list containing only needed properties of FLASHCARD.
"
  (let ((begin (org-element-property :begin flashcard))
        (end (org-element-property :end flashcard)))
    (list :title (org-element-property :raw-value flashcard)
          :revisions (org-element-property :REVISION_DATE flashcard)
          :tags (org-element-property :tags flashcard)
          :begin begin
          :end end
          :contents (string-trim (buffer-substring begin end)))))

(defun filter-flashcards (flashcards)
  (let ((filtered '()))
    (dolist (flashcard flashcards)
      (when (is-flashcard-up-for-practice-p flashcard)
        (setq filtered (append filtered (list flashcard)))))
    filtered))

(defun write-to-file (any)
  (with-current-buffer (get-buffer-create "tmp2")
    (erase-buffer)
    (insert (format "%s" any))))

(defun is-flashcard-p (headline)
  (string= "flash" (org-element-property :CATEGORY headline)))

(defun is-flashcard-up-for-practice-p (flashcard)
  (flashcard-revdate-is-today (flashcard-get-revisions flashcard)))

(defun flashcard-revdate-is-today (revdate)
  "Return 't if REVDATE is TODAY"
  (string=
   (format-time-string "%d" (flashcard-revdate-to-timestamp revdate))
   (format-time-string "%d" (current-time))))

(defun flashcard-get-revisions (flashcard)
  "Returns all revisions in FLASHCARD"
  (plist-get flashcard :revisions))

(defun flashcard-revdate-to-timestamp (revdate)
  "Convert REVDATE from iso-8601 extended format into a
lisp TIMESTAMP"
  (encode-time (iso8601-parse revdate)))

(defun flashcard-timestamp-to-revdate (timestamp)
  "Convert TIMESTAMP to an iso-8601 extended format string: '%FT%T'"
  (format-time-string "%FT%T" timestamp))

