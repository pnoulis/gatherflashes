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
  (org-entry-put (point) "REVISION_DATE" (format-time-string "%FT%T" (org-read-date 't 't)))
  (end-of-line 4)
  (org-insert-subheading 1)
  (insert "Answer")
  (newline-and-indent)
  )

(defun at-flash-card-p()
  "Non-nil when point is at a flash-card heading")

(defun get-flash-cards(file)
  "Return ALL FLASHCARDS found within FILE"
  (interactive)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((flashcards))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (when (is-flashcard-p headline)
            (setq flashcards (append flashcards (list headline))))))
      flashcards)))

(defun filter-flash-cards (flashcards)
  (let ((filtered))
    (dolist (fc flashcards)
      (when (is-flashcard-up-for-practice-p fc)))))
;; (when (and
;;        (is-flashcard-p headline)
;;        (is-flashcard-up-for-practice-p headline))
;;   (setq flashcards (append flashcards (list headline)))
;; (setq flashcards (append flashcards (list (get-flashcard-bounds headline))))

(defun write-to-file (any)
  (with-current-buffer (get-buffer-create "tmp2")
    (erase-buffer)
    (insert (format "%s" any))))

  ;; (set-buffer (get-buffer-create "gatherflashes<VISITING_FILE>"))
  ;; (erase-buffer)
  ;; (insert-file-contents file)
  ;; (let ((AST (org-element-parse-buffer))
  ;;       (headlines '()))
  ;;   ;; get a list of all flash cards
  ;;   (goto-char (point-max))
  ;;   (newline)
  ;;   ;; (insert (format "%s" AST))
  ;;   (newline)
  ;;   (newline)
  ;;   (org-element-map AST 'headline
  ;;     (lambda (hl)
  ;;       ;; (insert (format "%s" hl))
  ;;       (insert (format "%s" (org-element-property :title hl)))
  ;;       (when (and (is-flashcard-p hl) (is-flashcard-up-for-practice-p hl))
  ;;         (setq headlines (append headlines (list (get-flashcard-bounds hl))))
  ;;         )))
  ;;   (with-current-buffer (get-buffer-create "tmp2")
  ;;     (erase-buffer)
  ;;     (dolist (flashcard headlines)
  ;;       (insert-buffer-substring "gatherflashes<VISITING_FILE>" (nth 0 flashcard) (nth 1 flashcard))
  ;;       )
  ;;     )
  ;;   ))
    
(defun is-flashcard-p (headline)
  (string= "flash" (org-element-property :CATEGORY headline)))

(defun is-flashcard-up-for-practice-p (flashcard)
  (flashcard-revdate-is-today (flashcard-get-revisions flashcard)))

(defun flashcard-revdate-is-today (revdate)
  "Return 't' if REVDATE is TODAY"
  (string=
   (format-time-string "%d" (flashcard-revdate-to-timestamp revdate))
   (format-time-string "%d" (current-time))))

(defun get-flashcard-bounds (flashcard)
  (list (org-element-property :begin flashcard)
        (org-element-property :end flashcard)))


(defun flashcard-get-revisions (flashcard)
  "Returns all revisions in FLASHCARD"
  (org-element-property :REVISION_DATE flashcard))

(defun flashcard-revdate-to-timestamp (revdate)
  "Convert REVDATE from iso-8601 extended format into a
lisp TIMESTAMP"
  (encode-time (iso8601-parse revdate)))

(defun flashcard-timestamp-to-revdate (timestamp)
  "iso8601 extended format given by: '%FT%T'"
  (format-time-string "%FT%T" timestamp))



(current-time)
(date-to-time "2023-10-22")
(time-less-p (date-to-time "2023-10-22") nil)
(flashcard-revdate-is-today "2023-10-22T13:45:00")


(current-time-string)

(float-time)



;; Get NOW as a TIMESTAMP
;; where TIMESTAMP is the seconds since epoch
(time-convert nil 'integer)1698056444
(time-convert (current-time) 'integer)1698056441

;; get NOW as a TIMESTAMP
;; where TIMESTAMP is in lisp format
(current-time)
(time-convert (current-time) 'list)(25910 18728 787390 752000)
(time-convert nil 'list)(25910 18733 933452 529000)

;; get NOW as a STRING
;; where STRING is in iso-8601 extended format.
(format-time-string "%FT%T%z" nil)"2023-10-23T13:55:11+0300"
(format-time-string "%FT%T%z" (current-time))"2023-10-23T13:55:07+0300"

;; convert a STRING in iso-8601 extended format
;; into a TIMESTAMP in lisp format
(iso8601-parse "2023-10-23T13:40:00")
(encode-time (iso8601-parse "2023-10-23T13:40:00"))

;; convert a TIMESTAMP into an ISO8601 extended format
(format-time-string "%FT%T" TIMESTAMP)

(current-time-string)"Mon Oct 23 13:37:59 2023"

(current-time)(25910 19733 978225 30000)

(encode-time (current-time))

(encode-time nil)

;; (iso8601-parse (current-time))
(format-time-string "%FT%T" (org-read-date 't 't))

(setq iso8601-format-specifier "%FT%T")

(setq d1 (flashcard-revdate-to-timestamp (format-time-string iso8601-format-specifier nil)))


(setq d2 (flashcard-revdate-to-timestamp (format-time-string iso8601-format-specifier nil)))

(time-less-p d1 d2)

(format-time-string "%d" (current-time))

