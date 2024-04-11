;; -*- lexical-binding: t; -*-

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

(defun get-flashcards (file &optional prune)
  "Return a LIST of FLASHCARDS found within FILE"
  (with-temp-buffer
    (insert-file-contents file)
    (let ((flashcards '()))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (when (is-flashcard-p headline)
            (setq flashcards (append flashcards (list headline)))))
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
  "There are 2 types of flashcards scattered arround my filesystem.
Those that have been created using the insert-flashcard function.
And the ones created without the insert-flashcard function which
usually have a subheading whose raw-value is 'Answer'
"
  (or (is-new-flashcard headline)
      (is-old-flashcard headline)))

(defun is-new-flashcard (flashcard)
  (string-equal-ignore-case "flash" (or (org-element-property :CATEGORY flashcard) "")))

(defun is-old-flashcard (flashcard)
  (dolist (el (org-element-contents flashcard) isflashcard)
    (setq isflashcard (string-equal-ignore-case (or (org-element-property :raw-value el) "") "answer"))))


(defun is-element-answer (element)
  (string-equal-ignore-case (or (org-element-property :raw-value element) "") "answer"))


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



;; (save-current-buffer
;; (progn (set-buffer (get-buffer-create "gather"))
;;        (erase-buffer)
;;        (insert (org-element-interpret-data (get-flashcards "/home/pnoul/office/nodejs.org")))
;;        (org-mode))
;; (switch-to-buffer (get-buffer "gather") '(right))
;; "gather")


;; (get-flashcards "./tests/flash-cards.org")

(defun flashcard-answer ()
  "Answer the flashcard
A new Answer entry shall be created.
That new entry shall include the following properties
:Date
"
  (interactive)
  (with-output-to-temp-buffer "tmp"
    (print (string-equal-ignore-case "answer"
                                     (org-element-property :raw-value (org-element-at-point))))))

(defun flashcard-raw-value ()
  (interactive)
  (with-output-to-temp-buffer "tmp"
      (print (org-element-property :raw-value (org-element-at-point)))))


(defun flashcard-is-headline ()
  (interactive)
  (with-output-to-temp-buffer "tmp"
    (print (__flashcard-is-headline (org-element-at-point)))
    (print (and (is-element-answer (org-element-at-point)) "answer"))))

(defun __flashcard-is-headline (element)
  (eq 'headline (org-element-type element)))


;; org-forward-heading-


(defun move-up-until-org-data (headline)
  (if (eq 'org-data )))


(defun flashcard-last-answer (flashcard)
  (dolist (current flashcard last-answer)
    (and (string-equal-ignore-case (or (org-element-property :raw-value current) "") "answer")
         (setq last-answer
               (cons (org-element-property :begin current)
                (cons (org-element-property :end current) '()))))))


(defun flashcard-get-tree (buffer-or-filename)
  (if (bufferp buffer-or-filename)
      (with-current-buffer buffer-or-filename
        (org-element-parse-buffer))
    (with-temp-buffer
      (insert-file-contents buffer-or-filename)
      (org-element-parse-buffer))))


(defun flashcard-find-point (buffer-or-filename)
  (let ((headline-count 0)
        (headlines '()))
    (setq headlines
          (org-element-map (flashcard-get-tree buffer-or-filename) 'headline
            (lambda (headline)
              (setq headline-count (+ 1 headline-count))
              (when (= 1 headline-count)
                (org-element-put-property headline :parent nil))
            nil nil '(org-data)))
    (cons (cons 'headline-count (cons headline-count '())) headlines))))

(defun flashcard-subs (element)
  (dolist (sub (org-element-contents element) subs)
    (setq subs (cons sub subs))))


(defun is-at-point (element)
  "If ELEMENT bounds (point) return it"
  (and (>= (point) (flashcard-element-begin element))
       (<= (point) (flashcard-element-end element))
       element))

(defun flashcard-element-begin (element)
  "Return the START index of ELEMENT"
  (org-element-property :begin element))

(defun flashcard-element-end (element)
  "Return the END index of ELEMENT"
  (org-element-property :end element))

(defun tmp-write ()
  (interactive)
    (with-output-to-temp-buffer "tmp"
      (print (flashcard-find-point))))


(defun write-to-tmp (fn)
  (interactive "a")
  (let ((res (funcall fn)))
    (with-current-buffer (get-buffer-create "tmp2")
      (erase-buffer)
      ;; (insert (format "%s" res))
      (insert (format "%s" res))
      )))


(setq data
      (save-excursion
        (set-buffer (get-buffer "flash-cards.org"))
        (flashcard-find-point)))


(write-to-tmp (lambda ()
                (org-element-map (flashcard-get-tree "./tests/flash-cards.org") 'headline
                  (lambda (headline)
                    (car (cdr (cdr (car (org-element-contents headline)))))
                    )
                    ;; (car (cdr (cdr (car (org-element-contents
                    ;;       (org-element-put-property headline :parent nil))))))
                    ;; )
                    ;; property
                    ;; (cdr (car (cdr headline)))
                  nil 't '(org-data))))


(defun flashcard-drawer (element)
  (car (cdr element)))
(flashcard-drawer (element)
                  )

(org-element-property :property )


;; org-data
(flashcard-get-tree "./tests/flash-cards.org")
