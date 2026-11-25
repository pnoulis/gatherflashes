;; -*- lexical-binding: t; -*-

(setq TEST_DATA "/home/pnoulis/src/pnoulis/gatherflashes/flashcards-test-data.org")

(defun flashcards/get (FILENAME)
  "Retrieves all flashcards found in the file referenced by FILENAME"
  (save-current-buffer
    (set-buffer (get-buffer-create "tmp-buffer"))
    (erase-buffer)
    (insert-file-contents-literally FILENAME)
    (let ((count 0) (properties '()))
      (org-element-map (org-element-parse-buffer 'headline) 'headline
        (lambda (headline)
          (unless (or (= count 1) (not (flashcards/is-flashcardp headline)))
            (setq count (1+ count))
            (list
             :title (org-element-property :raw-value (org-element-property :parent headline))
             :tags (org-element-property :tags (org-element-property :parent headline))
             :begin (org-element-property :begin (org-element-property :parent headline))
             :end (org-element-property :end (org-element-property :parent headline))
             )
            )
          )
        )
      )
    )
  )

(defun flashcards/parse-org-file (FILENAME)
  (save-current-buffer
    (set-buffer (get-buffer-create "tmp-buffer"))
    (erase-buffer)
    (insert-file-contents-literally FILENAME)
    (org-mode)
    (org-element-parse-buffer 'greater-element)))

(defun flashcards/get-filetags (AST)
  (split-string
   (org-element-map
       AST
       'keyword
     (lambda (keyword)
       (when (string-equal-ignore-case (org-element-property :key keyword) "FILETAGS")
         (org-element-property :value keyword)))
     ;; The values that follow are arguments to org-element-map. Check the
     ;; function documentation for details. Essentially, they stop the iteration
     ;; upon the return of a non-nil value by the mapping function.
     nil t)
   ":" t  "[[:space:]]*"))

(defun flashcards/is-flashcardp (NODE)
  "Returns true if the NODE is a flashcard"
  (when (eq (org-element-type NODE) 'headline)
    (string-equal-ignore-case (org-element-property :title NODE) "answer")))

(defun flashcards/get-flashcard-nodes (AST)
  "Returns all headlines that are identified as a flashcard"
  (org-element-map AST 'headline
    (lambda (headline)
      (when (flashcards/is-flashcardp headline)
        (org-element-copy (org-element-property :parent headline))))))

(defun flashcards/intersectsp (A B)
  "Returns true if an element of A is also a member of B"
  (when (and A B)
    (or (member (car A) B)
        (flashcards/intersectsp (cdr A) B))))

(defun flashcards/filter-flashcard-nodes (AST &optional FILTER_TAGS)
  "The FILTER_* arguments define conditions that each
flashcard node should meet to be included in the return set"
  (remq nil
        (cond ((null FILTER_TAGS)
               (flashcards/get-flashcard-nodes AST))
              ((flashcards/intersectsp FILTER_TAGS (flashcards/get-filetags AST))
               (flashcards/get-flashcard-nodes AST))
              (t (mapcar (lambda (flashcard)
                           (when (flashcards/intersectsp
                                  (org-element-property :tags flashcard) FILTER_TAGS)
                             flashcard))
                         (flashcards/get-flashcard-nodes AST))))))

(defun flashcards/get-node-raw-text (NODE)
  "Extracts the raw text out of NODE"
  (save-current-buffer
    (set-buffer (get-buffer-create "tmp-buffer"))
    (string-trim (buffer-substring
                  (org-element-property :begin NODE)
                  (org-element-property :end NODE)))))

(defun flashcards/normalize-flashcard-nodes (FLASHCARD_NODES)
  "The purpose of this function is to:
1. Strip unwanted data off the flashcard
2. Convert it into a plist for easy handling down the line"
  (mapcar
   (lambda (flashcard)
     (let ((question flashcard)
           (answer (car (cdr (cdr flashcard))))
           (attempts (cdr (cdr (cdr flashcard)))))
       (list :question (list :title (org-element-property :raw-value flashcard)
                             :tags  (org-element-property :tags      flashcard)
                             :begin (org-element-property :begin     flashcard)
                             :end   (org-element-property :end       flashcard))
             :answer   (list :begin (org-element-property :begin     answer)
                             :end   (org-element-property :end       answer)
                             :raw   (flashcards/get-node-raw-text    answer)
                             :contents (org-element-content answer)
                             )
             :attempts attempts)))
   FLASHCARD_NODES))

(flashcards/normalize-flashcard-nodes (flashcards/filter-flashcard-nodes (flashcards/parse-org-file TEST_DATA) '("notlinux")))

;; (defun flashcards/filter-flashcard-nodes (AST)
;;   (org-element-map AST 'headline
;;     (lambda (headline)
;;       (when (flashcards/is-flashcardp headline)
;;         ;; FINALLY THIS WORKS BEAUTIFULLY (removes the :parent contents from each node)
;;         (org-element-copy (org-element-property :parent headline))))))

;; (let ((flashcard (org-element-property :parent headline)))
;;   (list :question (org-element-property :raw-value flashcard)
;;         :answer (org-element-copy (car (cdr (cdr flashcard))))
;;         :attempts (cdr (cdr (cdr flashcard)))))))))

;; (let ((question (org-element-property :parent headline))
;;       (answer headline))
;;   (list
;;    :question (list
;;               :title (org-element-property :raw-value question)
;;               :tags (org-element-property :tags question)
;;               :begin (org-element-property :begin question)
;;               :end (org-element-property :end question))
;;    :answer (list
;;             :title (org-element-property :raw-value answer)
;;             :begin (org-element-property :begin answer)
;;             :end (org-element-property :end answer))))))))


(defun flashcards/log-to-message (text)
  (with-current-buffer "*Messages*"
    ;; The *Messages* buffer tends to be in read-only-mode
    ;; by default
    (setq inhibit-read-only t)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert "(flashcards)-> " text)
    (goto-char (point-max))))

(flashcards/get "/home/pnoulis/src/pnoulis/study/books/domain-modeling-made-functional/flashcards.org")

(org-element-interpret-data (car (cdr (cdr (cdr (car (flashcards/filter-flashcard-nodes
                                                      (flashcards/parse-org-file "/home/pnoulis/src/pnoulis/gatherflashes/flashcards-test-data.org"))))))))

(flashcards/filter-flashcard-nodes
 (flashcards/parse-org-file "/home/pnoulis/src/pnoulis/gatherflashes/flashcards-test-data.org"))

(flashcards/log-to-message (org-element-interpret-data (org-element-copy (flashcards/filter-flashcard-nodes
                                                                          (flashcards/parse-org-file "/home/pnoulis/src/pnoulis/gatherflashes/flashcards-test-data.org")))))


;; (flashcards/get-filetags2 (flashcards/parse-org-file "/home/pnoulis/src/pnoulis/gatherflashes/flashcards-test-data.org"))

;; (org-element-interpret-data '(paragraph (:standard-properties [218 218 218 223 223 0 nil planning element nil nil nil nil nil \,(get-buffer "tmp-buffer") nil nil])))

;; ;; (flashcards/log-to-message (org-element-normalize-contents (org-element-interpret-data (cdr (cdr (car (flashcards/filter-flashcard-nodes
;; ;;  (flashcards/parse-org-file "/home/pnoulis/src/pnoulis/gatherflashes/flashcards-test-data.org"))))))))

(flashcards/filter-flashcard-nodes
 (flashcards/parse-org-file "/home/pnoulis/src/pnoulis/gatherflashes/flashcards-test-data.org"))
