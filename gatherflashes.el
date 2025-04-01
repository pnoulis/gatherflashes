;; -*- lexical-binding: t; -*-

(defvar FLASHCARDDIRS '()
  "A list of directories that will be recursively searched for
flashcards")

(defun flashcards/insert (title)
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
  (newline-and-indent))

(defun flashcards/train (tags &optional path)
  "Displays a buffer in the current window with all
flashcards found in PATHS that match TAGS.

PATHS is optional. If not provided then the value of
FLASHCARDDIRS is used.

TAGS is optional. If not provided with, ALL flashcards under
PATHS will be gathered."
  (interactive
   (list (read-string "tags (comma separated): ")))
  (let ((search-path FLASHCARDDIRS)
        (search-tags (string-split (string-trim tags) "," t))
        (training-buffer "flashcards")
        point-question point-answer)
    (flashcards/log-to-message (format "Train: %s" (list search-path search-tags)))
    (with-current-buffer (get-buffer-create training-buffer)
      (erase-buffer)
      (org-mode)
      (dolist (flashcard (flashcards/get search-path search-tags))
        (setq point-question (point))
        (insert (format "%s\n" (plist-get (plist-get flashcard :question) :content)))
        (insert (format "%s\n" (plist-get (plist-get flashcard :answer) :content)))
        (setq point-answer (point))
        (goto-char point-question)
        (flashcards/promote-flashcard flashcard)
        (goto-char point-answer))
      (org-fold-hide-drawer-all)
      (org-shifttab)
      (org-shifttab)
      (goto-char 1)
      (switch-to-buffer training-buffer))))

(defun flashcards/promote-flashcard (flashcard)
  "All FLASHCARD questions are promoted to LEVEL 1 and all
FLASHCARD answers to LEVEL 2 for the purposes of displaying
them in the training buffer"
  (let ((level (plist-get (plist-get flashcard :question) :level)))
    (while (> level 1)
      (org-promote-subtree)
      (setq level (- 1 level)))))

(defun flashcards/get (paths tags)
  "Recursively searches PATHS for flashcards that include TAGS. It completes
by returning a list of all matched flashcards."
  (let ((flashcards '()) flashcard matched-tags)
    (flashcards/map-path-to-buffer
     paths
     (lambda (buffer)
       (org-element-map (org-element-parse-buffer 'headline) 'headline
         (lambda (headline)

           ;; TODO: This beauty of a spaghetti clause needs a total remake.
           ;; It's job is to filter out flashcards that are not a subset
           ;; of `TAGS'. However, an empty TAGS list is interpreted to mean
           ;; that all flashcards regardless of TAG should be included in the
           ;; output.At the same time care is taken for flashcards without any TAGS
           ;; The expression (cons nil '()) basically ensures that a flashcard without any
           ;; TAGS will be included in the output.
           (when (and (setq flashcard (flashcards/is-flashcard-p headline))
                      (setq matched-tags
                            (or (and (null tags) (or (org-element-property :tags headline) (cons nil '())))
                                (flashcards/get-matching-tags headline tags))))
             (plist-put flashcard :question
                        (list
                         :title (org-element-property :raw-value (plist-get flashcard :question))
                         :tags matched-tags
                         :begin (org-element-property :begin (plist-get flashcard :question))
                         :end (org-element-property :begin (plist-get flashcard :answer))
                         :level (org-element-property :level (plist-get flashcard :question))))
             (plist-put flashcard :answer
                        (list
                         :title (org-element-property :raw-value (plist-get flashcard :answer))
                         :begin (org-element-property :begin (plist-get flashcard :answer))
                         :end (org-element-property :end (plist-get flashcard :answer))
                         :level (org-element-property :level (plist-get flashcard :answer))))
             (flashcards/log-to-message (format "Flashcard: %s" flashcard))

             ;; TODO: Right now the actual flashcard is held within
             ;; the :content property. This means that (flashcard
             ;; quantity * size) will bog down speed and clog memory
             ;; if too large of a dataset is found. A solution I can
             ;; think off the top of my head is lazy loading the
             ;; flashcard :content as needed. For example, this
             ;; function would return a list containing the PATH the
             ;; matched flashcards belong to, including the flashcard
             ;; METADATA. Upon user request the program would read the
             ;; substring of the requested PATH with the help of
             ;; :begin and :end.

             ;; Solution 2: (Much better at first glance)
             ;; Instead of holding the flashcards in a list simply
             ;; write them to a file. This function could accept
             ;; a callback. When the flashcard is ready it would
             ;; invoke it with the flashcard. This introduces
             ;; some separation of concerns i guess.
             (plist-put (plist-get flashcard :question) :content
                        (string-trim (buffer-substring
                                      (plist-get (plist-get flashcard :question) :begin)
                                      (plist-get (plist-get flashcard :question) :end))))
             (plist-put (plist-get flashcard :answer) :content
                        (string-trim (buffer-substring
                                      (plist-get (plist-get flashcard :answer) :begin)
                                      (plist-get (plist-get flashcard :answer) :end))))
             (setq flashcards (append flashcards (list flashcard)))
             )))))
   flashcards))

;; TODO: Deal with symlinks
;; TODO: I think when a file is already visited by a Buffer some
;; unexpected behavior occurs, but have yet to pinpoint exactly what.
;; TODO: The (directory-files) screws up a lot if the directory contains
;; something weird like an Emacs recovery files. All this peculiar
;; cases need to be addressed
(defun flashcards/map-path-to-buffer (paths func)
  (dolist (path paths)
    (if (not (file-exists-p path))
        (flashcards/log-to-message (format "Missing path: %s" path))
      (cond ((file-directory-p path)
             (flashcards/log-to-message (format "Descending into: %s" path))
             (flashcards/log-to-message (format "%s" (directory-files path t nil)))
             (flashcards/map-path-to-buffer
;; The expression (cdr (cdr ())) removes the first 2 elements from the
;; (directory-files) list. It is assumed that the first 2 elements
;; will always be the special dot entries (., ..). Although the Elisp
;; manual clearly states that the order of directory entries is not
;; guaranteed to NOT change I must assume they refer to 'actual'
;; files. Such a removal must take place otherwise this function
;; enters an infinitely recursive loop.
              (cdr (cdr (directory-files path t nil))) func))
            ((string= (file-name-extension path) "org")
             (with-current-buffer (find-file-noselect path)
               (flashcards/log-to-message (format "Reading file: %s" path))
               (funcall func (current-buffer))))))))

(defun flashcards/get-matching-tags (headline tags)
  "Check if the tags of HEADLINE are included in TAGS.
Returns the list of matched tags or nil in case of no matches."
  (let ((matched-tags nil))
    (dolist (tag (org-element-property :tags headline))
      (when (member-ignore-case tag tags)
        (setq matched-tags (cons tag matched-tags))))
    matched-tags))

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

(defun flashcards/is-flashcard-p (headline)
  "There are 2 types of flashcards scattered arround my filesystem.
Those created with the '(insert-flashcard) command and those
manually typed. Both include a subheading titled 'Answer'.

This predicate performs 2 tasks:

1. Checks if a heading or one of its children is a flashcard
2. Parses the flashcard into the :question and :answer part"
  (let (question answer)
    (dolist (child (org-element-contents headline))
      (when (and (eq (org-element-type child) 'headline)
                 (string-equal-ignore-case (org-element-property :title child) "Answer"))
        (setq question (org-element-property :parent child))
        (setq answer child)))
    (and answer (list :question question :answer answer))))
