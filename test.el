(defun gatherflashes/org-get-ast ()
  (org-element-parse-buffer))



(defun read-stringified-list (&optional buffer-or-name)
  (let ((buffer (or buffer-or-name (current-buffer))))
    (with-temp-buffer
      (insert "'")
      (insert-buffer buffer)
      (goto-char (point-min))
      (replace-string-in-region "#" "")
      (eval-buffer (current-buffer) t nil nil t))))
  
(defun gatherflashes/ast-flashcards ()
  (org-element-map (read-stringified-list) 'headline
    (lambda (headline)
      (gatherflashes/message "thoeunth"))))

(with-current-buffer (get-cache-create "./tests/flash-cards.org" "ast" 'gatherflashes/org-get-ast)
  (print "hello")
  (gatherflashes/ast-flashcards))

(get-cache-create "./tests/flash-cards.org" "ast" 'gatherflashes/org-get-ast t)
(get-cache-create (get-buffer "cache.flash-cards.ast") "ast" 'gatherflashes/org-get-ast)


(with-current-buffer (create-file-buffer "./tests/flash-cards-org")
  (message (format "%S" (org-element-parse-buffer))))
(with-current-buffer ())

(read-stringified-list "cache.flash-cards.ast")
(eval-buffer "cache.flash-cards.ast" t nil nil t)

(get-cache-create "cache.flash-cards.ast" "ast" 'gatherflashes/org-get-ast)


(get-buffer "cache.flash-cards.ast")
(time-subtract '(0 0 0 0) '(0 0 0 0))

(time-convert 0 'integer)

(get-buffer "cache.flash-cards.ast")


(get-path-or-name "cache.flash-cards.ast")
(file-name-nondirectory (get-path-or-name (get-buffer "cache.flash-cards.ast")))
(get-cache-path "cache.flash-cards.ast")

(with-current-buffer (get-buffer "untitled")
  (goto-char (point-min))
  (print (point))
  (insert (format "%s" '(one)))
  (print (point)))

(with-current-buffer (get-buffer "untitled")
  (goto-char (point-min))
  (print (point))
  (insert (format "%s" '(two)))
  (print (point)))

(with-current-buffer (get-buffer "untitled")
  (goto-char (point-min))
  (print (point))
  (insert (format "%s" '(three)))
  (delete-char (- (point-max) (point)))
  (print (point)))

(with-current-buffer "untitled"
  (insert "'")
  (insert (format "%S" '(one two three))))
