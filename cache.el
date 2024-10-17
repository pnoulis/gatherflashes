(defconst CACHEDIR "~/tmp")

(defun get-cache-create (filename stage stagefn &optional force)
  (let* ((cachename (cache-file-path filename stage))
        (filemodtime (file-attribute-modification-time (file-attributes filename)))
        (cachemodtime (when (file-exists-p cachename) (file-attribute-modification-time (file-attributes cachename)))))
    (with-current-buffer (get-cache cachename)
      (when (or force (time-less-p cachemodtime filemodtime))
        (gatherflashes/message (concat "filemodtime -> " (format "%s" filemodtime)))
        (gatherflashes/message (concat "cachemodtime -> " (format "%s" cachemodtime)))
        (gatherflashes/message (concat "renewing cache -> " filename))
        (erase-buffer)
        (insert-file-contents-literally filename nil)
        (insert (format "%s" (funcall stagefn)))
        (set-cache cachename (current-buffer)))
      (current-buffer))))

(defun cache-file-path (filename stage)
  (expand-file-name
   (substitute-in-file-name
    (concat
     (file-name-as-directory CACHEDIR)
     "cache."
     (file-name-nondirectory filename)
     (concat "." stage)))))

(defun set-cache (filename data)
  "This function serves multiple purposes. It is part of the cache
mechanism of the application and it lends itself as a convenient
debugging method. It is supposed to be invoked after an important
STAGE so that it may be cached and examined."
  (cond ((bufferp data)
         (with-current-buffer data (write-region nil nil filename)))
        ((listp data)
         (write-region (with-output-to-string (princ data)) nil filename))
        (write-region (with-output-to-string data) nil filename))
  data)

(defun get-cache (filename)
  (get-buffer-create (file-name-nondirectory filename)))
