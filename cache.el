(defconst CACHEDIR "~/tmp")
(defconst CACHE_NAME_PREFIX "cache")

(defun get-cache-create (current-stage next-stage-id stagefn &optional force)
  (let* ((bufname (get-name-or-path current-stage))
         (current-stage-cache (get-cache-file bufname))
         (current-stage-name (file-name-nondirectory (or current-stage-cache bufname)))
         (next-stage-name (create-stage-name (or (and current-stage-cache (strip-stage-name current-stage-name)) bufname) next-stage-id))
         (current-stage-mod-time (file-attribute-modification-time (file-attributes current-stage-cache)))
         (next-stage-mod-time (file-attribute-modification-time (file-attributes (append-cachedir next-stage-name)))))
    (gatherflashes/message (concat "force -> " (format "%s" force)))
    (gatherflashes/message (concat "next-stage-id -> " next-stage-id))
    (gatherflashes/message (concat "cache-stage-cache -> " (or current-stage-cache bufname)))
    (gatherflashes/message (concat "current-stage-name -> " current-stage-name))
    (gatherflashes/message (concat "current-stage-mod-time -> " (format "%s" current-stage-mod-time)))
    (gatherflashes/message (concat "next-stage-name -> " next-stage-name))
    (gatherflashes/message (concat "next-stage-mod-time -> " (format "%s" next-stage-mod-time)))
    (with-current-buffer (get-buffer-create (or (and current-stage-cache current-stage-name) next-stage-name))
      (if (and (not force) (time-less-p (or current-stage-mod-time 0) (or next-stage-mod-time 0)))
          (gatherflashes/message "cache exists")
        (gatherflashes/message "cache does not exist")
        (unless current-stage-cache
          (insert-file-contents-literally bufname nil nil nil t))
        (insert (format "%s" (funcall stagefn)))
        (delete-char (- (point-max) (point)))
        (set-cache (append-cachedir next-stage-name) (current-buffer))
        (rename-buffer next-stage-name))
      (current-buffer))))

(defun get-cache-file (buffer-or-name)
  (let ((filename (append-cachedir (file-name-nondirectory (get-name-or-path buffer-or-name)))))
    (and (file-exists-p filename) filename)))

(defun create-stage-name (filename stage)
  (concat CACHE_NAME_PREFIX "." (file-name-sans-extension (file-name-nondirectory filename)) "." stage))

(defun strip-stage-name (buffer-or-path)
  (substring
   (file-name-nondirectory
    (file-name-sans-extension
     (or (and (bufferp buffer-or-path) (or (buffer-file-name buffer-or-path) (buffer-name buffer-or-path)))
         buffer-or-path)))
   (+ (length CACHE_NAME_PREFIX) 1)))

(defun append-cachedir (filename)
  (expand-file-name (substitute-in-file-name (concat CACHEDIR"/" filename))))

(defun get-name-or-path (buffer-or-name)
  (or (and (bufferp buffer-or-name) (or (buffer-file-name buffer-or-name) (buffer-name buffer-or-name)))
      buffer-or-name))

(defun set-cache (filename data)
  (if (bufferp data)
      (with-current-buffer data (write-region nil nil filename))
    (write-region (format "%s" data) nil filename)))
