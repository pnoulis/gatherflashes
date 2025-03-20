(defconst CACHEDIR "~/tmp")
(defconst CACHE_NAME_PREFIX "cache")

(defun get-cache-create (current-stage next-stage-id stagefn &optional force)
  (let* (
         (print-circle t)
         (current-stage-path (get-path-or-name current-stage))
         (current-stage-cache (get-cache-path current-stage-path))
         (current-stage-name (file-name-nondirectory (or current-stage-cache current-stage-path)))
         (next-stage-name (create-stage-name (or (and current-stage-cache (strip-stage-name current-stage-cache)) current-stage-name) next-stage-id))
         (next-stage-cache (append-cachedir (or (and current-stage-cache current-stage-name) next-stage-name)))
         (current-stage-mod-time (time-convert (or (file-attribute-modification-time (file-attributes current-stage-cache)) 0) 'integer))
         (next-stage-mod-time (time-convert (or (file-attribute-modification-time (file-attributes next-stage-cache)) 0) 'integer)))
    (gatherflashes/message "force -> " force)
    (gatherflashes/message "next-stage-id -> " next-stage-id)
    (gatherflashes/message "current-stage-path -> " current-stage-path)
    (gatherflashes/message "current-stage-cache -> " current-stage-cache)
    (gatherflashes/message "current-stage-name -> " current-stage-name)
    (gatherflashes/message "current-stage-mod-time -> " current-stage-mod-time)
    (gatherflashes/message "next-stage-name -> " next-stage-name)
    (gatherflashes/message "next-stage-cache -> " next-stage-cache)
    (gatherflashes/message "next-stage-mod-time -> " next-stage-mod-time)
    (with-current-buffer (get-buffer-create (or (and current-stage-cache current-stage-name) next-stage-name))
      (goto-char (point-min))
      (if (and (not force) (> next-stage-mod-time 0) (>= next-stage-mod-time current-stage-mod-time))
          (progn
            (gatherflashes/message "Cache: fresh")
            (insert-file-contents-literally next-stage-cache nil nil nil t))
        (gatherflashes/message "Cache: stale")
        ;; This should only run when the input is a file. Which means that we
        ;; are at the start of processing a new flashcard file and hence the
        ;; buffer should be replaced
        (when (file-exists-p current-stage-path)
          (gatherflashes/message "Current Stage: is a file")
          (insert-file-contents-literally current-stage-path nil nil nil t))
        (insert (prin1 (funcall stagefn)))
        (delete-char (- (point-max) (point)))
        (set-cache next-stage-cache (current-buffer)))
      (rename-buffer next-stage-name)
      (get-buffer next-stage-name))))

(defun get-cache-path (buffer-or-name)
  (let ((filename (append-cachedir (file-name-nondirectory (get-path-or-name buffer-or-name)))))
    (and (file-exists-p filename) filename)))

(defun create-stage-name (filename stage)
  (concat CACHE_NAME_PREFIX "." (file-name-sans-extension (file-name-nondirectory filename)) "." stage))

(defun strip-stage-name (buffer-or-path)
  (substring (file-name-nondirectory (file-name-sans-extension (get-path-or-name buffer-or-path)))
             (+ (length CACHE_NAME_PREFIX) 1)))

(defun append-cachedir (filename)
  (expand-file-name (substitute-in-file-name (concat CACHEDIR"/" filename))))

(defun get-path-or-name (buffer-or-name)
  (or (and (bufferp buffer-or-name) (or (buffer-file-name buffer-or-name) (buffer-name buffer-or-name)))
      buffer-or-name))

(defun set-cache (filename data)
  (if (bufferp data)
      (with-current-buffer data (write-region nil nil filename))
    (write-region (format "%s" data) nil filename)))
