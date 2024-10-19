(defun any-join (anys &optional separator)
  (let ((str ""))
    (if (not separator)
        (dolist (any anys) (setq str (concat str (format "%s" any))))
      (setq str (format "%s" (car anys)))
      (dolist (any (cdr anys)) (setq str (concat str separator (format "%s" any)))))
    str))

(defun gatherflashes/message (&rest args)
  (message (any-join (append '("[gatherflashes]") args) " ")))

