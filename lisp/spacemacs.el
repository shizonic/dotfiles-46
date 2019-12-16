;; stuff from spacemacs

(defun spacemacs/alternate-buffer (&optional window)
  "switch buffer back-and-forth"
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(provide 'spacemacs)
