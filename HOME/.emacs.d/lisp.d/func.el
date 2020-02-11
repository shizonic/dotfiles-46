;;; -*- lexical-binding: t; -*-

(defun shutdown ()
  (interactive)
  (let ((choices '("reboot" "poweroff")))
    (message "%s" (setq choice (ido-completing-read "Shutdown:" choices )))
    (progn
      (with-temp-buffer
        (cd "/su::")
        (shell-command (concat choice))))))

(defun dmenu ()
  (interactive)
  (let ((choices (directory-files "/bin")))
    (setq-local choice (message "%s" (ido-completing-read "Run:" choices)))
    (start-process choice nil choice)))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))
