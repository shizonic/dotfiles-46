;;; -*- lexical-binding: t; -*-

(defun shutdown ()
  "uses tramp to run the commands as root with creds stored in ~/.authinfo.gpg"
  (interactive)
  (let ((choices '("reboot" "poweroff" "suspend")))
    (message "%s" (setq choice (ido-completing-read "Shutdown:" choices )))
    (with-temp-buffer
      (cd "/su::")
      (if (string-equal choice "suspend")
          (shell-command "echo mem > /sys/power/state")
        (shell-command (concat choice))))))

(defun dmenu ()
  "requires ido-mode"
  (interactive)
  (let ((choices (directory-files "/bin")))
    (setq-local choice (message "%s" (ido-completing-read "Shutdown:" choices )))
    (start-process choice nil choice)))

(defun scrot ()
  "requires imagemagick"
  (interactive)
  (shell-command
   (concat "import -window root ~/scrot" (number-to-string(random 1000000)) ".png")))

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
