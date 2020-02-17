;;; -*- lexical-binding: t; -*-

(defun shutdown ()
  (interactive)
  (let ((choices '("reboot" "poweroff" "suspend")))
    (progn
      (with-temp-buffer
        (message "%s" (setq choice (ido-completing-read "Shutdown:" choices )))
        (cd "/su::")
        (if (string-equal choice "suspend")
            (shell-command "echo mem > /sys/power/state")
          (shell-command (concat choice)))))))

(defun dmenu ()
  (interactive)
  (let ((choices (remove "."
                         (remove ".."
                                 (delete-dups ;; todo - iterate exec-path instead
                                  (append
                                   (when (file-exists-p (concat (getenv "HOME") "/.nix-profile/bin"))
                                     (directory-files (concat (getenv "HOME") "/.nix-profile/bin")))
                                   (directory-files "/bin")))))))
    (setq-local choice (message "%s" (ido-completing-read "Shutdown:" choices )))
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

(defun scrot ()
  (interactive)
  (shell-command
   (concat "import -window root ~/scrot" (number-to-string(random 1000000)) ".png")))
