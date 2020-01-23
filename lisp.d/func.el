(defun shutdown ()
  (interactive)
  (let ((choices '("kexec" "suspend" "hibernate" "reboot" "poweroff")))
    (message "%s" (setq shutdown-choice (ido-completing-read "Shutdown:" choices )))
    (with-temp-buffer
      (cd "/su::")
      (shell-command (concat "systemctl " shutdown-choice)))))

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

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(defun unlock ()
  (interactive)
  (async-shell-command
   "eval $(keychain --eval --agents gpg,ssh 77CF5C5C65A8F9F44940A72CDD4795B51117D906 id_rsa); emacsclient -e '(keychain-refresh-environment)'"))

(defun lock ()
  (interactive)
  (async-shell-command "keychain --agents ssh,gpg -k all"))
