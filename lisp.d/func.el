(defun shutdown ()
  (interactive)
  (let ((choices '("kexec" "suspend" "hibernate" "reboot" "poweroff")))
    (message "%s" (setq choice (ido-completing-read "Shutdown:" choices )))

    (if (string-equal choice "kexec")
        (progn
          (with-temp-buffer
            (cd "/su::")
            (shell-command (concat "systemctl " choice))))
      (progn
        (shell-command (concat "systemctl " choice))))))

(defun dmenu ()
  (interactive)
  (let ((choices (directory-files "/bin")))
    (setq-local choice (message "%s" (ido-completing-read "Run:" choices)))
    (start-process choice nil choice)))
