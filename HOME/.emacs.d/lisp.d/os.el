;;; -*- lexical-binding: t; -*-

(use-package systemd)

(defun shutdown ()
  (interactive)
  (let ((choices '("kexec" "suspend" "hibernate" "reboot" "poweroff")))
    (message "%s" (setq choice (ido-completing-read "Shutdown:" choices )))
    (progn
      (with-temp-buffer
        (cd "/su::")
        (shell-command (concat "systemctl " choice))))))


(use-package system-packages
  :config
  (setq system-packages-use-sudo nil)
  (setq system-packages-package-manager 'yay)
  (add-to-list 'system-packages-supported-package-managers
               '(yay .
                     ((default-sudo . nil)
                      (install . "yay -S")
                      (search . "yay -Ss")
                      (uninstall . "yay -Rs")
                      (update . "expect << EOF
spawn yay
expect \"password for foo\"
send \"$\(gpg -d < \"$HOME/.authinfo.sudo.gpg\"\)\\r\"
expect eof
EOF")
                      (clean-cache . "yay -Sc")
                      (log . "cat /var/log/pacman.log")
                      (get-info . "yay -Qi")
                      (get-info-remote . "yay -Si")
                      (list-files-provided-by . "yay -Ql")
                      (verify-all-packages . "yay -Qkk")
                      (verify-all-dependencies . "yay -Dk")
                      (remove-orphaned . "yay -Rns $(pacman -Qtdq)")
                      (list-installed-packages . "yay -Qe")
                      (list-installed-packages-all . "yay -Q")
                      (list-dependencies-of . "yay -Qi")
                      (noconfirm . "--noconfirm")))))
