;;; -*- lexical-binding: t; -*-

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"

      gnutls-verify-error t
      gnutls-min-prime-bits 2048

      epg-gpg-program "gpg2"
      password-cache-expiry nil

      mouse-yank-at-point t
      save-interprogram-paste-before-kill t

      apropos-do-all t
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      tab-always-indent 'complete

      tramp-default-method "ssh"
      tramp-copy-size-limit nil

      vc-follow-symlinks t

      ring-bell-function 'ignore)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-x -") 'bury-buffer)
(global-set-key (kbd "<f5>") 'compile)

(fset 'yes-or-no-p 'y-or-n-p)

(show-paren-mode 1)

(delete-selection-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(winner-mode 1)

(show-paren-mode 1)
(electric-pair-mode 1)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq grep-command "grep -r ")

(add-hook 'before-save-hook 'whitespace-cleanup)
