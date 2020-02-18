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

(fset 'yes-or-no-p 'y-or-n-p)

(require 'delsel)
(delete-selection-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'winner)
(winner-mode 1)

(require 'paren)
(show-paren-mode 1)

(require 'elec-pair)
(electric-pair-mode 1)

(require 'executable)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq grep-command "grep -r ")

(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'saveplace)
(save-place-mode 1)

(require 'recentf)
(recentf-mode 1)
