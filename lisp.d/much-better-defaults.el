(setq gnutls-verify-error t
      gnutls-min-prime-bits 2048)

(setq mouse-yank-at-point t
      apropos-do-all t
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      save-interprogram-paste-before-kill t
      tab-always-indent 'complete
      tramp-default-method "ssh"
      vc-follow-symlinks t
      tramp-copy-size-limit nil
      dired-auto-revert-buffer t
      password-cache-expiry nil
      epa-pinentry-mode 'loopback)

(setq-default epg-gpg-program "gpg")

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 8
              fill-column 80)

(ido-mode 1)
(ido-everywhere 1)
(setq ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess)

(show-paren-mode 1)

(require 'dired-x)
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(with-eval-after-load 'async
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(delete-selection-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(winner-mode 1)
