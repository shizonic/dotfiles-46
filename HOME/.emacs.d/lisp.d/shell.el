;;; -*- lexical-binding: t; -*-

(use-package better-shell :demand
  :init
  (bind-key* (kbd "C-t") 'better-shell-for-current-dir)
  (with-eval-after-load 'exwm (exwm-input-set-key (kbd "C-t") 'better-shell-for-current-dir))

  (global-set-key (kbd "C-c T") 'better-shell-remote-open)
  (global-set-key (kbd "C-c P") 'better-shell-for-projectile-root)
  (global-set-key (kbd "C-c s") 'better-shell-sudo-here))

;; TODO
(use-package emacs-piper :disabled
  :straight (emacs-piper :type git :host gitlab :repo "howardabrams/emacs-piper"))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (when (executable-find "shellcheck")
    (add-hook 'sh-mode-hook 'flymake-shellcheck-load)))
