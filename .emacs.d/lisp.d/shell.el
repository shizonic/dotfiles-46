;;; -*- lexical-binding: t; -*-

(use-package better-shell
  :commands better-shell-for-current-dir
  :init
  (bind-key* (kbd "s-t") 'better-shell-for-current-dir))

(use-package emacs-piper :disabled
  ;; TODO -- this looks fun
  :straight (emacs-piper :type git :host gitlab :repo "howardabrams/emacs-piper"))

(use-package flymake-shellcheck
  :after flymake
  :commands flymake-shellcheck-load
  :init
  (when (executable-find "shellcheck")
    (add-hook 'sh-mode-hook 'flymake-shellcheck-load)))
