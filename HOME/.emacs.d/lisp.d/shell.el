;;; -*- lexical-binding: t; -*-

(use-package better-shell :demand t
  :init
  (global-set-key (kbd "C-t") 'better-shell-for-current-dir)
  (global-set-key (kbd "C-c T") 'better-shell-remote-open)
  (global-set-key (kbd "C-c P") 'better-shell-for-projectile-root)
  (global-set-key (kbd "C-c s") 'better-shell-sudo-here))

(use-package emacs-piper
  :straight (emacs-piper :type git :host gitlab :repo "howardabrams/emacs-piper"))
