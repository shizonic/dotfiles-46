;;; -*- lexical-binding: t; -*-

(use-package projectile
  :init
  (add-hook 'after-init-hook 'projectile-mode)
  (global-set-key (kbd "C-c p") 'projectile-command-map))
