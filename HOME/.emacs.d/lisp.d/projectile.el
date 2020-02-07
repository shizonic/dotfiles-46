;;; -*- lexical-binding: t; -*-

(use-package projectile
  :init
  (projectile-mode 1)
  (global-set-key (kbd "C-c p") 'projectile-command-map))
