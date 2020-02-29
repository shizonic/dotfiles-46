;;; -*- lexical-binding: t; -*-

(use-package better-shell
  :commands better-shell-for-current-dir
  :init
  (bind-key* (kbd "C-t") 'better-shell-for-current-dir))
