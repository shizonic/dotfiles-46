;;; -*- lexical-binding: t; -*-

;; general binds that are unrelated to modes go here

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-x -") 'bury-buffer)
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-x C-o") 'other-window)
