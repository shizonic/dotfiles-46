;;; -*- lexical-binding: t; -*-

;; requires language server be installed https://github.com/joaotavora/eglot#connecting-to-a-server

(use-package eglot
  :config
  (define-key eglot-mode-map (kbd "C-h .") 'eglot-help-at-point)
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions))
