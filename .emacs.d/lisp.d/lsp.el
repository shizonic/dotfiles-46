;;; -*- lexical-binding: t; -*-

;; requires language servers be installed to actually do stuff

(use-package eglot
  :init (fset 'lsp 'eglot-ensure)
  :config
  (define-key eglot-mode-map (kbd "C-h .") 'eglot-help-at-point)
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions))
