;;; -*- lexical-binding: t; -*-

;; requires language servers be installed to actually do stuff

(or
 (use-package lsp-mode
   :init (setq lsp-keymap-prefix "M-m")
   :config
   (define-key lsp-mode-map (kbd "M-.") 'xref-find-definitions))

 (use-package eglot :disabled
   :init (fset 'lsp 'eglot-ensure)
   :config
   (define-key eglot-mode-map (kbd "C-h .") 'eglot-help-at-point)
   (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)))
