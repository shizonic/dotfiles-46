;;; -*- lexical-binding: t; -*-

;; requires language servers be on PATH to actually do stuff

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "M-m"))

(use-package lsp-ui
  :config
  (require 'lsp-ui-imenu)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "M-.") 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "M-m ?") 'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "M-m r") 'lsp-rename)
  (define-key lsp-ui-mode-map (kbd "M-m x") 'lsp-restart-workspace)
  (define-key lsp-ui-mode-map (kbd "M-m w") 'lsp-ui-peek-find-workspace-symbol)
  (define-key lsp-ui-mode-map (kbd "M-m i") 'lsp-ui-peek-find-implementation)
  (define-key lsp-ui-mode-map (kbd "M-m d") 'lsp-describe-thing-at-point)
  (define-key lsp-ui-mode-map (kbd "M-m e") 'lsp-execute-code-action)

  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-always-show t))

(use-package company-lsp)
