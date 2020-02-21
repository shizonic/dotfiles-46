;;; -*- lexical-binding: t; -*-

(add-to-list 'completion-ignored-extensions ".rbc")

(when (executable-find "solargraph")
  (add-hook 'ruby-mode-hook #'lsp))

(with-eval-after-load 'dap-mode
  (require 'dap-ruby))

(when (executable-find "rbenv")
  (add-hook 'after-init-hook #'shim-init-ruby)
  (add-hook 'ruby-mode-hook #'shim-mode))

(use-package inf-ruby
  ;; https://github.com/nonsequitur/inf-ruby
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'ruby-mode-hook 'subword-mode))
