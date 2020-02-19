;;; -*- lexical-binding: t; -*-

(add-to-list 'completion-ignored-extensions ".rbc")

(when (executable-find "solargraph")
  (add-hook 'ruby-mode-hook 'eglot-ensure))

(when (executable-find "rbenv")
  (add-hook 'after-init-hook 'shim-init-ruby)
  (add-hook 'ruby-mode-hook 'shim-mode))

(use-package inf-ruby
  ;; https://github.com/nonsequitur/inf-ruby
  :init
  (with-eval-after-load 'ruby-mode
    (inf-ruby-minor-mode +1)

    ;; CamelCase aware editing operations
    (subword-mode +1)))
