;;; -*- lexical-binding: t; -*-

(when (executable-find "solargraph")
  (add-hook 'ruby-mode-hook 'eglot-ensure))

(when (executable-find "rbenv")
  (add-hook 'after-init-hook 'shim-init-ruby)
  (add-hook 'ruby-mode-hook 'shim-mode))
