;;; -*- lexical-binding: t; -*-

;; js-mode in Emacs 27 includes full support for syntax highlighting and indenting of JSX synta, but fallback to js2-mode
;; use lsp for some auto complete, linting, and jump to def, etc...

(when (< emacs-major-version 27)
  (use-package js2-mode
    :init (add-hook 'js-mode-hook 'js2-mode)))

(when (executable-find "javascript-typescript-stdio")
  (add-hook 'js-mode-hook #'lsp))

(when (executable-find "nodenv")
  (add-hook 'after-init-hook #'shim-init-node)
  (add-hook 'js-mode-hook #'shim-mode))
