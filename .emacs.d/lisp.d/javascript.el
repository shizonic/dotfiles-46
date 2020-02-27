;;; -*- lexical-binding: t; -*-

(use-package js2-mode
  :init
  (if (< emacs-major-version 27)
      (add-hook 'js-mode-hook 'js2-mode)
    (add-hook 'js-mode-hook 'js2-minor-mode)))

(when (executable-find "javascript-typescript-stdio")
  (add-hook 'js-mode-hook #'lsp))
