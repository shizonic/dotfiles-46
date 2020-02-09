;;; -*- lexical-binding: t; -*-

(use-package common-lisp-snippets)

(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(use-package slime :disabled
  :config
  (setq slime-default-lisp 'sbcl)
  (setq slime-contribs '(slime-fancy slime-cl-indent))
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t)
  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))

(use-package sly
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package sly-quicklisp)
(use-package sly-named-readtables)
(use-package sly-asdf)
(use-package sly-package-inferred
  :straight (sly-package-inferred :type git :host github :repo "40ants/sly-package-inferred"))
