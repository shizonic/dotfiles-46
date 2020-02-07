(use-package slime
  :init
  (add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (setq slime-default-lisp 'sbcl)
  (setq slime-contribs '(slime-fancy slime-cl-indent))
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t)
  :config
  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))
