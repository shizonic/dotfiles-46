;;; -*- lexical-binding: t; -*-

(use-package rainbow-delimiters
  :init
  (rainbow-delimiters-mode 1))

;; all lisp

;; replace insane C-M-prefixed binds with something that doesn't break the wrist
(define-key lisp-mode-shared-map (kbd "s-b") 'paredit-backward)
(define-key lisp-mode-shared-map (kbd "s-f") 'paredit-forward)
(define-key lisp-mode-shared-map (kbd "s-n") 'paredit-forward-up)
(define-key lisp-mode-shared-map (kbd "s-p") 'paredit-backward-down)
(define-key lisp-mode-shared-map (kbd "s-u") 'paredit-backward-up)
(define-key lisp-mode-shared-map (kbd "s-d") 'paredit-forward-down)

;; elisp

(defun my-ielm ()
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'my-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

(add-hook 'ielm-mode-hook 'eldoc-mode)

(use-package elisp-slime-nav
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

;; common lisp

;; (use-package common-lisp-snippets :after yasnippet)

(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(use-package slime
  :config
  (setq slime-default-lisp 'sbcl)
  (setq slime-contribs '(slime-fancy slime-cl-indent))
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t)
  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))

(use-package sly :disabled
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package sly-quicklisp :disabled)
(use-package sly-named-readtables :disabled)
(use-package sly-asdf :disabled)
(use-package sly-package-inferred :disabled
  :straight (sly-package-inferred :type git :host github :repo "40ants/sly-package-inferred"))
