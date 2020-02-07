;;; -*- lexical-binding: t; -*-

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
