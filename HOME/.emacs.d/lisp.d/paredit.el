;;; -*- lexical-binding: t; -*-

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook #'enable-paredit-mode))

(defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when hippie expanding in a lisp editing mode"
  (if (and paredit-mode
           (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))
