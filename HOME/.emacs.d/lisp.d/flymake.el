;;; -*- lexical-binding: t; -*-

(use-package flymake
  :init
  (add-hook 'prog-mode-hook #'(lambda ()
                                (flymake-mode 1)))
  :config
  (global-set-key (kbd "C-?") 'flymake-show-diagnostics-buffer)
  (global-set-key (kbd "C->") 'flymake-goto-next-error)
  (global-set-key (kbd "C-<") 'flymake-goto-prev-error))
