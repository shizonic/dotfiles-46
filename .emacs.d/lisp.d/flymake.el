;;; -*- lexical-binding: t; -*-

(use-package flymake
  :init
  (global-set-key (kbd "C-c f") 'flymake-mode)
  :config
  (global-set-key (kbd "C-?") 'flymake-show-diagnostics-buffer)
  (global-set-key (kbd "C->") 'flymake-goto-next-error)
  (global-set-key (kbd "C-<") 'flymake-goto-prev-error))
