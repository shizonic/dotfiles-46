;;; -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook #'(lambda ()
                              (flymake-mode 1)))

(global-set-key (kbd "C-!") 'flymake-show-diagnostics-buffer)
(global-set-key (kbd "C->") 'flymake-goto-next-error)
(global-set-key (kbd "C-<") 'flymake-goto-prev-error)
