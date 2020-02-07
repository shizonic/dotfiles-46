;;; -*- lexical-binding: t; -*-

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-c I") #'(lambda ()
                                  (interactive)
                                  (find-file user-init-file)))
(global-set-key (kbd "C-x -") 'bury-buffer)
(global-set-key (kbd "<f5>") 'compile)
