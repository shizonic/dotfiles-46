;;; -*- lexical-binding: t; -*-

;; remove annoyances
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; minor modes may override
(bind-key "C-a" 'crux-move-beginning-of-line)

;; minor modes may not override
(bind-key* "C-c I" #'(lambda ()
                       (interactive)
                       (find-file user-init-file)))
(bind-key* "C-x C-b" 'ido-switch-buffer)
(bind-key* "M-y" 'browse-kill-ring)
(bind-key* "C-c p" 'projectile-command-map)
(bind-key* "C-c g" 'magit-status)
(bind-key* "C-c f" 'flycheck-mode)
(bind-key* "C-c t r" 'region-to-termbin)
(bind-key* "C-c t b" 'buffer-to-termbin)
(bind-key* "C-o" 'crux-smart-open-line)
(bind-key* "C-c C-l" 'crux-duplicate-current-line-or-region)
(bind-key* "C-c -" 'crux-kill-whole-line)
(bind-key* "C-x -" 'bury-buffer)
(bind-key* "C-c ;" 'crux-duplicate-and-comment-current-line-or-region) ; because -nox
(bind-key* "C-x ;" 'comment-line)                                      ; because -nox
(bind-key* "<f5>" 'compile)
(bind-key "M-/" 'hippie-expand)
(bind-key* "C-t" 'eshell)

(bind-key* (kbd "<mouse-4>") '(lambda () (interactive) (scroll-down 5)))
(bind-key* (kbd "<mouse-5>") '(lambda () (interactive) (scroll-up 5)))
