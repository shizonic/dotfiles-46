;;; -*- lexical-binding: t; -*-

;; remove annoyances
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; minor modes may override
(bind-key "C-a" 'crux-move-beginning-of-line)

;; minor modes may not override
(bind-key* "C-x C-b" 'ido-switch-buffer)
(bind-key* "M-y" 'browse-kill-ring)
(bind-key* "C-c p" 'projectile-command-map)
(bind-key* "C-c g" 'magit-status)
(bind-key* "C-c f" 'flycheck-mode)
(bind-key* "C-c t r" 'region-to-termbin)
(bind-key* "C-c t b" 'buffer-to-termbin)
(bind-key* "C-c I" 'crux-find-user-init-file)
(bind-key* "C-c S" 'crux-find-shell-init-file)
(bind-key* "C-o" 'crux-smart-open-line)
(bind-key* "C-c C-l" 'crux-duplicate-current-line-or-region)
(bind-key* "C-c -" 'crux-kill-whole-line)
(bind-key* "C-x -" 'bury-buffer)
(bind-key* "C-c ;" 'crux-duplicate-and-comment-current-line-or-region) ; because -nox
(bind-key* "C-x ;" 'comment-line)                                      ; because -nox
(bind-key* "<f5>" 'compile)
(bind-key* "M-/" 'hippie-expand)

(bind-key* "C-t" 'better-shell-for-current-dir)
(bind-key* "C-c T" 'better-shell-remote-open)
(bind-key* "C-c P" 'better-shell-for-projectile-root)
(bind-key* "C-c s" 'better-shell-sudo-here)

(bind-key* (kbd "<mouse-4>") '(lambda () (interactive) (scroll-down 5)))
(bind-key* (kbd "<mouse-5>") '(lambda () (interactive) (scroll-up 5)))
