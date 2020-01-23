;; unbind
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; minor modes may override
(bind-key "C-a" 'crux-move-beginning-of-line)
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)

;; minor modes may not override (global)
(bind-key* "C-x C-b" 'ido-switch-buffer)
(bind-key* "M-y" 'browse-kill-ring)
(bind-key* "C-c a" 'abook)
(bind-key* "C-c e" 'gnus)
(bind-key* "C-c m" 'my-emms)
(bind-key* "C-c b" 'eww)
(bind-key* "C-c B" (lambda ()
                     (interactive)
                     (call-process-shell-command "surf google.com &" nil nil 0)))
(bind-key* "C-c i" 'freenode)
(bind-key* "C-c p" 'projectile-command-map)
(bind-key* "C-c g" 'magit-status)
(bind-key* "C-c f" 'flycheck-mode)
(bind-key* "C-c t r" 'region-to-termbin)
(bind-key* "C-c t b" 'buffer-to-termbin)
(bind-key* "C-c I" (lambda () (interactive) (find-file user-init-file)))
(bind-key* "C-o" 'crux-smart-open-line)
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'crux-kill-whole-line)))
(bind-key* "C-c C-l" 'crux-duplicate-current-line-or-region)
(bind-key* "C-c C-;" 'crux-duplicate-and-comment-current-line-or-region)
(bind-key* "C-;" 'comment-line)
(bind-key* "<f5>" 'compile)
(bind-key* "C-t" 'eshell)
(bind-key* "C-c #" (lambda () (interactive) (supershell)))
(bind-key "M-/" 'hippie-expand)

(bind-key* "<C-tab>" 'spacemacs/alternate-buffer)
(bind-key* "C-`" 'spacemacs/alternate-window)
(bind-key* "C--" 'bury-buffer)
(bind-key* "<home>" 'unlock)
(bind-key* "<end>" 'lock)

(exwm-input-set-key (kbd "<f9>") 'exwm-input-toggle-keyboard)
(bind-key* "<f9>" 'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(bind-key* "<C-tab>" 'spacemacs/alternate-buffer)
(exwm-input-set-key (kbd "<C-return>") 'spacemacs/alternate-window)
(bind-key* "<C-return>" 'spacemacs/alternate-window)
(exwm-input-set-key (kbd "C-1") 'delete-other-windows)
(bind-key* "C-1" 'delete-other-windows)
(exwm-input-set-key (kbd "C-2") 'split-window-below)
(bind-key* "C-2" 'split-window-below)
(exwm-input-set-key (kbd "C-3") 'split-window-right)
(bind-key* "C-3" 'split-window-right)
(exwm-input-set-key (kbd "C-0") 'delete-window)
(bind-key* "C-0" 'delete-window)
(exwm-input-set-key (kbd "C--") 'bury-buffer)
(bind-key* "C--" 'bury-buffer)

(exwm-input-set-key (kbd "<C-up>") 'enlarge-window)
(bind-key* "<C-up>" 'enlarge-window)
(exwm-input-set-key (kbd "<C-down>") 'shrink-window)
(bind-key* "<C-down>" 'shrink-window)
(exwm-input-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
(bind-key* "<C-right>" 'enlarge-window-horizontally)
(exwm-input-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(bind-key* "<C-left>" 'shrink-window-horizontally)
(exwm-input-set-key (kbd "<menu>") 'dmenu)

(exwm-input-set-key (kbd "<s-kp-add>") 'desktop-environment-volume-increment)
(exwm-input-set-key (kbd "<s-kp-subtract>") 'desktop-environment-volume-decrement)
(exwm-input-set-key (kbd "<S-s-kp-add>") 'desktop-environment-brightness-increment)
(exwm-input-set-key (kbd "<S-s-kp-subtract>") 'desktop-environment-brightness-decrement)
(exwm-input-set-key (kbd "<S-s-kp-subtract>") 'desktop-environment-brightness-decrement)
