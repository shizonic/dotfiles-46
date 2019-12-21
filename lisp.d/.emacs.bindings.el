;;; -*- lexical-binding: t; -*-

;; nearly all of my binds are here

;; Swapping lctl and lalt is the easiest way to make Emacs ergo-friendly.
;; Also swap <caps> with <menu> and bind to M-x Compile.
(start-process-shell-command
 "setxkbmap" nil "setxkbmap -option ctrl:swap_lalt_lctl -option caps:menu")

(global-set-key (kbd "<menu>") 'compile)

;; Faster keyboard repeat
(start-process-shell-command
 "xset" nil "xset r rate 200 60")

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "<home>") (lambda()(interactive)(toor)(keychain-unlock)))
(global-set-key (kbd "<end>") (lambda()(interactive)(toor)(keychain-lock)))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<C-kp-add>") (lambda()(interactive)(my-font-resizer 1)))
(global-set-key (kbd "<C-kp-subtract>") (lambda()(interactive)(my-font-resizer -1)))
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c f") 'flycheck-mode)

(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "s-p") 'backward-paragraph)
(global-set-key (kbd "s-n") 'forward-paragraph)
(global-set-key (kbd "C-c t r") 'region-to-termbin)
(global-set-key (kbd "C-c t b") 'buffer-to-termbin)

(global-set-key (kbd "C-c I") 'crux-find-user-init-file)
(global-set-key (kbd "C-c s") 'eshell-here)
(global-set-key (kbd "C-c S") 'my-su-edit)
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
(global-set-key (kbd "C-c C-k") 'crux-kill-whole-line)
(global-set-key (kbd "<C-backspace>") 'crux-kill-line-backwards)
(global-set-key (kbd "C-S-o") 'crux-smart-open-line-above)
(global-set-key (kbd "C-o") 'crux-smart-open-line)
(global-set-key (kbd "C-c C-l") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c C-;") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key "%" 'match-paren)
(global-set-key (kbd "M-y") 'browse-kill-ring)

(global-set-key (kbd "C-c m") 'menu)

(global-set-key (kbd "C-t") 'eshell-here)
(define-key dired-mode-map (kbd "C-t") 'eshell-here)
(define-key org-mode-map (kbd "C-t") 'eshell-here)
(global-set-key (kbd "C-x TAB") 'spacemacs/alternate-buffer)
(global-set-key (kbd "C-x w") 'spacemacs/alternate-window)

;; redshift
(global-set-key (kbd "<s-right>")
                (lambda()
                  (interactive)
                  (my-redshift-setter 1)))
(global-set-key (kbd "<s-left>")
                (lambda()
                  (interactive)
                  (my-redshift-setter -1)))

;; font resize
(global-set-key (kbd "<C-kp-add>")
                (lambda()(interactive)(my-font-resizer 1)))
(global-set-key (kbd "<C-kp-subtract>")
                (lambda()(interactive)(my-font-resizer -1)))

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "^") 'eww-open-yt-dl))
