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

(with-eval-after-load 'desktop-environment
  (setf ;; https://github.com/DamienCassou/desktop-environment/issues/1

         (alist-get (elt (kbd "s-l") 0) desktop-environment-mode-map nil t)
         nil)
  (exwm-input-set-key (kbd "s-l") 'enlarge-window-horizontally))

(exwm-input-set-key (kbd "s-h") 'shrink-window-horizontally)
(exwm-input-set-key (kbd "s-j") 'enlarge-window)
(exwm-input-set-key (kbd "s-k") 'shrink-window)

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

(exwm-input-set-key
 (kbd "<s-kp-add>") 'desktop-environment-volume-increment)
(exwm-input-set-key
 (kbd "<s-kp-subtract>") 'desktop-environment-volume-decrement)

(exwm-input-set-key (kbd "<XF86PowerOff>")
                    (lambda()
                      (interactive)
                      (root)
                      (async-shell-command "poweroff")))

(exwm-input-set-key (kbd "<f9>") 'exwm-input-toggle-keyboard)

(exwm-input-set-key (kbd "<f12>") (lambda()
                                    (interactive)
                                    (my-tramp-root-switcher)))

(exwm-input-set-key (kbd "s-1") 'delete-other-windows)
(exwm-input-set-key (kbd "s-2") 'split-window-below)
(exwm-input-set-key (kbd "s-3") 'split-window-right)
(exwm-input-set-key (kbd "s-0") 'delete-window)
(exwm-input-set-key (kbd "s-x") '(lambda ()
                                   (interactive)
                                   (kill-buffer (current-buffer))))
(exwm-input-set-key (kbd "s-u") 'winner-undo)
(exwm-input-set-key (kbd "s-U") 'winner-redo)
(exwm-input-set-key (kbd "s-r") 'rotate-frame-anticlockwise)
(exwm-input-set-key (kbd "s-o") 'other-window)
(exwm-input-set-key (kbd "s-O") 'spacemacs/alternate-window)
(exwm-input-set-key (kbd "<s-tab>") 'spacemacs/alternate-buffer)

(exwm-input-set-key (kbd "s-t") 'eshell-here)
(exwm-input-set-key (kbd "s-s") 'crux-create-scratch-buffer)
(exwm-input-set-key (kbd "s-m") 'my-interactive-menu)

;; redshift
(exwm-input-set-key (kbd "<s-right>")
                    (lambda()
                      (interactive)
                      (my-redshift-setter 1)))
(exwm-input-set-key (kbd "<s-left>")
                    (lambda()
                      (interactive)
                      (my-redshift-setter -1)))

;; brightness
(exwm-input-set-key (kbd "<s-up>")
                    (lambda()
                      (interactive)
                      (desktop-environment-brightness-increment)))
(exwm-input-set-key (kbd "<s-down>")
                    (lambda()
                      (interactive)
                      (desktop-environment-brightness-decrement)))

;; dmenu
(exwm-input-set-key (kbd "s-d")
                    (lambda()
                      (interactive)
                      (call-process-shell-command "dmenu_run")))

;; slock
(exwm-input-set-key (kbd "<s-escape>")
                    (lambda()
                      (interactive)
                      (start-process-shell-command "slock" nil "slock")))

(exwm-input-set-key (kbd "s-T")
                    (lambda()
                      (interactive)
                      (start-process-shell-command "st" nil "st")))

;; font resize
(global-set-key (kbd "<C-kp-add>")
                (lambda()(interactive)(my-font-resizer 1)))
(global-set-key (kbd "<C-kp-subtract>")
                (lambda()(interactive)(my-font-resizer -1)))

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "^") 'eww-open-yt-dl))
