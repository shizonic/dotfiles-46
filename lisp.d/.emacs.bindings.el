;;; -*- lexical-binding: t; -*-
;; nearly all of my binds are in this file
;; except some that are in .emacs.programming.el

;; Swapping lctl and lalt is the easiest way to make Emacs ergo-friendly.
;; And Place a helpful hydra menu on Caps Lock.
(start-process-shell-command "setxkbmap" nil "setxkbmap -option ctrl:swap_lalt_lctl -option caps:menu")

;; Faster keyboard repeat
(start-process-shell-command "xset" nil "xset r rate 200 60")

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-x TAB") 'spacemacs/alternate-buffer)
(global-set-key (kbd "<menu>") 'windows-hydra/body)
(global-set-key (kbd "<home>") 'keychain-unlock)
(global-set-key (kbd "<end>") 'keychain-lock)
(global-set-key (kbd "C-c i") 'my-erc)
(global-set-key (kbd "C-c b") 'eww)
(global-set-key (kbd "C-c m") 'gnus)
(global-set-key (kbd "C-c $") 'eshell)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<C-kp-add>") (lambda()(interactive)(my-font-resizer 1)))
(global-set-key (kbd "<C-kp-subtract>") (lambda()(interactive)(my-font-resizer -1)))
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c f") 'flycheck-mode)
(global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)

(global-set-key (kbd "C-c C-;") 'comment-line)
(global-set-key (kbd "C-c t r") 'region-to-termbin)
(global-set-key (kbd "C-c t b") 'buffer-to-termbin)
(global-set-key (kbd "<s-return>")
                '(lambda (command)
                   (interactive (list (read-shell-command "$ ")))
                   (start-process-shell-command command nil command)))

(use-package crux
  :config
  (global-set-key (kbd "C-c k") 'crux-kill-whole-line)
  (global-set-key (kbd "<C-backspace>") 'crux-kill-line-backwards)
  (global-set-key (kbd "C-c #") 'crux-create-scratch-buffer)
  (global-set-key (kbd "C-o") 'crux-smart-open-line-above)
  (global-set-key (kbd "C-j") 'crux-smart-open-line)
  (global-set-key (kbd "C-c k") 'crux-recentf-find-file)
  (global-set-key (kbd "C-c R") 'crux-rename-buffer-and-file)
  (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c D") 'crux-delete-buffer-and-file)
  (global-set-key (kbd "C-c K") 'crux-kill-other-buffers)
  (global-set-key (kbd "C-c I") 'crux-find-user-init-file))

(with-eval-after-load 'exwm
    (exwm-input-set-key (kbd "<s-return>") ;; a simple launcher
                '(lambda (command)
                   (interactive (list (read-shell-command "$ ")))
                   (start-process-shell-command command nil command)))

  (exwm-input-set-key (kbd "C-x TAB") 'spacemacs/alternate-buffer)
  (exwm-input-set-key (kbd "<f9>") 'exwm-input-toggle-keyboard)
  (exwm-input-set-key (kbd "<menu>") 'windows-hydra/body))

(use-package browse-kill-ring :config
  (global-set-key (kbd "M-y") 'browse-kill-ring))

(use-package hydra
  :init (use-package transpose-frame)
  :config
  (defhydra windows-hydra (:exit nil)
    ("h" (call-interactively 'shrink-window-horizontally) "shrink-left")
    ("j" (call-interactively 'shrink-window) "shrink-down")
    ("k" (call-interactively 'enlarge-window) "grow-up")
    ("l" (call-interactively 'enlarge-window-horizontally) "grow-right")
    ("r" (rotate-frame-anticlockwise) "rotate")
    ("o" (call-interactively 'other-window))
    ("1" (call-interactively 'delete-other-windows))
    ("2" (call-interactively 'split-window-below))
    ("3" (call-interactively 'split-window-right))
    ("0" (call-interactively 'delete-window))
    ("<menu>" nil)))

(with-eval-after-load 'exwm
  (exwm-input-set-key
   (kbd "<s-kp-multiply>") 'desktop-environment-toggle-mute)
  (exwm-input-set-key
    (kbd "<s-kp-add>") 'desktop-environment-volume-increment)
  (exwm-input-set-key
    (kbd "<s-kp-subtract>") 'desktop-environment-volume-decrement))

(global-set-key (kbd "M-+") (lambda()(interactive)(my-redshift-setter 1)))
(global-set-key (kbd "M--") (lambda()(interactive)(my-redshift-setter -1)))
(global-set-key (kbd "<M-kp-multiply>") (lambda()(interactive)
                                          (setq redshift '1000)
                                          (start-process-shell-command "redshift" nil "redshift -x")))

(global-set-key (kbd "<C-kp-add>") (lambda()(interactive)(my-font-resizer 1)))
(global-set-key (kbd "<C-kp-subtract>") (lambda()(interactive)(my-font-resizer -1)))
