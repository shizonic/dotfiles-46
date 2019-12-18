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

(windmove-default-keybindings)

(global-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(global-set-key (kbd "<menu>") 'menu-hydra/body)
(global-set-key (kbd "<home>") (lambda()(interactive)(toor)(keychain-unlock)))
(global-set-key (kbd "<end>") (lambda()(interactive)(toor)(keychain-lock)))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<C-kp-add>") (lambda()(interactive)(my-font-resizer 1)))
(global-set-key (kbd "<C-kp-subtract>") (lambda()(interactive)(my-font-resizer -1)))
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c f") 'flycheck-mode)

(global-set-key (kbd "C-c C-;") 'comment-line)
(global-set-key (kbd "C-c t r") 'region-to-termbin)
(global-set-key (kbd "C-c t b") 'buffer-to-termbin)

(global-set-key (kbd "C-c I") 'crux-find-user-init-file)
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
(global-set-key (kbd "C-c C-k") 'crux-kill-whole-line)
(global-set-key (kbd "<C-backspace>") 'crux-kill-line-backwards)
(global-set-key (kbd "C-o") 'crux-smart-open-line-above)
(global-set-key (kbd "C-j") 'crux-smart-open-line)
(global-set-key (kbd "C-c C-l") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c C-;") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "M-y") 'browse-kill-ring)

(defhydra menu-hydra (:exit t)
  "<Menu>"
  ("w" (call-interactively 'windows-hydra/body) "win")
  ("b" (call-interactively 'eww) "eww")
  ("B" (call-interactively 'eww-browse-with-external-browser) "ext. browse")
  ("g" (call-interactively 'gnus) "gnus")
  ("a" (call-interactively 'abook) "abook")
  ("k" (call-interactively 'kiss-hydra/body) "kiss")
  ("e" (call-interactively 'my-erc) "erc")
  ("$" (call-interactively 'eshell-here) "eshell")
  ("#" (call-interactively 'crux-create-scratch-buffer) "scratch")
  ("m" (call-interactively 'emms-hydra/body) "eMMs")
  ("<menu>" nil))

(defhydra windows-hydra (:exit nil)
  "Window"
  ("h" (call-interactively 'shrink-window-horizontally))
  ("j" (call-interactively 'shrink-window))
  ("k" (call-interactively 'enlarge-window))
  ("l" (call-interactively 'enlarge-window-horizontally))
  ("r" (rotate-frame-anticlockwise))
  ("o" (call-interactively 'other-window))
  ("1" (call-interactively 'delete-other-windows))
  ("2" (call-interactively 'split-window-below))
  ("3" (call-interactively 'split-window-right))
  ("0" (call-interactively 'delete-window))
  ("<menu>" nil))

(exwm-input-set-key
 (kbd "<s-kp-multiply>") 'deskktop-environment-toggle-mute)
(exwm-input-set-key
 (kbd "<s-kp-add>") 'desktop-environment-volume-increment)
(exwm-input-set-key
 (kbd "<s-kp-subtract>") 'desktop-environment-volume-decrement)

(exwm-input-set-key (kbd "<XF86PowerOff>")
                    (lambda()
                      (interactive)
                      (root)
                      (async-shell-command "poweroff")))

(exwm-input-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(exwm-input-set-key (kbd "<f9>") 'exwm-input-toggle-keyboard)

(exwm-input-set-key (kbd "<f12>") (lambda()
                                    (interactive)
                                    (my-tramp-root-switcher)))
(exwm-input-set-key (kbd "<menu>") 'menu-hydra/body)


(exwm-input-set-key (kbd "<s-right>") (lambda()(interactive)(my-redshift-setter 1)))
(exwm-input-set-key (kbd "<s-left>") (lambda()(interactive)(my-redshift-setter -1)))
(exwm-input-set-key (kbd "<s-up>") (lambda()(interactive)(desktop-environment-brightness-increment)))
(exwm-input-set-key (kbd "<s-down>") (lambda()(interactive)(desktop-environment-brightness-decrement)))


(global-set-key (kbd "<C-kp-add>") (lambda()(interactive)(my-font-resizer 1)))
(global-set-key (kbd "<C-kp-subtract>") (lambda()(interactive)(my-font-resizer -1)))
