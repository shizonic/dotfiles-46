;;; -*- lexical-binding: t; -*-

(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

;; general binds that are unrelated to modes go here

;; unbind annoyances
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; replace insane C-M-prefixed binds with something that doesn't break the wrist
(global-set-key (kbd "s-b") 'backward-sexp)
(global-set-key (kbd "s-f") 'forward-sexp)
(global-set-key (kbd "s-n") 'forward-list)
(global-set-key (kbd "s-p") 'backward-list)
(global-set-key (kbd "s-u") 'backward-up-list)
(global-set-key (kbd "s-d") 'down-list)
(global-set-key (kbd "s-SPC") 'mark-sexp)

;; windows and buffers
(global-set-key (kbd "C-x C-o") 'spacemacs/alternate-window)
(global-set-key (kbd "C-x C-b") 'spacemacs/alternate-buffer)
(global-set-key (kbd "<s-backspace>") 'bury-buffer)
(global-set-key (kbd "<s-return>") 'dmenu)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-0") 'delete-window)

(with-eval-after-load 'exwm
  (exwm-input-set-key (kbd "C-x C-o") 'spacemacs/alternate-window)
  (exwm-input-set-key (kbd "C-x C-b") 'spacemacs/alternate-buffer)
  (exwm-input-set-key (kbd "<s-backspace>") 'bury-buffer)
  (exwm-input-set-key (kbd "<s-return>") 'dmenu)
  (exwm-input-set-key (kbd "C-1") 'delete-other-windows)
  (exwm-input-set-key (kbd "C-2") 'split-window-below)
  (exwm-input-set-key (kbd "C-3") 'split-window-right)
  (exwm-input-set-key (kbd "C-0") 'delete-window))

;; misc
(global-set-key (kbd "<f5>") 'compile)
