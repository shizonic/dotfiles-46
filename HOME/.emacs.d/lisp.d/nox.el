;;; -*- lexical-binding: t; -*-

(defun xclip-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))

(defun xclip-paste-function ()
  (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
    (unless (string= (car kill-ring) xclip-output)
      xclip-output)))

(use-package awesome-tray :demand
  :straight (awesome-tray :type git :host github :repo "manateelazycat/awesome-tray")
  :config
  (awesome-tray-mode 1)
  (setq-default mode-line-format nil))

(or window-system
    (progn
      (global-set-key (kbd "C-x ;") 'comment-line)
      (menu-bar-mode -1)
      (setq-default default-frame-alist '((tty-color-mode . never)))
      (custom-set-faces '(font-lock-function-name-face ((t (:weight bold)))))
      (and (getenv "DISPLAY")
           (progn
             (xterm-mouse-mode 1)
             (global-set-key (kbd "<mouse-4>") '(lambda () (interactive) (scroll-down 5)))
             (global-set-key (kbd "<mouse-5>") '(lambda () (interactive) (scroll-up 5)))
             (setq interprogram-cut-function 'xclip-cut-function)
             (setq interprogram-paste-function 'xclip-paste-function)))))
