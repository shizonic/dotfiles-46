;;; -*- lexical-binding: t; -*-

(or window-system
    (progn
      (global-set-key (kbd "C-x ;") 'comment-line)
      (and (getenv "DISPLAY")
           (defun xclip-cut-function (text &optional push)
             (with-temp-buffer
               (insert text)
               (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))

           (defun xclip-paste-function ()
             (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
               (unless (string= (car kill-ring) xclip-output)
                 xclip-output)))
           (progn
             (xterm-mouse-mode 1)
             (global-set-key (kbd "<mouse-4>") '(lambda () (interactive) (scroll-down 5)))
             (global-set-key (kbd "<mouse-5>") '(lambda () (interactive) (scroll-up 5)))
             (setq interprogram-cut-function 'xclip-cut-function)
             (setq interprogram-paste-function 'xclip-paste-function)))))
