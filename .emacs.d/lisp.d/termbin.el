;;; -*- lexical-binding: t; -*-

;; TODO improve me

(defun region-to-termbin (start end)
  "push the marked region to termbin.com via shell command"
  (interactive "r")
  (message "pushing region to termbin.com...")
  (shell-command-on-region start end "nc termbin.com 9999"))

(defun buffer-to-termbin ()
  "push the whole buffer to termbin.com via shell command"
  (interactive)
  (message "pushing buffer to termbin.com...")
  (shell-command-on-region (point-min) (point-max) "nc termbin.com 9999"))

(global-set-key (kbd "C-c t r") 'region-to-termbin)
(global-set-key (kbd "C-c t b") 'buffer-to-termbin)
