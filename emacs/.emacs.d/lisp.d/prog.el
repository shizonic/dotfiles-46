;;; -*- lexical-binding: t; -*-

(add-hook 'after-init-hook 'projectile-mode)
(add-hook 'prog-mode-hook 'company-mode)
;; (add-hook 'after-init-hook 'global-flycheck-mode) ;; prefer buffer-local, manual enablement for security reasons, I also don't like being harassed.
(add-hook 'after-init-hook 'aggressive-indent-global-mode)

(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'electric-pair-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq grep-command "grep -r ")

;; TODO improve this
(defun region-to-termbin (start end)
  "push the marked region to termbin.com via shell command"
  (interactive "r")
  (message "pushing region to termbin.com...")
  (shell-command-on-region start end "nc termbin.com 9999")
  (switch-to-buffer "*Messages*"))

;; TODO improve this
(defun buffer-to-termbin ()
  "push the whole buffer to termbin.com via shell command"
  (interactive)
  (message "pushing buffer to termbin.com...")
  (shell-command-on-region (point-min) (point-max) "nc termbin.com 9999")
  (switch-to-buffer "*Messages*"))

(setq magit-diff-refine-hunk t)
(setq magit-repository-directories '(("~/repos" . 1)))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(setq company-idle-delay 0.0)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above t)

;; allow me to decline a suggestion and keep typing through as normal
(with-eval-after-load 'company
  (define-key company-active-map (kbd "SPC") nil))
(setq company-auto-complete-chars nil)