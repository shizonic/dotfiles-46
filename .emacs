;;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold 100000000
      debug-on-error nil
      custom-file "/dev/null"
      user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen nil)

(load-file "~/.emacs.use-melpa.el")

(use-package better-defaults
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t
        visible-bell nil
        tab-always-indent 'complete
        tramp-default-method "ssh"
        vc-follow-symlinks t
        tramp-copy-size-limit nil
        browse-url-browser-function 'eww-browse-url
        save-interprogram-paste-before-kill t)

  (delete-selection-mode 1)

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  (setq-default indent-tabs-mode nil
                tab-width 8
                fill-column 80)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq dired-auto-revert-buffer t)
  ;; (require 'dired-x)
  ;; (add-hook 'dired-load-hook
  ;;           (function (lambda () (load "dired-x"))))

  ;; (with-eval-after-load 'async
  ;;   (autoload 'dired-async-mode "dired-async.el" nil t)
  ;;   (dired-lasync-mode 1))

  (winner-mode 1)

  (defun spacemacs/alternate-buffer (&optional window)
    (interactive)
    (let ((current-buffer (window-buffer window)))
      (switch-to-buffer
       (cl-find-if (lambda (buffer)
                     (not (eq buffer current-buffer)))
                   (mapcar #'car (window-prev-buffers window)))))))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(global-set-key (kbd "C-c i") 'my-erc)
(global-set-key (kbd "C-c b") 'eww)
(global-set-key (kbd "C-c m") 'gnus)
(global-set-key (kbd "C-c s") '(lambda () (interactive)(shell)(delete-other-windows)))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<C-kp-add>") (lambda()(interactive)(my-font-resizer 1)))
(global-set-key (kbd "<C-kp-subtract>") (lambda()(interactive)(my-font-resizer -1)))
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-c ;") 'comment-line)
(global-set-key (kbd "C-c t r") 'region-to-termbin)
(global-set-key (kbd "C-c t b") 'buffer-to-termbin)
(global-set-key (kbd "<s-return>")
                '(lambda (command)
                   (interactive (list (read-shell-command "$ ")))
                   (start-process-shell-command command nil command)))

(use-package browse-kill-ring :bind (("M-y" . browse-kill-ring)))

(use-package crux
  :defer t
  :bind (("C-c r" . crux-rename-buffer-and-file)
         ("C-c k" . crux-kill-whole-line)
         ("C-c $" . crux-create-scratch-buffer)
         ("C-o" . crux-smart-open-line-above)
         ("C-j" . crux-smart-open-line)
         ("C-c r" . crux-recentf-find-file)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c D" . crux-delete-buffer-and-file)
         ("C-c K" . crux-kill-other-buffers)
         ("C-c I" . crux-find-user-init-file)))

(use-package hydra
  :init (use-package transpose-frame :defer t)
  :bind (("<menu>" . windows-hydra/body))
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

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package edit-server :defer t
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package keychain-environment
  :init (use-package pinentry :config (pinentry-start))
  :config
  (setq password-cache-expiry nil)
  (setq epa-pinentry-mode 'loopback))
  ;; (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version)))

(load-file "~/.emacs.code.el")
(load-file "~/.emacs.gnus.el")
(load-file "~/.emacs.erc.el")
(load-file "~/.emacs.theme.el")
(load-file "~/.emacs.exwm.el")
