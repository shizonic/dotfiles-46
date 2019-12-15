;;; -*- lexical-binding: t; -*-

(use-package keychain-environment
  :init
  (defun pinentry-emacs (desc prompt ok error)
    (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
      str))
  ;; :init (use-package pinentry :config (pinentry-start))
  :config
  (setq password-cache-expiry nil)
  (setq epa-pinentry-mode 'loopback)

  (defun keychain-unlock ()
    (interactive)
    (async-shell-command
     "eval $(keychain --eval --agents ssh,gpg id_rsa 77CF5C5C65A8F9F44940A72CDD4795B51117D906);
emacsclient -e '(keychain-refresh-environment)'"))

  (defun keychain-lock ()
    (interactive)
    (async-shell-command "keychain --agents ssh,gpg -k all")))

(defun region-to-termbin (start end)
  "push the marked region to termbin.com via shell command"
  (interactive "r")
  (message "pushing region to termbin.com...")
  (shell-command-on-region start end "nc termbin.com 9999" "*Termbin*"))

(defun buffer-to-termbin ()
  "push the whole buffer to termbin.com via shell command"
  (interactive)
  (message "pushing buffer to termbin.com...")
  (shell-command-on-region (point-min) (point-max) "nc termbin.com 9999" "*Termbin*"))

(add-hook 'before-save-hook 'whitespace-cleanup)

(show-paren-mode 1)

(electric-pair-mode 1)

(use-package magit
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-repository-directories '(("~/repos" . 1))))

(use-package projectile
  :config
  (projectile-mode 1))

(use-package flycheck)

;;; LANGS

;; shell

(defalias 'troot '(lambda()(interactive)(cd "/su:root@kiss:/root"))) ;; tramp root
(defun eshell/emacs (file)
  (find-file file))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defun my-shell-mode-hook()
  (setq-local compile-command
              '((lambda()
                  (save-buffer)
                  (async-shell-command (buffer-file-name))))))
(add-hook 'sh-mode-hook 'my-shell-mode-hook)

;; C

(defun c-mode-common-defaults ()
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'c-mode-common-defaults)

(defun makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'makefile-mode-defaults)

;; lisp

(defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when hippie expanding in a lisp editing mode"
  (if (and (parinfer-mode) (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

(use-package indent-guide
  :config
  (setq indent-guide-recursive t)
  (indent-guide-global-mode 1))

(use-package parinfer
  :init (use-package lispy)
  (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
  (add-hook 'lisp-mode-hook 'parinfer-mode)
  (add-hook 'common-lisp-mode-hook 'parinfer-mode)
  (add-hook 'slime-repl-mode-hook 'parinfer-mode)
  :config
  (setq parinfer-extensions
        '(defaults
           lispy
           smart-tab
           smart-yank))
  (global-set-key (kbd "C-c ,") 'parinfer-toggle-mode))

(defun my-ielm ()
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'my-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

(require 'ielm)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(use-package elisp-slime-nav
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(use-package slime
  :init
  (add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  :config
  (setq slime-default-lisp 'sbcl)
  (setq slime-contribs '(slime-fancy slime-cl-indent))
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t)

  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))
