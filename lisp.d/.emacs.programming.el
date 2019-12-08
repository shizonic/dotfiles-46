;;; -*- lexical-binding: t; -*-

(use-package keychain-environment
  :init (use-package pinentry :config (pinentry-start))
  :config
  (setq password-cache-expiry nil)
  (setq epa-pinentry-mode 'loopback)

  (defun keychain-unlock ()
    (interactive)
    (async-shell-command
     "eval $(keychain --eval --agents ssh,gpg id_rsa 77CF5C5C65A8F9F44940A72CDD4795B51117D906) \
  && emacsclient -e '(keychain-refresh-environment)'"))

  (defun keychain-lock ()
    (interactive)
    (async-shell-command "keychain --agents ssh,gpg -k all")))

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

(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'paren)
(show-paren-mode 1)

(require 'elec-pair)
(electric-pair-mode 1)

(use-package magit
  :defer t
  :pin melpa-stable
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-repository-directories '(("~/repos" . 1))))

(use-package projectile
  :defer t
  :bind-keymap (("C-c p" . projectile-command-map))
  :config
  (projectile-mode 1))

(use-package flycheck
  :defer t
  :bind (("C-c f" . flycheck-mode)))

;;; LANGS

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defun my-shell-mode-hook()
  (setq-local compile-command
              '((lambda()
                  (save-buffer)
                  (async-shell-command (buffer-file-name))))))
(add-hook 'sh-mode-hook 'my-shell-mode-hook)

(require 'lisp-mode)

(defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when hippie expanding in a lisp editing mode"
  (if (and (or lispy-mode parinfer-mode smartparens-strict-mode paredit-mode) (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

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

(use-package smartparens :defer t
  :bind (("C-c (" . sp-wrap-round)
         ("C-c )" . sp-unwrap-sexp)))

(use-package parinfer
  :defer t
  :init
  (add-hook 'clojure-mode-hook #'parinfer-mode)
  (add-hook 'cider-repl-mode-hook #'parinfer-mode)
  (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
  (add-hook 'common-lisp-mode-hook #'parinfer-mode)
  (add-hook 'scheme-mode-hook #'parinfer-mode)
  (add-hook 'lisp-mode-hook #'parinfer-mode)
  :config (use-package lispy)
  (setq parinfer-extensions
        '(defaults
           pretty-parens
           lispy
           smart-tab
           smart-yank))
  (global-set-key (kbd "C-c ,") 'parinfer-toggle-mode)
  (define-key parinfer-mode-map (kbd "M-a") 'backward-list)
  (define-key parinfer-mode-map (kbd "M-e") 'forward-list))

(use-package clojure-mode
  :defer t
  :config
  (add-hook 'clojure-mode-hook 'subword-mode))

(use-package cider
  :defer t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode))

(use-package web-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)))

(use-package js2-mode
  :defer t
  :init
  (if (>= emacs-major-version '27)
      (add-hook 'js-mode-hook 'js2-minor-mode)
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'"  . js2-mode))
      (add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
      (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))))

(use-package markdown-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (setq markdown-fontify-code-blocks-natively t))
