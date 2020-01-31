;;; -*- lexical-binding: t; -*-

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com")

(setq package-enable-at-startup nil
      load-prefer-newer t
      custom-file "/dev/null"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen nil)

;;;;reproduceable package management with straight.el

(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))

;; "Note that installing a package will activate all of its autoloads, but it
;; will not actually require the features provided by the package. This
;; means that you might need to use require or autoload for some antiquated
;; packages that do not properly declare their autoloads."
;; https://github.com/raxod502/straight.el

;;;;libs

(require 'subr-x)             ;Extra Lisp Functions

(straight-use-package 'a)     ;Associative data structure functions
(straight-use-package 'async) ;Simple library for asynchronous processing in Emacs
(straight-use-package 'cl-lib);Common Lisp extensions
(straight-use-package 'dash)  ;A modern list library
(straight-use-package 'f)     ;Modern API for working with files and directories
(straight-use-package 'ht)    ;The missing hash table library
(straight-use-package 's)     ;String manipulation library
(straight-use-package 'seq)   ;Sequence manipulation functions

;;;;pkgs
(straight-use-package 'aggressive-indent)
(straight-use-package 'bind-key)
(straight-use-package 'better-shell)
(require 'better-shell)
(straight-use-package 'browse-kill-ring)
(straight-use-package 'company)
(straight-use-package 'crux)
(require 'crux)
(straight-use-package 'elisp-slime-nav)
(straight-use-package 'flycheck)
(straight-use-package 'magit)
(straight-use-package 'paredit)
(straight-use-package 'projectile)
(straight-use-package 'slime)

;;;;theme

;; disable emacs-nox menu bar
(menu-bar-mode -1)

;; minimalist modeline
(setq-default mode-line-format
              '((:eval (format-mode-line "%* %b %l:%c"))))

;; disable color completely
(setq-default default-frame-alist '((tty-color-mode . never)))

;; highlight functions using [only] bold.
(custom-set-faces '(font-lock-function-name-face ((t (:weight bold)))))

;; true spartans should also uncomment the next line:
;; (global-font-lock-mode -1)

;;;;much-better-defaults

(setq gnutls-verify-error t
      gnutls-min-prime-bits 2048

      epg-gpg-program "gpg2"
      password-cache-expiry nil

      mouse-yank-at-point t
      save-interprogram-paste-before-kill t

      apropos-do-all t
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      tab-always-indent 'complete

      tramp-default-method "ssh"
      tramp-copy-size-limit nil

      vc-follow-symlinks t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 8
              fill-column 80)

(ido-mode 1)
(ido-everywhere 1)
(setq ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess)

(show-paren-mode 1)

(require 'dired-x)
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(with-eval-after-load 'async
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(defun dired-xdg-open-file ()
  "from https://www.emacswiki.org/emacs/OperatingOnFilesInDired"
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c !") 'dired-xdg-open-file))

(if (> emacs-major-version 26)
    (setq browse-url-browser-function 'browse-url-chromium)
  (setq browse-url-browser-function 'browse-url-chromium))

(delete-selection-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(winner-mode 1)

(xterm-mouse-mode 1)

;;;;functions

;; use xclip to copy/paste in emacs-nox
(unless window-system
  (when (getenv "DISPLAY")
    (defun xclip-cut-function (text &optional push)
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))
    (defun xclip-paste-function ()
      (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
        (unless (string= (car kill-ring) xclip-output)
          xclip-output)))
    (setq interprogram-cut-function 'xclip-cut-function)
    (setq interprogram-paste-function 'xclip-paste-function)))

(defun shutdown ()
  (interactive)
  (let ((choices '("kexec" "suspend" "hibernate" "reboot" "poweroff")))
    (message "%s" (setq choice (ido-completing-read "Shutdown:" choices )))
    (progn
      (with-temp-buffer
        (cd "/su::")
        (shell-command (concat "systemctl " choice))))))

;;;;programming

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

(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above t)

;; manual trigger company
(setq company-idle-delay 0.0)
(bind-key "M-/" 'company-complete)

;; allow me to decline a suggestion and keep typing through as normal...
(with-eval-after-load 'company
  (define-key company-active-map (kbd "SPC") nil))
(setq company-auto-complete-chars nil)

;;;;languages

;; C

(setq c-default-style "linux")
(add-hook 'c-mode-hook
          (lambda () (setq indent-tabs-mode t)))

;; paredit (for all lisps)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)

(defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when hippie expanding in a lisp editing mode"
  (if (and paredit-mode
           (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

;; elisp

(defun my-ielm ()
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'my-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

(add-hook 'ielm-mode-hook 'eldoc-mode)

;; common lisp

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(setq slime-default-lisp 'sbcl)
(setq slime-contribs '(slime-fancy slime-cl-indent))
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-fuzzy-completion-in-place t
      slime-enable-evaluate-in-emacs t
      slime-autodoc-use-multiline-p t)

(with-eval-after-load 'slime
  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))

;;;;bindings

;; remove annoyances
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; minor modes may override
(bind-key "C-a" 'crux-move-beginning-of-line)

;; minor modes may not override
(bind-key* "C-x C-b" 'ido-switch-buffer)
(bind-key* "M-y" 'browse-kill-ring)
(bind-key* "C-c p" 'projectile-command-map)
(bind-key* "C-c g" 'magit-status)
(bind-key* "C-c f" 'flycheck-mode)
(bind-key* "C-c t r" 'region-to-termbin)
(bind-key* "C-c t b" 'buffer-to-termbin)
(bind-key* "C-o" 'crux-smart-open-line)
(bind-key* "C-c C-l" 'crux-duplicate-current-line-or-region)
(bind-key* "C-c -" 'crux-kill-whole-line)
(bind-key* "C-x -" 'bury-buffer)
(bind-key* "C-c ;" 'crux-duplicate-and-comment-current-line-or-region) ; because -nox
(bind-key* "C-x ;" 'comment-line)                                      ; because -nox
(bind-key* "<f5>" 'compile)

(bind-key* "C-t" 'better-shell-for-current-dir)
(bind-key* "C-c T" 'better-shell-remote-open)
(bind-key* "C-c P" 'better-shell-for-projectile-root)
(bind-key* "C-c s" 'better-shell-sudo-here)

(bind-key* (kbd "<mouse-4>") '(lambda () (interactive) (scroll-down 5)))
(bind-key* (kbd "<mouse-5>") '(lambda () (interactive) (scroll-up 5)))
