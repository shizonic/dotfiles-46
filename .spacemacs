;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(spacemacs-defaults
     spacemacs-navigation
     spacemacs-editing
     spacemacs-editing-visual
     auto-completion
     emacs-lisp
     syntax-checking
     git
     github
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-diff-side 'right)
     lsp
     dap
     sql
     html
     (csharp :variables csharp-backend 'lsp)
     emacs-lisp
     common-lisp)

   dotspacemacs-additional-packages '(crux
                                      better-shell
                                      browse-kill-ring
                                      amx
                                      ido-completing-read+
                                      flx-ido
                                      projectile)

   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  (setq-default
   dotspacemacs-enable-emacs-pdumper t
   dotspacemacs-emacs-pdumper-executable-file "emacs"
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-gc-cons '(100000000 0.1)
   dotspacemacs-use-spacelpa nil
   dotspacemacs-verify-spacelpa-archives t
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-editing-style 'emacs
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-new-empty-buffer-major-mode nil
   dotspacemacs-scratch-mode 'fundamental-mode
   dotspacemacs-initial-scratch-message nil
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Noto Mono"
                               :size 12.0
                               :weight normal
                               :width normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-generate-layout-names nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-undecorated-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server t
   dotspacemacs-server-socket-dir nil
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("grep")
   dotspacemaedcs-frame-title-format "%I@%S"
   dotspacemacs-icon-title-format nil
   dotspacemacs-whitespace-cleanup 'all
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (setq shell-file-name "/bin/bash")
  (setenv "SHELL" "/bin/bash")

  (or (getenv "EDITOR")
      (setenv "EDITOR" "emacsclient"))

  (or (getenv "VISUAL")
      (setenv "VISUAL" (getenv "EDITOR")))

  (or (getenv "PAGER")
      (setenv "PAGER" "cat"))

  (defvar system-path-inherited
    (concat
     (getenv "PATH") ":"))

  (defvar my-path-insert
    (concat
     (getenv "HOME") "/bin:"
     (getenv "HOME") "/.local/bin:"))

  (defvar my-path-append (concat exec-directory))

  (setenv "PATH"
          (string-join
           (setq-default exec-path
                         (delete-dups (split-string
                                       (concat
                                        my-path-insert
                                        system-path-inherited
                                        my-path-append) ":"))) ":")))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (setq user-full-name "Adam Schaefers"
        user-mail-address "paxchristi888@gmail.com")

  (require 'dired-x)
  (add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))

  (with-eval-after-load 'async
    (autoload 'dired-async-mode "dired-async.el" nil t)
    (dired-async-mode 1))

  (require 'ido)

  (ido-mode 1)
  (ido-vertical-mode -1)
  (ido-everywhere 1)

  (setq ido-create-new-buffer 'always
        ido-auto-merge-work-directories-length -1
        ido-enable-flex-matching t
        ido-use-filename-at-point 'guess)

  (use-package flx-ido
    :init (flx-ido-mode 1)
    :config
    (setq flx-ido-threshold 10000))

  (use-package ido-completing-read+ :init (ido-ubiquitous-mode +1))

  (use-package amx :init (amx-mode 1))

  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq gnus-completing-read-function 'gnus-ido-completing-read)

  (use-package browse-kill-ring
    :init
    (bind-key* "M-y" 'browse-kill-ring))

  (use-package projectile :init (bind-key* "M-m p" 'projectile-command-map)
    :init (add-hook 'after-init-hook 'projectile-mode)
    :config
    (progn
      (projectile-mode)
      (spacemacs|hide-lighter projectile-mode)))

  (use-package magit
    :init
    (setq magit-diff-refine-hunk t)
    (setq magit-repository-directories '(("~/repos" . 1))))

  (use-package crux
    :init
    (bind-key* "C-a" 'crux-move-beginning-of-line)
    (bind-key* "C-o" 'crux-smart-open-line)
    (bind-key* "C-c C-l" 'crux-duplicate-current-line-or-region)
    (bind-key* "C-c C--" 'crux-kill-whole-line)
    (bind-key* "C-c ;" 'crux-duplicate-and-comment-current-line-or-region))

  (use-package better-shell
    :demand
    :init
    (bind-key* "C-t" 'better-shell-for-current-dir))

  (bind-key* "C-x C-o" 'spacemacs/alternate-window)
  (bind-key* "C-x C-b" 'spacemacs/alternate-buffer)
  (bind-key* "C-x <C-backspace>" 'bury-buffer)
  (bind-key* "C-1" 'delete-other-windows)
  (bind-key* "C-2" 'split-window-below)
  (bind-key* "C-3" 'split-window-right)
  (bind-key* "C-0" 'delete-window)

  (defun csharp/pre-init-dap-mode ()
    (add-to-list 'spacemacs--dap-supported-modes 'csharp-mode))

  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

  (add-hook 'window-setup-hook #'(lambda ()
                                   (dolist (buffer '("*scratch*" "*spacemacs*"))
                                     (when (get-buffer buffer)
                                       (kill-buffer buffer)))

                                   (cd "~/")
                                   (find-file ".")))

  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq require-final-newline t
        apropos-do-all t
        vc-follow-symlinks t)

  (shell-command "setxkbmap -option ctrl:swap_lalt_lctl")
  (shell-command "xset r rate 250 50"))

(setq custom-file "~/.emacs.d/.cache/.custom-settings")
(when (file-exists-p custom-file)
  (load custom-file))
