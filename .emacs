;;; -*- lexical-binding: t; -*-

;; first things
(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      my-contacts-file "~/contacts.el"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen t
      custom-file "/dev/null"
      my-dotfiles-dir "~/repos/dotfiles"
      my-lisp-files "lisp.d"
      my-lisp-libs "lisp"
      gc-cons-threshold 100000000
      debug-on-error nil)

;; startup
(add-hook 'window-setup-hook '(lambda()
                               (when (get-buffer "*scratch*")
                                (kill-buffer "*scratch*"))
                               (eshell)
                               (about-emacs)
                               (emacs-init-time)))

;; defer nothing
(with-eval-after-load 'use-package (setq use-package-always-ensure t use-package-always-demand t)
  (require 'cl-lib)   ;; Common Lisp extensions
  (require 'seq)      ;; Sequence manipulation functions
  (require 'subr-x)   ;; Extra Lisp functions
  (use-package async  ;; Asynchronous processing library
    :config (async-bytecomp-package-mode 1))
  (use-package dash)  ;; A modern list library
  (use-package a)     ;; Associative data structure functions
  (use-package s)     ;; String manipulation library
  (use-package f)     ;; Modern API for working with files and directories
  (use-package ht)    ;; The missing hash table library
  (use-package auto-package-update)
  (use-package crux)
  (use-package use-package)
  (use-package hydra)
  (use-package browse-kill-ring)
  (use-package ercn)
  (use-package exwm)
  (use-package xelb)
  (use-package desktop-environment)
  (use-package gnus-desktop-notify)
  (use-package keychain-environment)
  (use-package magit)
  (use-package projectile)
  (use-package flycheck)
  (use-package indent-guide)
  (use-package parinfer)
  (use-package lispy)
  (use-package elisp-slime-nav)
  (use-package slime)
  (use-package emms))

;; ensure tls
(if (and (and (executable-find "gnutls-cli")
              (executable-find "python3"))
         (eq (call-process "python3" nil nil nil "-m" "certifi") 0))
    (progn
      (with-eval-after-load 'gnutls
        (setq gnutls-log-level 0
              gnutls-verify-error t
              gnutls-min-prime-bits 3072))
      (setq tls-checktrust t)
      (let ((trustfile
             (replace-regexp-in-string
              "\\\\" "/"
              (replace-regexp-in-string
               "\n" ""
               (shell-command-to-string "python3 -m certifi")))))
        (setq tls-program
              (list
               (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                       (if (eq window-system 'w32) ".exe" "") trustfile)))))
  (progn
    (setq xbuff (generate-new-buffer "*INSECURE DEFAULTS WARNING*"))
    (with-output-to-temp-buffer
        xbuff
      (print "Ensure python3, certifi and gnutls-cli are installed to enforce TLS..."))
    (bail)))

;; bootstrap use-package
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu-elpa"     . "https://elpa.gnu.org/packages/"))
      package-narchive-priorities
      '(("melpa-stable" . 1)    ;; fallback to melpa-stable
        ("gnu-elpa"     . 10))) ;; gnu-elpa has priority
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; update packages
(setq auto-package-update-delete-old-versions t
      auto-package-update-hide-results t
      auto-package-update-interval 1
      auto-package-update-prompt-before-update nil)
(auto-package-update-maybe)

;; require lisp/ libs
(add-to-list 'load-path (concat my-dotfiles-dir "/" my-lisp-libs))
(require 'my-libs)
(require 'transpose-frame)
(require 'spacemacs)

;; load lisp.d/ files
(when (file-directory-p (concat my-dotfiles-dir "/" my-lisp-files))
  (load-directory (concat my-dotfiles-dir "/" my-lisp-files)))

;; start an Emacs server
(server-start)
