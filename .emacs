;;; -*- lexical-binding: t; -*-

;; first things
(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen t
      custom-file "/dev/null"
      my-dotfiles-dir "~/repos/dotfiles"
      my-lisp-files "lisp.d"
      my-lisp-libs "lisp"
      gc-cons-threshold 100000000
      debug-on-error nil)

;; startup to eshell
(add-hook 'after-init-hook '(lambda()
                              (interactive)
                              (when (get-buffer "*scratch*")
                                (kill-buffer "*scratch*"))
                              (eshell)))

;; require handy built-in libs
(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'eww)

;; require lisp/ libs
(add-to-list 'load-path (concat my-dotfiles-dir "/" my-lisp-libs))
(require 'my-libs)
(require 'transpose-frame)
(require 'spacemacs)

;; defer nothing, load everything !!!
(with-eval-after-load 'use-package
  (use-package f)
  (setq use-package-always-ensure t use-package-always-demand t)
  (use-package async)
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
  (use-package nofrils-acme-theme))

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

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu-elpa"     . "https://elpa.gnu.org/packages/"))
      package-narchive-priorities
      '(("melpa-stable" . 1) ;; fallback to melpa-stable
        ("gnu-elpa"     . 10))) ;; gnu-elpa has priority

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(async-bytecomp-package-mode 1)

(setq auto-package-update-delete-old-versions t
      auto-package-update-hide-results t
      auto-package-update-interval 1
      auto-package-update-prompt-before-update nil)

(auto-package-update-maybe)

;; load lisp.d/
(when (file-directory-p (concat my-dotfiles-dir "/" my-lisp-files))
  (load-directory (concat my-dotfiles-dir "/" my-lisp-files)))

(server-start)
