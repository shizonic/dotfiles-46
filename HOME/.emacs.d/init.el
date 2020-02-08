;;; -*- lexical-binding: t; -*-

;; Tweak GC to reduce startup time... Thanks: https://github.com/ianpan870102/yay-evil-emacs/blob/master/init.el

(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(defvar ian/gc-cons-threshold 20000000)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          (lambda ()
            (setq gc-cons-threshold ian/gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook (lambda ()
                                   (setq gc-cons-threshold (* ian/gc-cons-threshold 2))))
(add-hook 'minibuffer-exit-hook (lambda ()
                                  (garbage-collect)
                                  (setq gc-cons-threshold ian/gc-cons-threshold)))

;; Further reduce startup time by disabling package.el...

(setq package-enable-at-startup nil)

;; startup misc.

(setq custom-file "/dev/null"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen nil
      load-prefer-newer t)

;; Start an Emacs server...

(add-hook 'after-init-hook #'(lambda ()
                               (require 'server)
                               (or (server-running-p)
                                   (server-start))))

;; Kill unwanted startup buffers...

(add-hook 'window-setup-hook #'(lambda ()
                                 (dolist (buffer '("*scratch*" "*straight-process*"))
                                   (when (get-buffer buffer)
                                     (kill-buffer buffer)))))

;; Straight.el for reproduceable package management...

(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))

(setq straight-use-package-by-default t)

;; Use-package for greater control over our packages...

(straight-use-package 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t)

;; Libs for Emacs hackers

(use-package subr-x :straight nil) ;Extra Lisp Functions
(use-package a)                    ;Associative data structure functions
(use-package async)                ;Simple library for asynchronous processing in Emacs
(use-package cl-lib)               ;Common Lisp extensions
(use-package dash)                 ;A modern list library
(use-package f)                    ;Modern API for working with files and directories
(use-package ht)                   ;The missing hash table library
(use-package s)                    ;String manipulation library
(use-package seq)                  ;Sequence manipulation functions

;; Load lisp.d/ modules...

(dolist (file (directory-files (expand-file-name "lisp.d" user-emacs-directory) t "\.el$" nil))
  (load (file-name-sans-extension file)))

;; A helpful binding to return to this file...

(global-set-key (kbd "C-c I") #'(lambda ()
                                  (interactive)
                                  (find-file user-init-file)))
