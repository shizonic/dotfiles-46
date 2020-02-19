;;; -*- lexical-binding: t; -*-

;; init settings...

(setq gc-cons-threshold 20000000)

(setq package-enable-at-startup nil)

(setq custom-file "/dev/null"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen nil
      load-prefer-newer t)

(add-hook 'after-init-hook #'(lambda ()
                               ;; kill unwanted initial buffers...
                               (dolist (buffer '("*scratch*" "*straight-process*"))
                                 (when (get-buffer buffer)
                                   (kill-buffer buffer)))

                               ;; start an Emacs server...
                               (require 'server)
                               (or (server-running-p)
                                   (server-start))))

;; straight.el for reproduceable package management...

(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))

(setq straight-use-package-by-default t)

;; use-package for greater control over package loading...

(straight-use-package 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t)

;; libs for Emacs hackers...

(use-package subr-x :straight nil :ensure nil) ;Extra Lisp Functions
(use-package a)      ;Associative data structure functions
(use-package async)  ;Simple library for asynchronous processing in Emacs
(use-package cl-lib) ;Common Lisp extensions
(use-package dash)   ;A modern list library
(use-package f)      ;Modern API for working with files and directories
(use-package ht)     ;The missing hash table library
(use-package s)      ;String manipulation library
(use-package seq)    ;Sequence manipulation functions

;; load lisp.d/ modules...

(dolist (file (directory-files (expand-file-name "lisp.d" user-emacs-directory) t "\.el$" nil))
  (load (file-name-sans-extension file)))

;; a helpful binding to return to this file...

(global-set-key (kbd "C-c I") #'(lambda ()
                                  (interactive)
                                  (find-file user-init-file)))

;;; init.el ends here
