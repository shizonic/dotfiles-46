;;; -*- lexical-binding: t; -*-

;; Tweak GC to reduce startup time... Thanks: https://github.com/jwiegley/dot-emacs/blob/master/init.el

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; startup related settings...

(setq custom-file "/dev/null"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen nil
      load-prefer-newer t)

;; Kill unwanted startup buffers...

(add-hook 'window-setup-hook #'(lambda ()
                                 (dolist (buffer '("*scratch*" "*straight-process*"))
                                   (when (get-buffer buffer)
                                     (kill-buffer buffer)))))

;; Straight.el for reproduceable package management...

(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))

(setq straight-use-package-by-default t)

;; Use-package for greater control over our package loading...

(straight-use-package 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t)

;; Libs for Emacs hackers

(use-package subr-x :straight nil :ensure nil) ;Extra Lisp Functions
(use-package a)      ;Associative data structure functions
(use-package async)  ;Simple library for asynchronous processing in Emacs
(use-package cl-lib) ;Common Lisp extensions
(use-package dash)   ;A modern list library
(use-package f)      ;Modern API for working with files and directories
(use-package ht)     ;The missing hash table library
(use-package s)      ;String manipulation library
(use-package seq)    ;Sequence manipulation functions

;; Load lisp.d/ modules...

(dolist (file (directory-files (expand-file-name "lisp.d" user-emacs-directory) t "\.el$" nil))
  (load (file-name-sans-extension file)))

;; Check Email / Join IRC, but only if connected to the internet and only in my first Emacs session...

;; (when (eq 1 (string-to-number (string-trim (shell-command-to-string (concat "pgrep -u " user-login-name " -c emacs")))))
;;   (progn
;;     (add-hook 'internet-connected-hook 'gnus)
;;     (add-hook 'internet-connected-hook 'freenode)))

;; A helpful binding to return to this file...

(global-set-key (kbd "C-c I") #'(lambda ()
                                  (interactive)
                                  (find-file user-init-file)))

;; Start an Emacs server...

(add-hook 'after-init-hook #'(lambda ()
                               (require 'server)
                               (or (server-running-p)
                                   (server-start))))

;;; init.el ends here
