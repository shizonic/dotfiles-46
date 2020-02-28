;;; -*- lexical-binding: t; -*-

;; init settings...

(toggle-frame-maximized)

(setq gc-cons-threshold 20000000)

(setq package-enable-at-startup nil)

(setq custom-file "/dev/null"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen t
      load-prefer-newer t)

(add-hook 'after-init-hook #'(lambda ()
                               ;; kill unwanted initial buffers...
                               (dolist (buffer '("*scratch*" "*straight-process*"))
                                 (when (get-buffer buffer)
                                   (kill-buffer buffer)))

                               ;; start an Emacs server...
                               (require 'server)
                               (or (server-running-p)
                                   (server-start))

                               (find-file ".")))

;; straight.el for reproduceable package management...

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

;; use-package for greater control over package loading...

(straight-use-package 'use-package)

(setq use-package-always-ensure t
      use-package-always-defer t)

;; libs for Emacs hackers...

(require 'subr-x)    ;Extra Lisp Functions
(require 'cl-lib)    ;Common Lisp extensions
(use-package a)      ;Associative data structure functions
(use-package async)  ;Simple library for asynchronous processing in Emacs
(use-package dash)   ;A modern list library
(use-package f)      ;Modern API for working with files and directories
(use-package ht)     ;The missing hash table library
(use-package s)      ;String manipulation library
(use-package seq)    ;Sequence manipulation functions

;; load lisp.d/ modules...

(dolist (file (directory-files (expand-file-name "lisp.d" user-emacs-directory) t "\.el$" nil))
  (load file))

;; a helpful binding to return to this file...

(global-set-key (kbd "C-c I") #'(lambda ()
                                  (interactive)
                                  (find-file user-init-file)))

;;; init.el ends here
