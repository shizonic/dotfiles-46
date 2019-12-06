;;; -*- lexical-binding: t; -*-

;; general information
(setq gc-cons-threshold 100000000
      debug-on-error nil
      custom-file "/dev/null"
      user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen nil)

;; "(bail)" if no ssl enforcement
(if (and (and (executable-find "gnutls-cli")
              (executable-find "python3"))
         (eq (call-process "python3" nil nil nil "-m" "certifi") 0))
    (progn
      (with-eval-after-load 'gnutls
        (setq gnutls-log-level 0)
        (setq gnutls-verify-error t)
        (setq gnutls-min-prime-bits 3072))
      (setq tls-checktrust t)
      (let ((trustfile
             (replace-regexp-in-string
              "\\\\" "/"
              (replace-regexp-in-string
               "\n" ""
               (shell-command-to-string "python3 -m certifi")
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
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package async
  :config (async-bytecomp-package-mode 1))

(use-package auto-package-update :defer t
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-interval 1
        auto-package-update-prompt-before-update nil)
  (auto-package-update-maybe))

;; load my elisp files
(use-package load-dir
  :config
  (setq load-dirs "~/repos/dotfiles/lisp.d")
  (load-dirs))
