;;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold 20000000
      package-enable-at-startup nil
      custom-file "/dev/null"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen t
      load-prefer-newer t)

(add-hook 'after-init-hook #'(lambda ()
                               (dolist (buffer '("*scratch*" "*straight-process*"))
                                 (when (get-buffer buffer)
                                   (kill-buffer buffer)))

                               (dolist (file (directory-files (expand-file-name "lisp.d" user-emacs-directory) t "\.el$" nil))
                                 (load (file-name-sans-extension file)))

                               (find-file user-init-file)

                               (use-package zone :demand
                                 :config
                                 (zone-when-idle 120)
                                 (zone))))

(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t)
