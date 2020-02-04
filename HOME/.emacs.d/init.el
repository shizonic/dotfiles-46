;;; -*- lexical-binding: t; -*-

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com")

(setq package-enable-at-startup nil
      load-prefer-newer t
      custom-file "/dev/null"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen nil)

(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
(load (expand-file-name "lisp/lib.el" user-emacs-directory))
(load (expand-file-name "lisp/theme.el" user-emacs-directory))
(load (expand-file-name "lisp/bind.el" user-emacs-directory))
(load (expand-file-name "lisp/defaults.el" user-emacs-directory))
(load (expand-file-name "lisp/prog.el" user-emacs-directory))
(load (expand-file-name "lisp/lang.el" user-emacs-directory))
