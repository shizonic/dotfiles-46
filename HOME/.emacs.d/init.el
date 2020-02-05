;;; -*- lexical-binding: t; -*-

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com")

;; startup

(menu-bar-mode -1)
(setq-default default-frame-alist '((tty-color-mode . never)))
(setq-default mode-line-format '((:eval (format-mode-line "%* %b %l:%c"))))
(custom-set-faces '(font-lock-function-name-face ((t (:weight bold)))))

(setq package-enable-at-startup nil
      custom-file "/dev/null"
      initial-major-mode 'org-mode
      inhibit-startup-screen t
      load-prefer-newer t)

(add-hook 'after-init-hook #'(lambda ()
                               (when (get-buffer "*straight-process*")
                                 (kill-buffer "*straight-process*"))
                               (when (get-buffer "*scratch*")
                                 (kill-buffer "*scratch*"))
                               (shell)
                               (delete-other-windows)))

(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
(load (expand-file-name "lisp/lib.el" user-emacs-directory))
(load (expand-file-name "lisp/defaults.el" user-emacs-directory))
(load (expand-file-name "lisp/bind.el" user-emacs-directory))
(load (expand-file-name "lisp/prog.el" user-emacs-directory))
(load (expand-file-name "lisp/lang.el" user-emacs-directory))
