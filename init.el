;;; -*- lexical-binding: t; -*-

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com")

(setq package-enable-at-startup nil
      load-prefer-newer t
      custom-file "/dev/null"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen nil)

(add-hook 'after-init-hook #'(lambda ()
                               (require 'server)
                               (when (not (server-running-p))
                                 (server-start))))

;;;;reproduceable package management with straight.el

(load "~/repos/dot-emacs/straight.el/bootstrap.el" nil 'nomessage)

;; "Note that installing a package will activate all of its autoloads, but it
;; will not actually require the features provided by the package. This
;; means that you might need to use require or autoload for some antiquated
;; packages that do not properly declare their autoloads."
;; https://github.com/raxod502/straight.el

;;;;libs

(require 'subr-x)             ;Extra Lisp Functions
(straight-use-package 'seq)   ;Sequence manipulation functions
(straight-use-package 'cl-lib);Common Lisp extensions
(straight-use-package 'dash)  ;A modern list library
(straight-use-package 'a)     ;Associative data structure functions
(straight-use-package 's)     ;String manipulation library
(straight-use-package 'f)     ;Modern API for working with files and directories
(straight-use-package 'ht)    ;The missing hash table library
(straight-use-package 'async) ;Simple library for asynchronous processing in Emacs

;;;;pkgs

(straight-use-package 'gnus-desktop-notify)
(straight-use-package 'bind-key)
(straight-use-package 'keychain-environment)
(straight-use-package 'company)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'aggressive-indent)
(straight-use-package 'paredit)
(straight-use-package 'crux)
(straight-use-package 'browse-kill-ring)
(straight-use-package 'elisp-slime-nav)
(straight-use-package 'slime)

;;;;config

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

(load-directory "~/repos/dot-emacs/lisp.d")
