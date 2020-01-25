;;; -*- lexical-binding: t; -*-
;; lexical is faster than dynamic...

;; This software is considered complete and no further development is expected to happen.
;; Just kidding, it's Emacs!

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com")

;; Over-commented for new users to explore...
;; However, Emacs is self documented. C-h v on variables and C-h f on functions will show docs.

;; This hook loads last. Good place to put the first function you want to run on startup.
;; Note: (setq set inhibit-startup-screen t) to disable the default startup page.
(add-hook 'after-init-hook (lambda()
                             (require 'server)
                             (when (not (server-running-p))
                               (server-start))))

(setq initial-major-mode 'emacs-lisp-mode ;; Default major mode of the *scratch* buffer
      inhibit-startup-screen nil          ;; I like the default startup. It makes me happy :)
      load-prefer-newer t                 ;; should be a default...
      custom-file "/dev/null"             ;; M-x customize is against my principles...
      package-enable-at-startup nil       ;; faster load
      gc-cons-threshold 50000000)         ;; faster load

;;;;Do not use melpa it is both a wild west and a ghetto.
;;;;We can have reproduceable packages by using straight.el instead.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFER NOTHING, LOAD EVERYTHING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `use-package' may be overrated. If Emacs is slow to start, then Emacs is bloated...
;; Instead, by loading everything up-front, we may reduce the number of config bugs.

;;;;libs

;; We can `require' built-in libs, and `straight-use-package' non built-ins

(require 'subr-x)            ;Extra Lisp Functions
(require 'seq)               ;Sequence manipulation functions
(require 'cl-lib)            ;Common Lisp extensions
(straight-use-package 'dash) ;A modern list library
(straight-use-package 'a)    ;Associative data structure functions
(straight-use-package 's)    ;String manipulation library
(straight-use-package 'f)    ;Modern API for working with files and directories
(require 'f) ;; annoying that this is needed... I  believe `f' may have an autoload issue.
(straight-use-package 'ht)   ;The missing hash table library
(straight-use-package 'async);Simple library for asynchronous processing in Emacs

;;;;pkgs

(straight-use-package 'exwm)
(straight-use-package 'emms)
(straight-use-package 'desktop-environment)
(straight-use-package 'bind-key)
(straight-use-package
 '(emacs-piper :type git :host gitlab :repo "howardabrams/emacs-piper")) ;; TODO: This looks fun
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

;; Emacs best practice is modular configuration.
;; https://www.emacswiki.org/emacs/DotEmacsModular

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
