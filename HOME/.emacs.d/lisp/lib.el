;;; -*- lexical-binding: t; -*-

;;;;lib

;; "Note that installing a package will activate all of its autoloads, but it
;; will not actually require the features provided by the package. This
;; means that you might need to use require or autoload for some antiquated
;; packages that do not properly declare their autoloads."
;; https://github.com/raxod502/straight.el

(require 'subr-x)             ;Extra Lisp Functions

(straight-use-package 'a)     ;Associative data structure functions
(straight-use-package 'async) ;Simple library for asynchronous processing in Emacs
(straight-use-package 'cl-lib);Common Lisp extensions
(straight-use-package 'dash)  ;A modern list library
(straight-use-package 'f)     ;Modern API for working with files and directories
(straight-use-package 'ht)    ;The missing hash table library
(straight-use-package 's)     ;String manipulation library
(straight-use-package 'seq)   ;Sequence manipulation functions

;;;;pkgs

(straight-use-package 'aggressive-indent)
(straight-use-package 'bind-key)
(straight-use-package 'better-shell)
(require 'better-shell)
(straight-use-package 'browse-kill-ring)
(straight-use-package 'company)
(straight-use-package 'crux)
(require 'crux)
(straight-use-package 'elisp-slime-nav)
(straight-use-package 'flycheck)
(straight-use-package 'magit)
(straight-use-package 'paredit)
(straight-use-package 'projectile)
(straight-use-package 'slime)

;;;;misc

;; use xclip to copy/paste in emacs-nox
(unless window-system
  (when (getenv "DISPLAY")
    (defun xclip-cut-function (text &optional push)
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))
    (defun xclip-paste-function ()
      (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
        (unless (string= (car kill-ring) xclip-output)
          xclip-output)))
    (setq interprogram-cut-function 'xclip-cut-function)
    (setq interprogram-paste-function 'xclip-paste-function)))

(defun shutdown ()
  (interactive)
  (let ((choices '("kexec" "suspend" "hibernate" "reboot" "poweroff")))
    (message "%s" (setq choice (ido-completing-read "Shutdown:" choices )))
    (progn
      (with-temp-buffer
        (cd "/su::")
        (shell-command (concat "systemctl " choice))))))
