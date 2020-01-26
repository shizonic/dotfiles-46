;;; -*- lexical-binding: t; -*-

;; This software is considered complete and no further development is expected to happen.
;; Just kidding, it's Emacs!

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com")

(add-hook 'after-init-hook (lambda()
                             (require 'server)
                             (when (not (server-running-p))
                               (server-start))))

(setq initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen nil
      load-prefer-newer t
      custom-file "/dev/null"
      package-enable-at-startup nil
      gc-cons-threshold 50000000)

;;;;reproduceable package management with straight.el

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

;; If Emacs is slow to start, then Emacs is bloated...

;;;;libs

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; experimental section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package 'edwina)
(setq display-buffer-base-action '(display-buffer-below-selected))
(edwina-mode 1)
(edwina-setup-dwm-keys 'super)

(defun jump-to-window (buffer-name)
  "From emacswiki https://www.emacswiki.org/emacs/WindowNavigation"
  (interactive "Enter buffer to jump to: ")
  (let ((visible-buffers (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list)))
        window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have visible window" buffer-name)
      (setq window-of-buffer
            (delq nil (mapcar '(lambda (window)
                                 (if (equal buffer-name (buffer-name (window-buffer window)))
                                     window nil)) (window-list))))
      (select-window (car window-of-buffer)))))

(defun edwina-zoom ()
  "Zoom/cycle the selected window to/from master area."
  (interactive)

  (setq edwina-current-buf (buffer-name))

  (if (eq (selected-window) (frame-first-window))
      (edwina-swap-next-window)
    (let ((pane (edwina-pane (selected-window))))
      (edwina-delete-window)
      (edwina-arrange (cons pane (edwina-pane-list)))))

  (jump-to-window edwina-current-buf))

(exwm-input-set-key (kbd "<s-return>") 'edwina-zoom)
(exwm-input-set-key (kbd "<S-s-return>") 'edwina-clone-window)
(exwm-input-set-key (kbd "s-h") 'edwina-dec-mfact)
(exwm-input-set-key (kbd "s-j") 'edwina-select-next-window)
(exwm-input-set-key (kbd "s-k") 'edwina-select-previous-window)
(exwm-input-set-key (kbd "s-l") 'edwina-inc-mfact)
(exwm-input-set-key (kbd "<s-backspace>") 'edwina-delete-window)
(exwm-input-set-key (kbd "s-u") 'winner-undo)
(exwm-input-set-key (kbd "s-U") 'winner-redo)
(bind-key* "C--" 'kill-buffer-and-window)
(exwm-input-set-key (kbd "C--") 'kill-buffer-and-window)
