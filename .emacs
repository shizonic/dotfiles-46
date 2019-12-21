;;; -*- lexical-binding: t; -*-

;; first things
(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      my-contacts-file "~/contacts.el"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen t
      custom-file "/dev/null"
      my-dotfiles-dir "~/repos/dotfiles"
      my-lisp-files "lisp.d"
      gc-cons-threshold 100000000
      debug-on-error nil)

;; startup to eshell *only*
(add-hook 'after-init-hook '(lambda()
                              (kill-buffer "*scratch*")
                              (eshell)))

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

(setq use-package-always-ensure t
      use-package-always-demand t)
;;; libs
(require 'cl-lib)                     ;Common Lisp extensions
(require 'seq)                        ;Sequence manipulation functions
(require 'subr-x)                     ;Extra Lisp functions
(straight-use-package 'dash)                    ;A modern list library
(straight-use-package 'a)                       ;Associative data structure functions
(straight-use-package 's)                       ;String manipulation library
(straight-use-package 'f)                       ;Modern API for working with files and directories
(straight-use-package 'ht)                      ;The missing h ash table library
;;; misc packages
(straight-use-package 'crux)
(straight-use-package 'browse-kill-ring)
(straight-use-package 'keychain-environment)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'aggressive-indent)
(straight-use-package 'lispy)
(straight-use-package 'elisp-slime-nav)
(straight-use-package 'slime)
;;; misc built-ins
(require 'org)
(require 'erc)
(require 'ielm)
(require 'dired-x)
(require 'tramp)

;; handy functions

(defun split-file-by-delim (FILE delim)
  ;; e.g. (split-file-by-delim "~/.bashrc" "\n")
  ;; note: useful when used also with subr-x's join-string...
  (with-temp-buffer
    (insert-file-contents FILE)
    (split-string (buffer-string) delim t)))

(defun split-string-by-delim (STRING delim)
  ;; e.g. (split-string-by-delim "23:25:35" ":")
  ;; note: useful when used also with subr-x's join-string...
  (with-temp-buffer
    (princ STRING (current-buffer))
    (split-string (buffer-string) delim t)))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

;; dir loader

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

;; keychain

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(defun keychain-unlock ()
    (interactive)
    (async-shell-command
     "eval $(keychain --eval --agents ssh,gpg id_rsa 77CF5C5C65A8F9F44940A72CDD4795B51117D906); emacsclient -e '(keychain-refresh-environment)'"))

(defun keychain-lock ()
  (interactive)
  (async-shell-command "keychain --agents ssh,gpg -k all"))

;; tramp stuff

(defun my-pwd ()
  "Show the real pwd whether we are tramp-root or regular user"
  (interactive)
  (if (string-match "root@" (pwd))
      (string-trim
       (format "%s" (cddr (split-string-by-delim default-directory ":"))) "\(" "\)")
    (string-trim default-directory)))

(defun toor ()
  (interactive)
  "change Emacs internal directory to user (away from tramp root)"
  (if (string-match "root@" (pwd))
      (cd (my-pwd))))

(defun suroot ()
  (interactive)
  "change Emacs internal directory to root using su"
  (if (not (string-match "root@" (pwd)))
      (cd (concat "/su:root@"system-name":"default-directory))))

(defun tooroot ()
  (interactive)
  "toggle Emacs internal directory using su"
  (if (string-match "root@" (pwd))
      (toor)
    (suroot))
  (pwd))

(defun eshell-tramp-su ()
  "tramp su back and forth in the Eshell."
  (interactive)
  (if (string-match "*eshell" (format "%s" (current-buffer)))
      (progn
        (if (string-match "root@" (pwd))
            (progn
              (insert (concat "cd" " " (my-pwd)))
              (eshell-send-input))
          (progn
            (insert (concat "cd /su:root@"system-name":"default-directory))
            (eshell-send-input))))))

(defun my-su-edit ()
  "WE DON'T NEED SUDO, we have su!"
  (interactive)
  ;; eshell
  (when (string= "eshell-mode" major-mode)
    (eshell-tramp-su))

  ;; dired
  (when (string= "dired-mode" major-mode)
    (find-file (concat "/su:root@"system-name":"(expand-file-name "."))))

  ;; file
  (when (buffer-file-name)
    (find-file (concat "/su:root@"system-name":"(buffer-file-name)))))

;; operating system

(defun kiss ()
  "front-end for getkiss.org linux package manager"
  (interactive)
  (with-temp-buffer
    (if (not (string-match "root@" (pwd)))
        (cd (concat "/su:root@"system-name":"default-directory)))
    (setq-local
     my-read
     (read-string "kiss [b|c|i|l|r|s|u] [pkg] [pkg] [pkg] " ""))
    (async-shell-command (concat "kiss " my-read "|| echo err $?"))
    (delete-other-windows)
    (switch-to-buffer "*Async Shell Command*")))

;; load lisp.d/ files
(when (file-directory-p (concat my-dotfiles-dir "/" my-lisp-files))
  (load-directory (concat my-dotfiles-dir "/" my-lisp-files)))

;; start an Emacs server

(server-start)
