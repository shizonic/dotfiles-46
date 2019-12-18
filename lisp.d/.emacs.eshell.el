;;; -*- lexical-binding: t; -*-

;; environment variables

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))

(setenv "CC" "x86_64-pc-linux-musl-gcc -static")
(setenv "MAKEFLAGS" "-j5")
(setenv "CFLAGS" "-O3 -pipe")
(setenv "CXXFLAGS" "-O3 -pipe")

;; PATH

(setq my-path-insert (concat
                      "/home/" user-login-name "/bin:"
                      "/home/" user-login-name "/.local/bin:"
                      "/sucks/coreutils/bin:"
                      "/sucks/misc/bin:"
                      "/rocks/bin:"))

(setq my-path-append ":/foo/bar")

;; set (tramp-)root's path and env

(defvar my-sync-root-path t
  "Keep root's (tramp-)PATH in sync with user/Emacs?")

(defun my-path-env-to-root ()
  "Why would you want to do this!!!"
  (if (bound-and-true-p my-sync-root-path)
      (progn
        ;; add local user's path to roots path
        (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
        ;; define roots env
        (setq tramp-remote-process-environment
              '("ENV=''"
                "TMOUT=0"
                "LC_CTYPE=''"
                "EDITOR=ed"
                "PAGER=cat"
                "MAKEFLAGS=j5"
                "CC=x86_64-pc-linux-musl-gcc -static"
                "CFLAGS=-O3 -pipe"
                "CXXFLAGS=-O3 -pipe")))))

;; eshell hooks

;; auto ls
(add-hook 'eshell-directory-change-hook 'eshell/ls)

;; eshell alias / functions

(defun eshell/emacs (file)
  "Intercept the accidental execution of emacs"
  (find-file file))

(defun eshell/- ()
  (insert "cd -")
  (eshell-send-input))

(defun eshell/.. (&optional counter)
  (defun up ()
    (insert "cd ..")
    (eshell-send-input))
  (if (numberp counter)
    (while (> counter 0)
      (up)
      (setq counter (1- counter)))
    (up)))

;; MISC

;; make scripts executeable automatically
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; a shell-mode hook
(defun my-shell-mode-hook()
  (setq-local compile-command
              '((lambda()
                  (save-buffer)
                  (async-shell-command (buffer-file-name))))))
(add-hook 'sh-mode-hook 'my-shell-mode-hook)
