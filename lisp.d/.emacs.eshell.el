;;; -*- lexical-binding: t; -*-

;; prompt
(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "┌─[" 'face `(:foreground "cyan"))
         (propertize (user-login-name) 'face `(:foreground "red"))
         (propertize "@" 'face `(:foreground "cyan"))
         (propertize (system-name) 'face `(:foreground "red"))
         (propertize "]──[" 'face `(:foreground "cyan"))
         (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "red"))
         (propertize "]──[" 'face `(:foreground "cyan"))
         (propertize (concat (eshell/pwd)) 'face `(:foreground "cyan"))
         (propertize "]\n" 'face `(:foreground "cyan"))
         (propertize "└─>" 'face `(:foreground "cyan"))
         (if (> eshell-last-command-status 0) ;; echo return codes
             (propertize (format "%s " eshell-last-command-status) 'face `(:foreground "red")))
         (propertize (if (or (string-match "root" (pwd)) ;; detect root via tramp
                             (= (user-uid) 0))           ;; or the uid
                         "# "
                       "$ " )
                     'face `(:foreground "cyan")))))

;; environment variables
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))
(setenv "MAKEFLAGS" "-j5")
(setenv "CFLAGS" "-O2 -pipe")
(setenv "CXXFLAGS" "-O2 -pipe")
(setenv "KISS_PATH" "/var/db/kiss/repo/core:/var/db/kiss/repo/extra:/var/db/kiss/repo/xorg:/root/community/community:/home/adam/repos/community/community")

;; PATH

(setq my-path-insert (concat
                      "/home/" user-login-name "/bin:"
                      "/home/" user-login-name "/.local/bin:"
                      "/opt/awk/awk/bin:"
                      "/opt/gnu/coreutils/bin:"
                      "/opt/gnu/findutils/bin:"
                      "/opt/gnu/diffutils/bin:"
                      "/opt/gnu/gawk/bin:"
                      "/opt/gnu/patch/bin:"))

(setq my-path-append ":/rocks/more/bin")

(setq my-path-inherited (getenv "PATH"))

(setenv "PATH"
  (string-join
   (setq my-path
    (delete-dups (split-string-by-delim
                  (setenv "PATH" (concat
                                  my-path-insert
                                  my-path-inherited
                                  my-path-append)) ":")))":"))

(setq my-path (concat "PATH=" (getenv "PATH")))

;; set (tramp-)root's path and env

(defvar my-sync-root-path t
  "Keep root's (tramp-)PATH in sync with Emacs environment")

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
              "CFLAGS=-O2 -pipe"
              "CXXFLAGS=-O2 -pipe"
              "KISS_PATH=/var/db/kiss/repo/core:/var/db/kiss/repo/extra:/var/db/kiss/repo/xorg:/root/community/community:/home/adam/repos/community/community"))))

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

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (eshell "new")
    (insert "ls")
    (eshell-send-input)
    (rename-buffer (concat "*eshell: " name "*"))))

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
