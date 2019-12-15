;;; -*- lexical-binding: t; -*-

;; environment variables

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))

;; PATH

(setq my-add-to-path (concat
                      "/rocks/bin:"
                      "/sucks/bin:"
                      "/home/" user-login-name "/bin:"
                      "/home/" user-login-name "/.local/bin:"))

(with-eval-after-load 'funclib
  (setenv "PATH"
          (string-join
           (setq my-path
                 (delete-dups (split-string-by-delim
                               (setenv "PATH" (concat
                                               my-add-to-path
                                               (getenv "PATH"))) ":")))":")))

;; aliases

(defalias 'troot '(lambda()(interactive)(cd "/su:root@kiss:/root"))) ;; tramp root

;; functions

(defun eshell/emacs (file)
  (find-file file))

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
