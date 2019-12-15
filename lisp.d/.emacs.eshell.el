;;; -*- lexical-binding: t; -*-

;; environment variables

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")

(setq my-add-to-path (concat
                      "/rocks/bin:"
                      "/sucks/bin:"
                      "/home/" user-login-name "/.local/bin:"))
(setenv "PATH"
        (string-join
         (setq my-path
               (delete-dups (split-string-by-delim
                             (setenv "PATH" (concat
                                             my-add-to-path
                                             (getenv "PATH"))) ":")))
         ":"))

;; aliases

(defalias 'troot '(lambda()(interactive)(cd "/su:root@kiss:/root"))) ;; tramp root

;; functions

(defun eshell/emacs (file)
  (find-file file))
