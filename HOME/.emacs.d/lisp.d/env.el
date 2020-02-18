;;; -*- lexical-binding: t; -*-

(setq shell-file-name "/bin/bash")
(setenv "SHELL" "/bin/bash")

(or (getenv "EDITOR")
    (setenv "EDITOR" "emacsclient"))

(or (getenv "VISUAL")
    (setenv "VISUAL" (getenv "EDITOR")))

(or (getenv "PAGER")
    (setenv "PAGER" "cat"))

(setenv "KISS_AUDIT" "1")

(when (file-directory-p "/nix")
  (setenv "NIX_PROFILES" (concat (getenv "HOME") "/nix/var/nix/profiles/default " (getenv "HOME") "/.nix-profile"))
  (setenv "NIX_PATH" (concat (getenv "HOME") "/.nix-defexpr/channels"))
  (setenv "NIX_SSL_CERT_FILE" (concat (getenv "HOME") "/.nix-profile/etc/ssl/certs/ca-bundle.crt")))

(defvar system-path-inherited
  (concat
   (getenv "PATH") ":"))

(defvar my-path-insert
  (concat
   (getenv "HOME") "/bin:"
   (getenv "HOME") "/.local/bin:"
   (getenv "HOME") "/.nix-profile/bin:"
   (getenv "HOME") "/.npm-global/bin:"))

(defvar my-path-append (concat exec-directory))

(setenv "PATH"
        (string-join
         (setq-default exec-path
                       (delete-dups (split-string
                                     (concat
                                      my-path-insert
                                      system-path-inherited
                                      my-path-append) ":"))) ":"))

;; (with-eval-after-load 'tramp
;;   ;; make tramp use remote machine's PATH
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;;   ;; define remote tramp env
;;   (setq tramp-remote-process-environment
;;         ;; TODO :: find a way to set this on a per-tramp connection/machine basis
;;         '(;; original values
;;           "ENV=''"
;;           "TMOUT=0"
;;           "LC_CTYPE=''"
;;           "CDPATH="
;;           "HISTORY="
;;           "MAIL="
;;           "MAILCHECK="
;;           "MAILPATH="
;;           "autocorrect="
;;           "correct="

;;           ;; my values
;;           "EDITOR=cat"
;;           "PAGER=cat")))
