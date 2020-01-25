(setq shell-file-name "/bin/bash")
(setenv "SHELL" "/bin/bash")

(setenv "HOME" (concat "/home/" user-login-name))
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))

(defvar system-path-inherited
  (concat
   (getenv "PATH") ":"))

(defvar my-path-insert
  (concat
   (getenv "HOME") "/bin:"
   (getenv "HOME") ".local/bin:"))

(defvar my-path-append (concat ":" exec-directory))

(setenv "PATH"
        (string-join
         (setq-default exec-path
                       (delete-dups (split-string
                                     (concat
                                      my-path-insert
                                      system-path-inherited
                                      my-path-append) ":"))) ":"))

(with-eval-after-load 'tramp
  ;; make tramp use remote machine's PATH
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; define remote tramp env
  (setq tramp-remote-process-environment
        ;; TODO :: find a way to set this on a per-tramp connection/machine basis
        '(;; original values
          "ENV=''"
          "TMOUT=0"
          "LC_CTYPE=''"
          "CDPATH="
          "HISTORY="
          "MAIL="
          "MAILCHECK="
          "MAILPATH="
          "autocorrect="
          "correct="

          ;; my values
          "EDITOR=cat"
          "PAGER=cat")))
