;;;;ENV/PATH -- for when you want absolute control over these things, here's how to do it

(setq shell-file-name "/bin/mksh")
(setenv "SHELL" "/bin/mksh")

(setenv "HOME" (concat "/home/" user-login-name))
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))

(defvar system-profile-path
  (string-trim (shell-command-to-string "grep -E '^export PATH' /etc/profile") "export PATH="))

(defvar my-path-insert
  (concat
   "/usr/local/bin:"))

(defvar my-path-append (concat ":" exec-directory))

(setenv "PATH"
        (string-join
         (setq-default exec-path
                       (delete-dups (split-string
                                     (concat
                                      my-path-insert
                                      system-profile-path
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
          "EDITOR=ed"
          "PAGER=cat"))

  (add-to-list 'tramp-remote-process-environment
               (concat "CFLAGS="(string-trim
                                 (string-trim (shell-command-to-string "grep CFLAGS /etc/profile")
                                              "export ") "CFLAGS=\\\""  "\\\"")))
  (add-to-list 'tramp-remote-process-environment
               (concat "CFLAGS="(string-trim
                                 (string-trim (shell-command-to-string "grep CFLAGS /etc/profile")
                                              "export ") "CXXFLAGS=\\\""  "\\\"")))
  (add-to-list 'tramp-remote-process-environment
               (string-trim (shell-command-to-string "grep MAKEFLAGS /etc/profile") "export "))
  (add-to-list 'tramp-remote-process-environment
               (string-trim (shell-command-to-string "grep KISS_PATH /etc/profile") "export ")))

(string-trim (shell-command-to-string "grep CFLAGS /etc/profile") "export ")
