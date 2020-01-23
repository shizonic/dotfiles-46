#!/bin/dash
":"; exec /bin/emacs --quick --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'subr-x)

(if (not (or (string= "poweroff" (format "%s" (elt argv 0)))
             (string= "reboot" (format "%s" (elt argv 0)))))
    (progn
      (message "Usage: shutdown.el reboot|poweroff")
      (kill-emacs 1))
  (if (string= "poweroff" (format "%s" (elt argv 0)))
      (setq-local goodbye "-p")
    (setq-local goodbye "-r")))

(setq debug-on-error nil)

(setenv "PATH" "/bin")
(setq exec-path '("/bin"))

(setenv "SHELL" "/bin/dash")
(setq shell-file-name "/bin/dash")

(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(when (and (executable-find "runsvdir")
           (eq 0 (call-process "pgrep" nil nil nil "runsvdir")))
  (progn
    (message "Waiting for services to stop...")
    (shell-command "sv -w196 force-stop /var/service/*")
    (shell-command "sv exit /var/service/*")))

(progn
  (message "Saving random seed...")
  (message "%s"
           (process-exit-code-and-output "dd" "count=1" "bs=512" "if=/dev/random" "of=/var/random.seed")))

(progn
  (message "Sending TERM signal to all processes...")
  (message "%s"
           (process-exit-code-and-output
            "ubase-box" "killall5" "-o" (format "%s" (emacs-pid)) "-s" "TERM"))

  (sleep-for 1)

  (message "Sending KILL signal to all processes...")
  (message "%s"
           (process-exit-code-and-output
            "ubase-box" "killall5" "-o" (format "%s" (emacs-pid)) "-s" "KILL")))

(progn
  (message "Unmounting filesystems and disabling swap...")
  (message "%s"
           (process-exit-code-and-output "ubase-box" "swapoff" "-a"))
  (message "%s"
           (process-exit-code-and-output "ubase-box" "umount" "-a"))
  (message "%s"
           (process-exit-code-and-output "ubase-box" "mount" "-o" "remount,ro" "/"))
  (message "%s"
           (process-exit-code-and-output "sbase-box" "sync")))

(progn
  (message "Goodbye.")
  (message "%s"
           (process-exit-code-and-output "ubase-box" "halt" goodbye)))
