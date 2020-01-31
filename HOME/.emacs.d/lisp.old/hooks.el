;;; -*- lexical-binding: t; -*-

(defun internet-connected-default ()
  "Sends a message that Internet Connectivity has been detected
via `internet-detect'"
  (message "Internet Connection Established."))

(defvar internet-connected-hook 'internet-connected-default
  "Hook that is run by the `internet-detect' function.")

(defun internet-detect ()
  "Test for internet connectivity in an asynchronous loop and run
hooks of `internet-connected-hook' only after internet connectivity
 has been established."
  (async-start
   (lambda ()
     (while (not (eq 0 (call-process "nc" nil nil nil "-zw1" "google.com" "80")))
       (sleep-for 5)))
   (lambda (result)

     (run-hooks 'internet-connected-hook))))

(add-hook 'after-init-hook 'internet-detect)

;; e.g. now one might (add-hook 'internet-connected-hook 'my-custom-function)

(add-hook 'internet-connected-hook 'gnus)
(add-hook 'internet-connected-hook 'freenode)
