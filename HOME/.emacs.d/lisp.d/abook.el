;;; -*- lexical-binding: t; -*-

(defvar my-abook "~/.authinfo.contacts.el.gpg")

;; e.g., a dummy contacts.el file...

;; (setq my-contact-list '((name . foo@bar.email)
;;                         (nick . nick@nick.com)
;;                         (john . john@doe.com)))

(defun abook ()
  "Insert an email address from `my-contact-list' to the current buffer."
  (interactive)

  (load-file my-abook)

  (setq my-contact-keys (cl-loop for (key . value) in my-contact-list
                                 collect key))

  (let ((item my-contact-keys))
    (fset 'my-read 'completing-read)

    ;; interactive menu + convert chosen item (key) from string to data
    (setq-local interactive-chosen-key (intern (my-read "Contact Name:" item)))
    ;; match key to list and get associated email (value), convert back to string
    (setq-local email (format "%s" (cdr (assq interactive-chosen-key my-contact-list))))

    ;; output email address to buffer
    (princ email (current-buffer))))
