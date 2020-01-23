(defvar my-abook "~/contacts.el") ;; TODO .gpg

(with-eval-after-load 'gnus
  (when (file-exists-p my-abook)
    (progn
      (load-file my-abook)

      ;; e.g. dummy address book
      ;; (setq my-contact-list '((name . foo@bar.email)
      ;;                         (nick . nick@nick.com)
      ;;                         (john . john@doe.com)))

      (setq my-contact-keys (cl-loop for (key . value) in my-contact-list
                                     collect key))

      (defun abook ()
        "Insert an email address from `my-contact-list' to the current buffer."
        (interactive)
        (let ((item my-contact-keys))
          (fset 'my-read 'completing-read)

          ;; interactive menu + convert chosen item (key) from string to data
          (setq-local interactive-chosen-key (intern (my-read "Contact Name:" item)))
          ;; match key to list and get associated email (value), convert back to string
          (setq-local email (format "%s" (cdr (assq interactive-chosen-key my-contact-list))))

          ;; output email address to buffer
          (princ email (current-buffer)))))))
