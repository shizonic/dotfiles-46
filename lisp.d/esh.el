(add-hook 'eshell-directory-change-hook 'eshell/ls)

(defun eshell/emacs (&optional file)
  "Intercept the accidental execution of emacs"
  (if file
      (find-file file)
    (find-file ".")))

(defun eshell/.. (&optional counter)
  "extend `..' to optionally use an integer for an argument"
  (if (numberp counter)
      (while (> counter 0)
        (insert "cd ..")
        (eshell-send-input)
        (setq counter (1- counter)))
    (progn
      (insert "cd ..")
      (eshell-send-input))))

(defun supershell ()
  "Quickly toggle back and forth as root using tramp and su.
NOTE: Place in ~/.authinfo or ~/.authinfo.gpg something like
what follows to get a sudo-like experience:

machine HOSTNAME login root password PASSWORD"
  (if (not (string-match "/su:root@" (pwd)))
      (progn
        (insert "cd /su::$PWD")
        (eshell-send-input))
    (progn
      (insert "cd $OLDPWD")
      (eshell-send-input))))
