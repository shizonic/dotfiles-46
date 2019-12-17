;;; -*- lexical-binding: t; -*-

;; handy functions I have adopted unto myself and/or conjured up into being . . .

(defun split-file-by-delim (FILE delim)
  ;; e.g. (split-file-by-delim "~/.bashrc" "\n")
  ;; note: useful when used also with subr-x's join-string...
  (with-temp-buffer
    (insert-file-contents FILE)
    (split-string (buffer-string) delim t)))

(defun split-string-by-delim (STRING delim)
  ;; e.g. (split-string-by-delim "23:25:35" ":")
  ;; note: useful when used also with subr-x's join-string...
  (with-temp-buffer
    (princ STRING (current-buffer))
    (split-string (buffer-string) delim t)))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name   (car (last (split-string parent "/" t)))))
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))

(defun spacemacs/alternate-buffer (&optional window)
  "switch buffer back-and-forth ... from spacEmacs!"
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defvar yt-dl-player "mpv"
  "Video player used by `eww-open-yt-dl'")

(defun eww-open-yt-dl ()
  "Browse youtube videos using the Emacs `eww' browser and \"youtube-dl.\"
Specify the video player to use by setting the value of `yt-dl-player'"
  (interactive)
  (if (executable-find "youtube-dl")
      (progn
        (eww-copy-page-url)
        (start-process-shell-command "youtube-dl" nil
                                     (concat "youtube-dl -o - " (nth 0 kill-ring) " - | " yt-dl-player " -")))
    (progn
      (setq xbuff (generate-new-buffer "*youtube-dl not found*"))
      (with-output-to-temp-buffer xbuff
        (print "Ensure youtube-dl is installed on the system and try again...")))))

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "^") 'eww-open-yt-dl))

;; abook

(when (file-exists-p my-contacts-file)
  (progn
    (load-file my-contacts-file)

    ;; e.g. dummy address book (key . value) list
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
        (princ email (current-buffer))))))

;; global font resizer

(defun my-set-font (my-font my-font-size)
  (set-face-attribute 'default nil :font (concat my-font "-" (number-to-string my-font-size))))

(defun my-font-resizer (x)
  (if (> x 0)
      (setq-local n '1)
    (setq-local n -1))
  (when (< my-font-size 11)
      (setq my-font-size 11))
  (when (> my-font-size 24)
      (setq my-font-size 24))
  (setq my-font-size (+ n my-font-size))
  (my-set-font my-font my-font-size)
  (message (concat "Font Size: " (number-to-string my-font-size))))

;; redshift setter

(defun my-set-redshift (redshift)
  (start-process-shell-command "redshift" nil (concat "redshift -x && redshift -O " (number-to-string redshift))))

(defun my-redshift-setter (x)
  (if (> x 0)
      (setq-local n '250)
    (setq-local n '-250))
  (setq redshift (+ n redshift))
  (if (< redshift 1000)
      (setq redshift '1000))
  (if (> redshift 25000)
      (setq redshift '25000))
  (my-set-redshift redshift))

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

;; keychain

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(defun keychain-unlock ()
    (interactive)
    (async-shell-command
     "eval $(keychain --eval --agents ssh,gpg id_rsa 77CF5C5C65A8F9F44940A72CDD4795B51117D906); emacsclient -e '(keychain-refresh-environment)'"))

(defun keychain-lock ()
  (interactive)
  (async-shell-command "keychain --agents ssh,gpg -k all"))

;; tramp

(defun my-pwd ()
  (string-trim (format "%s" (cddr (split-string-by-delim default-directory ":"))) "\(" "\)"))

(defun toor ()
  (interactive)
  (cd (my-pwd)))

(defun root ()
  (interactive)
  (cd (concat "/su:root@"system-name":"default-directory)))

;; a front-end to getkiss.org package manager

(defun kiss-pop ()
  (switch-to-buffer "*Async Shell Command*")
  (delete-other-windows))

(defun kiss-list ()
  (interactive)
  (root)
  (async-shell-command "kiss list")
  (kiss-pop))

(defun kiss-build ()
  (interactive)
  (root)
  (setq-local my-read (read-string "Build package(s):" ""))
  (async-shell-command (concat "kiss b" " " my-read))
  (kiss-pop))

(defun kiss-install ()
  (interactive)
  (root)
  (setq-local my-read (read-string "Install package(s):" ""))
  (async-shell-command (concat "kiss i" " " my-read))
  (kiss-pop))

(defun kiss-remove ()
  (interactive)
  (root)
  (setq-local my-read (read-string "Remove package(s):" ""))
  (async-shell-command (concat "kiss remove" " " my-read))
  (kiss-pop))

(defun kiss-search ()
  (interactive)
  (root)
  (setq-local my-read (read-string "Search package(s):" ""))
  (async-shell-command (concat "kiss search" " " my-read))
  (kiss-pop))

(defun kiss-checksum ()
  (interactive)
  (root)
  (setq-local my-read (read-string "Checksum package(s):" ""))
  (async-shell-command (concat "kiss checksum" " " my-read))
  (kiss-pop))

(defun kiss-update ()
  (interactive)
  (root)
  (async-shell-command "kiss update")
  (kiss-pop))

(defhydra kiss-hydra (:exit t)
  "Kiss Package Manager"
  ("u" (call-interactively 'kiss-update) "update")
  ("b" (call-interactively 'kiss-build) "build")
  ("i" (call-interactively 'kiss-install) "install")
  ("r" (call-interactively 'kiss-remove) "remove")
  ("s" (call-interactively 'kiss-search) "search")
  ("c" (call-interactively 'kiss-checksum) "checksum")
  ("l" (call-interactively 'kiss-list) "list")
  ("<menu>" nil))

(provide 'my-libs)
