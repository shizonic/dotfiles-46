;;; -*- lexical-binding: t; -*-

;; handy functions

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

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

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

;; dir loader

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

;; tramp stuff
(defun my-pwd ()
  (interactive)
  (if (string-match "@" (pwd))
      (string-trim
       (format "%s" (cddr (split-string-by-delim default-directory ":"))) "\(" "\)")
    (string-trim default-directory)))

(defun toor ()
  (if (string-match "@" (pwd))
      (cd (my-pwd))))

(defun root ()
  (if (not (string-match "@" (pwd)))
   (cd (concat "/su:root@"system-name":"default-directory))))

(defun tooroot ()
  (if (string-match "@" (pwd))
      (toor)
    (root))
  (pwd))

(defun my-tramp-root-switcher ()
  "tramp back and forth between root and regular user."
  (interactive)
  (if (string-match "*eshell" (format "%s" (current-buffer)))
      (progn
        (if (string-match "@" (pwd))
            (progn
              (my-path-env-to-root)
              (insert (concat "cd" " " (my-pwd)))
              (eshell-send-input))
          (progn
            (my-path-env-to-root)
            (insert (concat "cd /su:root@"system-name":"default-directory))
            (eshell-send-input))))
    (progn
      (my-path-env-to-root)
      (tooroot))))

(defun my-su-edit ()
  "WE DON'T NEED SUDO, we have su!"
  (interactive)
  (if (buffer-file-name)
      (find-file (concat "/su:root@"system-name":"(buffer-file-name)))
    (find-file (concat "/su:root@"system-name":"(expand-file-name ".")))))

;; a front-end to getkiss.org package manager

(defun kiss ()
  (interactive)
  (root)
  (setq-local
   my-read
   (read-string "kiss [b|c|i|l|r|s|u] [pkg] [pkg] [pkg] " ""))
  (async-shell-command (concat "kiss " my-read))
  (delete-other-windows)
  (switch-to-buffer "*Async Shell Command*"))

;; a home brewed menu

(defun my-interactive-menu ()
  (interactive)
  (setq-local options '((a . abook)
                        (e . (lambda ()(interactive)(call-interactively 'eww)))
                        (g . gnus)
                        (k . kiss)
                        (i . erc)
                        (r . emms-streams)
                        (m . (lambda ()(interactive)(call-interactively 'emms-play-file)))
                        (p . emms-playlist-mode-go)))

  (setq-local
   my-read
   (read-string
    "Menu [e]ww|[g]nus|[a]book|[k]iss|[i]rc|[r]adio|[p]laylist|[m]edia " ""))

  (setq-local option-keys (cl-loop for (key . value) in options
                                   collect key))

  (funcall (cdr (assq (intern my-read) options))))

(provide 'my-libs)
