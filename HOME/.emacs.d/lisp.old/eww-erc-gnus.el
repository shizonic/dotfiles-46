;;;;gnus/erc/eww

;; simple address book

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

(defun freenode ()
  (interactive)
  (erc-tls :server "chat.freenode.net" :port "6697"))

(setq erc-hide-list '("JOIN" "PART" "QUIT")
      erc-autojoin-timing "ident"
      erc-prompt-for-password nil
      erc-nick "adamantium"
      erc-autojoin-channels-alist '(("freenode.net"
                                        ;"#archlinux"
                                     "#kisslinux"
                                     "#commanduser"
                                     "##apoptosis"
                                     "#liguros"
                                        ;"#emacs"
                                        ;"#lisp"
                                     )))

(defun my-erc-multi-line-disable (string)
  (when (string-match-p "\n+" string)
    (setq str nil)))

(add-hook 'erc-send-pre-hook 'my-erc-multi-line-disable)

(with-eval-after-load 'erc
  (add-to-list 'erc-modules 'notifications))

(require 'gnus)

(setq read-mail-command 'gnus)
(setq gnus-use-full-window nil)
(setq gnus-site-init-file "~/repos/dot-emacs/lisp.d/gnus.el")
(setq gnus-save-newsrc-file nil)
(setq gnus-startup-file "~/.emacs.d/.newsrc")
(setq message-directory "~/.emacs.d/mail/")
(setq gnus-directory "~/.emacs.d/news/")
(setq nnfolder-directory "~/.emacs.d/mail/archive")
(setq nndraft-directory "~/.emacs.d/mail/drafts")
(setq gnus-always-read-dribble-file t)

(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl)
                                  (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                                  (nnmail-expiry-wait immediate)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type  'ssl
      smtpmail-debug-info t smtpmail-debug-verb t
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      gnus-message-archive-method '(nnimap "gmail")
      gnus-message-archive-group "nnimap+gmail:[Gmail]/Sent Mail"
      gnus-gcc-mark-as-read t)

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext")
  (setq mm-automatic-display (remove "text/html" mm-automatic-display)))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-visible-headers
      "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")
(setq gnus-sorted-header-list
      '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
        "^Subject:" "^Date:" "^Gnus"))
(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-group-line-format "%M%S%p%P%5y:%B %G\n" ;;"%B%(%g%)"
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│"
 gnus-article-browse-delete-temp t
 gnus-treat-strip-trailing-blank-lines 'last
 gnus-keep-backlog 'nil
 gnus-summary-display-arrow nil
 gnus-mime-display-multipart-related-as-mixed t
 gnus-auto-select-first nil
 smiley-style 'medium
 gnus-keep-backlog '0)

(with-eval-after-load 'gnus
  (setq gnus-desktop-notify-function 'gnus-desktop-notify-exec
        gnus-desktop-notify-exec-program "notify-send")

  (gnus-desktop-notify-mode)

  (gnus-demon-add-scanmail)
  (gnus-demon-add-handler 'gnus-demon-scan-news 5 t)

  (setq gnus-no-groups-message "")

  (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
    (with-timeout
        (3 (message "Gnus timed out."))
      ad-do-it)))

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "^") 'eww-open-yt-dl) ;; inside `eww' press ^ to open the url with youtube-dl
  (define-key eww-mode-map (kbd "W") 'shr-copy-url));; "w" by default copies current page URL, while "W" now will copy url at point.

(defvar yt-dl-player "mpv"
  "Video player used by `eww-open-yt-dl'")

(defun open-yt-dl ()
  "Browse youtube videos using the Emacs `eww' browser and \"youtube-dl.\"
Specify the video player to use by setting the value of `yt-dl-player'"
  (interactive)
  (when (executable-find "youtube-dl")
    (progn
      (if (string-match  "*eww*" (format "%s"(current-buffer)))
          (eww-copy-page-url)
        (with-temp-buffer (yank)))
      (start-process-shell-command "youtube-dl" nil
                                   (concat "youtube-dl -o - " (nth 0 kill-ring) " - | " yt-dl-player " -")))))


;;;;internet-hook

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

(add-hook 'internet-connected-hook 'freenode)
(add-hook 'internet-connected-hook 'gnus)
