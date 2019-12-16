;;; -*- lexical-binding: t; -*-

(setq gnus-use-full-window nil)
(setq gnus-site-init-file "~/repos/dotfiles/lisp.d/.emacs.gnus.el")
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
      smtpmail-smtp-service 587
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
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
 gnus-group-line-format "%M%S%p%P%5y:%B %G\n";;"%B%(%g%)"
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

(setq gnus-no-groups-message "")

(with-eval-after-load 'gnus (gnus-demon-add-handler 'gnus-demon-scan-news 1 t))

(defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
  (with-timeout
      (3 (message "Gnus timed out."))
    ad-do-it))

(with-eval-after-load 'gnus
  (setq gnus-desktop-notify-function 'gnus-desktop-notify-exec
        gnus-desktop-notify-exec-program "notify-send")
  (gnus-desktop-notify-mode)
  (gnus-demon-add-scanmail))
