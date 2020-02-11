;;; -*- lexical-binding: t; -*-

(use-package erc
  :init
  (defun freenode ()
    (interactive)
    (erc-tls :server "chat.freenode.net" :port "6697"))
  :config
  (require 'erc-track)
  (erc-track-disable)
  (setq erc-hide-list '("JOIN" "PART" "QUIT")
        erc-autojoin-timing "ident"
        erc-prompt-for-password nil
        erc-nick "adamantium"
        erc-autojoin-channels-alist '(("freenode.net"
                                       "#kisslinux"
                                       ;; "#emacs"
                                       ;; "#lisp"
                                       )))

  (defun my-erc-multi-line-disable (string)
    (when (string-match-p "\n+" string)
      (setq str nil)))

  ;; fool-proof anti-flood
  (add-hook 'erc-send-pre-hook 'my-erc-multi-line-disable))
