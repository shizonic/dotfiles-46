(defun freenode ()
  (interactive)
  (erc-tls :server "chat.freenode.net" :port "6697"))

(setq erc-hide-list '("JOIN" "PART" "QUIT")
      erc-autojoin-timing "ident"
      erc-prompt-for-password nil
      erc-nick "adamantium"
      erc-autojoin-channels-alist '(("freenode.net"
                                     "#archlinux"
                                     "#emacs"
                                     "#lisp")))

(with-eval-after-load 'erc
  (erc-track-mode -1))

(defun my-erc-multi-line-disable (string)
  "DISABLE SENDING OF MULTI-LINE MESSAGES ENTIRELY!

In my experience erc has terrible flood prevention. I
wrote this quick and dirty bit to avoid accidental flooding
after having flooded the #nixos channel with massive amounts
of text for over 5 minutes. It was so bad, that my bouncer had
become unresponsive, and kept sending text to the channel, line
by line. Completely prevented now with this little bit of elisp."
  (when (string-match-p "\n+" string)
    (setq str nil)))

(add-hook 'erc-send-pre-hook 'my-erc-multi-line-disable)
