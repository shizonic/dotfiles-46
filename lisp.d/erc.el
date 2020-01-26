(defun freenode ()
  (interactive)
  (erc-tls :server "chat.freenode.net" :port "6697"))

(setq erc-hide-list '("JOIN" "PART" "QUIT")
      erc-autojoin-timing "ident"
      erc-prompt-for-password nil
      erc-nick "adamantium"
      erc-autojoin-channels-alist '(("freenode.net"
                                     "#archlinux"
                                     "#kisslinux"
                                     "#commanduser"
                                     "##apoptosis"
                                     "#emacs"
                                     "#lisp")))

(defun my-erc-multi-line-disable (string)
  (when (string-match-p "\n+" string)
    (setq str nil)))

(add-hook 'erc-send-pre-hook 'my-erc-multi-line-disable)

(with-eval-after-load 'erc
  (add-to-list 'erc-modules 'notifications))
