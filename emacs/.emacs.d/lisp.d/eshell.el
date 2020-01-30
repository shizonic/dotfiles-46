;;; -*- lexical-binding: t; -*-

(require 'aweshell)

(defun eshell/restow ()
  (insert "cd ~/dotfiles")
  (eshell-send-input)
  (insert "stow -R bin bash config emacs misc wallpaper ssh gnupg")
  (eshell-send-input))

(defun eshell/su ()
  (cd "/su::"))
