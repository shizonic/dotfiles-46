(defun dotfiles-install ()
  (interactive)

  ;; put straight pkg versions under vc
  (start-process-shell-command
   "ln" nil
   "DIR=~/.emacs.d/straight/versions ; \[ -d \"$DIR\" ] && rm -rf $DIR && ln -sf ~/repos/dot-emacs/versions $DIR")

  ;; setup eshell aliases
  (start-process-shell-command "ln" nil
                               "ln -sf ~/repos/dot-emacs/eshell/alias ~/.emacs.d/eshell/")

  ;; git
  (setq dotfiles-gitconfig "\[user]
email = paxchristi888@gmail.com
name = Adam Schaefers
signingkey = 77CF5C5C65A8F9F44940A72CDD4795B51117D906
\[commit]
        gpgsign = true")
  (f-write-text dotfiles-gitconfig 'utf-8 "~/.gitconfig")

  ;; gpg
  (setq dotfiles-gnupg-gpg-agent-conf "default-cache-ttl 84000
max-cache-ttl 84000
allow-emacs-pinentry
allow-loopback-pinentry
pinentry-program /home/adam/repos/dot-emacs/extra/pinentry-emacs")
  (f-write-text dotfiles-gnupg-gpg-agent-conf 'utf-8 "~/.gnupg/gpg-agent.conf")

  ;; xinitrc
  (setq dotfiles-xinitrc "touchpad=$\(xinput list | awk '/TouchPad/ { print $7 }'\)
xinput set-prop ${touchpad#id=} \"libinput Tapping Enabled\" 1
xinput set-prop ${touchpad#id=} \"libinput Accel Speed\" 0.4

external=VGA-1
internal=LVDS-1
if xrandr | grep -q \"$external connected\"; then xrandr --output \"$internal\" --off --output \"$external\" --auto; fi

xsetroot -cursor_name left_ptr
xset r rate 250 60

exec emacs")

  (f-write-text dotfiles-xinitrc 'utf-8 "~/.config/sx/sxrc")
  (set-file-modes "~/.config/sx/sxrc" #o755))
