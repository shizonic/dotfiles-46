#!/bin/sh
":"; exec /bin/emacs --quick --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;;dotfiles.el -- kind of bonkers, but "it works" ... and this is just how I get it done.

(load "~/repos/dot-emacs/straight.el/bootstrap.el" nil 'nomessage)
(straight-use-package 'f)
(require 'f)

;; CREATE FILES IN $HOME

;;symlink ~/bin
(start-process-shell-command
 "ln" nil
 "ln -sf ~/repos/dot-emacs/bin ~/")

;;symlink wallpaper
(start-process-shell-command
 "ln" nil
 "ln -sf ~/repos/dot-emacs/wallpaper/tree.jpg ~/.wallpaper")

;; put straight pkg versions under vc
(start-process-shell-command
 "ln" nil
 "DIR=~/.emacs.d/straight/versions ; \[ -d \"$DIR\" ] && rm -rf $DIR && ln -sf ~/repos/dot-emacs/versions $DIR")

;; git
(setq dotfiles-gitconfig "\[user]
email = paxchristi888@gmail.com
name = Adam Schaefers
signingkey = 77CF5C5C65A8F9F44940A72CDD4795B51117D906
\[commit]
        gpgsign = true")
(f-write-text dotfiles-gitconfig 'utf-8 "~/.gitconfig")

;; bash
(setq dotfiles-bashrc "\[\[ $- != *i* ]] && return")
(f-write-text dotfiles-bashrc 'utf-8 "~/.bashrc")

(setq dotfiles-bash-profile "\[\[ -f ~/.bashrc ]] && . ~/.bashrc

export PATH=\"$HOME/bin:$HOME/.local/bin:$PATH\"

if \[ -z \"$DISPLAY\" ] && \[ \"$\(tty\)\" = /dev/tty1 ];then
    sx
fi")
(f-write-text dotfiles-bash-profile 'utf-8 "~/.bash_profile")

;; create ~/.gnupg/gpg-agent.conf
(setq dotfiles-gnupg-gpg-agent-conf (concat  "default-cache-ttl 84000
max-cache-ttl 84000
allow-emacs-pinentry
allow-loopback-pinentry
pinentry-program /home/"user-login-name"/bin/pinentry-emacs"))
(f-write-text dotfiles-gnupg-gpg-agent-conf 'utf-8 "~/.gnupg/gpg-agent.conf")


;; xinitrc
(setq dotfiles-xinitrc "touchpad=$\(xinput list | awk '/TouchPad/ { print $7 }'\)
xinput set-prop ${touchpad#id=} \"libinput Tapping Enabled\" 1
xinput set-prop ${touchpad#id=} \"libinput Accel Speed\" 0.4

external=VGA-1
internal=LVDS-1
if xrandr | grep -q \"$external connected\"; then xrandr --output \"$internal\" --off --output \"$external\" --auto; fi

xsetroot -cursor_name left_ptr

feh --no-fehbg --bg-max ~/.wallpaper &
while pgrep X; do xsetroot -name \" $\(date\) \"; sleep 60; done &
picom --backend glx &

while pgrep X; do dwm; done")

(f-write-text dotfiles-xinitrc 'utf-8 "~/.xinitrc")
(set-file-modes "~/.xinitrc" #o755)

;; CREATE FILES IN /etc

(setq dotfiles-xorg-conf-d "Section \"InputClass\"
        Identifier \"system-keyboard\"
        MatchIsKeyboard \"on\"
        Option \"XkbLayout\" \"us\"
        Option \"XkbModel\" \"pc104\"
        Option \"XkbOptions\" \"ctrl:swap_lalt_lctl\"
EndSection
")
(f-write-text dotfiles-xorg-conf-d 'utf-8 "/su::/etc/X11/xorg.conf.d/00-keyboard.conf")
