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
    x
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
dunst &
xss-lock -- slock &
st -e emacsclient -t -e \"\(about-emacs\)\" -e \"\(xterm-mouse-mode\)\" &

exec dwm")

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
(when (not (file-exists-p "/etc/X11/xorg.conf.d/00-keyboard.conf"))
  (f-write-text dotfiles-xorg-conf-d 'utf-8 "/su::/etc/X11/xorg.conf.d/00-keyboard.conf"))

;; dunstrc
(setq dotfiles-dunstrc "\[global]
frame_color = \"#586e75 \"
separator_color = \"#586e75 \"

geometry = \"300x60-20+30\"

idle_threshold = 30
show_age_threshold = 60

font = Noto Sans Mono 8

shrink = no
separator_height = 0
padding = 18
horizontal_padding = 18
frame_width = 1
sort = no
line_height = 2
word_wrap = yes
icon_position = off
ignore_newline = no

\[base16_low]
    msg_urgency = low
    background = \"#eee8d5\"
    foreground = \"#fdf6e3\"

\[base16_normal]
    msg_urgency = normal
    background = \"#93a1a1\"
    foreground = \"#fdf6e3\"

\[base16_critical]
    msg_urgency = critical
    background = \"#dc322f\"
    foreground = \"#fdf6e3\"")
(make-directory "~/.config/dunst" t)
(f-write-text dotfiles-dunstrc 'utf-8 "~/.config/dunst/dunstrc")

(setq dotfiles-systemd-emacs (concat "\[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs\(1\) https://gnu.org/software/emacs/

[Service]
Type=simple
ExecStart=/usr/bin/emacs --fg-daemon
ExecStop=/usr/bin/emacsclient --eval \"\(kill-emacs\)\"
Environment=SSH_AUTH_SOCK=%t/keyring/ssh
Restart=on-failure

[Install]
WantedBy=default.target"))
(make-directory "~/.config/systemd/user" t)
(f-write-text dotfiles-systemd-emacs 'utf-8 "~/.config/systemd/user/emacs.service")

(setq dotfiles-systemd-emacs "profile=gpu-hq
scale=ewa_lanczossharp
cscale=ewa_lanczossharp
video-sync=display-resample
interpolation
tscale=oversample")
(make-directory "~/.config/mpv" t)
(f-write-text dotfiles-systemd-emacs 'utf-8 "~/.config/mpv/mpv.conf")
