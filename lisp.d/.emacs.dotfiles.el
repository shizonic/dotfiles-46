;;; -*- lexical-binding: t; -*-

(defun dotfiles-install ()
  (interactive)

  (use-package f)
  (f-write-text "dotfiles" 'utf-8 "~/.emacs.d/.dotfiles")

  (make-directory "~/bin" t)
  (start-process-shell-command "ln" nil "ln -sf ~/repos/dotfiles/bin ~/")

  (setq dotfiles-xresources "Xft.dpi: 96
Xft.autohint: 0
Xft.antialias: 1
Xft.hinting: true
Xft.hintstyle: hintslight
Xft.rgba: rgb
Xft.lcdfilter: lcddefault")

  (f-write-text dotfiles-xresources 'utf-8 "~/.Xresources")

  (setq dotfiles-bash-logout "if [ $\(tty\) = \"/dev/tty1\" ]; then
    keychain --agents ssh,gpg -k all
fi")

  (f-write-text dotfiles-bash-logout 'utf-8 "~/.bash_logout")

  (setq dotfiles-profile "export EDITOR=emacsclient
export VISUAL=$EDITOR
export PATH=~/bin:$PATH

\[ -f \"$HOME/.bashrc\" ] && . \"$HOME/.bashrc\"

. /home/adam/.nix-profile/etc/profile.d/nix.sh
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH # https://github.com/NixOS/nix/issues/2033

echo \"start X?\"
read -r && \[ -z \"$DISPLAY\" ] && \[ \"$(tty)\" = \"/dev/tty1\" ] && xinit ~/.xinitrc -- /usr/bin/X :0 vt1 -keeptty")

  (f-write-text dotfiles-profile 'utf-8 "~/.bash_profile")

  (setq dotfiles-xserverrc "#!/bin/sh

exec /usr/bin/Xorg -nolisten tcp \"$@\" vt$XDG_VTNR")

  (f-write-text dotfiles-xserverrc 'utf-8 "~/.xserverrc")

  (setq dotfiles-bashrc "\[[ $- != *i* ]] && return

PS1='$ '")

  (f-write-text dotfiles-bashrc 'utf-8 "~/.bashrc")

  (setq dotfiles-gitconfig "\[user]
        email = paxchristi888@gmail.com
        name = Adam Schaefers
        signingkey = 77CF5C5C65A8F9F44940A72CDD4795B51117D906
\[commit]
        gpgsign = true")

  (f-write-text dotfiles-gitconfig 'utf-8 "~/.gitconfig")

  (setq dotfiles-mailcap "application/pdf; emacsclient %s
image/png; emacsclient %s
image/jpeg; emacsclient %s
image/gif; emacsclient %s")

  (f-write-text dotfiles-mailcap 'utf-8 "~/.mailcap")

  (setq dotfiles-gnupg-gpg-agent-conf "default-cache-ttl 84000
max-cache-ttl 84000
allow-emacs-pinentry
allow-loopback-pinentry
pinentry-program /home/adam/bin/pinentry-emacs")

  (f-write-text dotfiles-gnupg-gpg-agent-conf 'utf-8 "~/.gnupg/gpg-agent.conf")

  (setq dotfiles-config-mpv "profile=gpu-hq
scale=ewa_lanczossharp
cscale=ewa_lanczossharp
video-sync=display-resample
interpolation
tscale=oversample")

  (make-directory "~/.config/mpv" t)
  (f-write-text dotfiles-config-mpv 'utf-8 "~/.config/mpv/mpv.conf")

  (setq dotfiles-config-sx-sxrc "#!/bin/sh

while ! xprop -root | grep -q Free; do sleep 1; done
internal=LVDS1
external=VGA1
if xrandr | grep -q \"$external connected\" ; then  xrandr --output $internal --off --output $external --auto ; fi
xset +dpms
xset s 1800
xset b off
xset dpms 0 0 1860

xset r rate 200 60

touchpad=\"$(xinput list | awk '/TouchPad/ { print $7 }')\"
xinput set-prop \"${touchpad#id=}\" 'libinput Tapping Enabled' 1
xinput set-prop \"${touchpad#id=}\" 'libinput Accel Speed' 0.4

xsetroot -solid black -cursor_name left_ptr
xrdb -merge ~/.Xresources

export XDG_CURRENT_DESKTOP=\"Exwm\";
export _JAVA_AWT_WM_NONREPARENTING=\"1\";

exec emacs")

  (make-directory "~/.config/sx" t)
  (f-write-text dotfiles-config-sx-sxrc 'utf-8 "~/.config/sx/sxrc"))

(when (not (file-exists-p "~/.emacs.d/.dotfiles"))
  (dotfiles-install))
