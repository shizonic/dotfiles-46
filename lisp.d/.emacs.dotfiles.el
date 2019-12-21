;;; -*- lexical-binding: t; -*-

(defun dotfiles-install ()
  (interactive)

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

  (setq root-dot-profile (concat my-path "
export CFLAGS=\"-O3 -pipe\"
export CXXFLAGS=\"-O3 -pipe\"
export MAKEFLAGS=\"-j$(nproc)\""))
  (f-write-text root-dot-profile 'utf-8
                (concat "/su:root@"system-name":/root/.profile"))

  (setq dotfiles-profile "PATH=~/bin:$PATH

. \"$HOME/.nix-profile/etc/profile.d/nix.sh\"
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH # https://github.com/NixOS/nix/issues/2033

echo \"start X?\"
read -r && \[ -z \"$DISPLAY\" ] && sx")

  (f-write-text dotfiles-profile 'utf-8 "~/.profile")

  (setq dotfiles-gitconfig "\[user]
email = paxchristi888@gmail.com
name = Adam Schaefers
signingkey = 77CF5C5C65A8F9F44940A72CDD4795B51117D906
\[commit]
        gpgsign = true")

  (f-write-text dotfiles-gitconfig 'utf-8 "~/.gitconfig")

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

  (setq dotfiles-xinitrc "#!/bin/sh

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

exec dwm")

  (f-write-text dotfiles-xinitrc 'utf-8 "~/.xinitrc"))

(when (not (file-exists-p "~/.emacs.d/.dotfiles"))
  (dotfiles-install))
