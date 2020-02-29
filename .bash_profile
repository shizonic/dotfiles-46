. /home/foo/.nix-profile/etc/profile.d/nix.sh
export PATH="$HOME/.local/bin:$HOME/bin:$PATH"
export PAGER=cat
export EDITOR=emacsclient
export VISUAL="$EDITOR"

[ "$(tty)" = /dev/tty1 ] && [ -z "$DISPLAY" ] && {
    # read -rp "start X?" && {
    startx > ~/.x11.out 2>&1
    # }
}

[ -f ~/.bashrc ] && . ~/.bashrc
