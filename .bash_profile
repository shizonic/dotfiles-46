. /home/foo/.nix-profile/etc/profile.d/nix.sh

PATH="$HOME/.local/bin:$HOME/bin:$PATH"
PAGER=cat
EDITOR=emacsclient
VISUAL="$EDITOR"

export PATH PAGER EDITOR VISUAL

[ "$(tty)" = /dev/tty1 ] && [ -z "$DISPLAY" ] && {
    read -rp "start X?" && {
        startx > ~/.x11.out 2>&1
    }
}

[ -f ~/.bashrc ] && . ~/.bashrc
