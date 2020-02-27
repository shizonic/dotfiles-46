export PATH="$HOME/.local/bin:$HOME/bin:$PATH"
PAGER=cat
EDITOR=emacsclient
VISUAL="$EDITOR"

[ "$(tty)" = /dev/tty1 ] && [ -z "$DISPLAY" ] && {
    # start gpg-agent
    gpg-connect-agent /bye
    GPG_TTY=$(tty)
    export GPG_TTY

    # start ssh-agent
    eval $(ssh-agent)
    export SSH_AUTH_SOCK SSH_AGENT_PID

    # unlock them simultaneously with the help of `expect'
    expect << EOF
spawn ssh-add $HOME/.ssh/id_rsa
expect "Enter passphrase"
send "$(gpg -d < "$HOME/.authinfo.id_rsa.gpg")\r"
expect eof
EOF

    . /home/foo/.nix-profile/etc/profile.d/nix.sh

    read -rp "start X?" && {
        startx
    }

}

[ -f ~/.bashrc ] && . ~/.bashrc
