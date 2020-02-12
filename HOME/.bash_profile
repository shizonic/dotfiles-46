PATH="$HOME/bin:$HOME/.local/bin:$PATH"

PAGER=cat
EDITOR=emacsclient
VISUAL="$EDITOR"

export PATH EDITOR VISUAL PAGER

[ "$(tty)" = /dev/tty1 ] && [ -z "$DISPLAY" ] && {

    gpg-connect-agent /bye
    GPG_TTY=$(tty)
    export GPG_TTY

    eval $(ssh-agent)
    export SSH_AUTH_SOCK SSH_AGENT_PID

    expect << EOF
spawn ssh-add $HOME/.ssh/id_rsa
expect "Enter passphrase"
send "$(gpg -d < "$HOME/.authinfo.id_rsa.gpg")\r"
expect eof
EOF
}

[ -f ~/.bashrc ] && . ~/.bashrc
