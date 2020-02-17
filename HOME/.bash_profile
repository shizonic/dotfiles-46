[ "$(tty)" = /dev/tty1 ] && [ -z "$DISPLAY" ] && {
    # start gpg-agent
    gpg-connect-agent /bye > /dev/null 2>&1
    GPG_TTY=$(tty)
    export GPG_TTY

    # start ssh-agent
    eval $(ssh-agent) > /dev/null 2>&1
    export SSH_AUTH_SOCK SSH_AGENT_PID

    # unlock them simultaneously with the help of `expect'
    expect << EOF
spawn ssh-add $HOME/.ssh/id_rsa
expect "Enter passphrase"
send "$(gpg -d < "$HOME/.authinfo.id_rsa.gpg")\r"
expect eof
EOF

    read -rp "start X?" && [ -z "$DISPLAY" ] && {
        startx > /dev/null 2>&1
    }

}

[ -f ~/.bashrc ] && . ~/.bashrc
