case $- in
    *i*) ;;
    *) return;;
esac

PS1='$(pwd) $ '

unlock() {
    # start gpg-agent
    gpg-connect-agent /bye
    GPG_TTY="$(tty)"
    export GPG_TTY

    # start ssh-agent
    eval "$(ssh-agent)"
    export SSH_AUTH_SOCK SSH_AGENT_PID

    # unlock them simultaneously with the help of `expect'
    expect << EOF
spawn ssh-add $HOME/.ssh/id_rsa
expect "Enter passphrase"
send "$(gpg -d < "$HOME/.authinfo.id_rsa.gpg")\r"
expect eof
EOF
}

lock() {
    pkill -u "$USER" ssh-agent
    pkill -u "$USER" gpg-agent
}

suspend() {
    sudo echo mem > /sys/power/state
}
