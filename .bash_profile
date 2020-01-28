export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

[ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ] && {

    gpg_fail() {
        exit 1
    }

    trap gpg_fail INT QUIT TERM HUP

    # unlock gpg master "keyring" manually
    gpg-connect-agent /bye
    GPG_TTY=$(tty)
    export GPG_TTY

    echo
    echo "ENTER GPG PASSPHRASE:"
    read -rs ; /usr/lib/gnupg/gpg-preset-passphrase --preset 367EC054909BD58458B1D55A0542410134A10B68 <<< "$REPLY"

    [ "$?" -ne 0 ] && gpg_fail

    # unlock everything else now using its' own preferred agent & passwords from authinfo.gpg

    #ssh-agent
    eval $(ssh-agent)
    export SSH_AUTH_SOCK SSH_AGENT_PID

    #~/.ssh/id_rsa
    id_rsa_pass="$(gpg -d < "$HOME/.authinfo.gpg" | awk '/id_rsa/ { print $6 }' )"

    expect << EOF
spawn ssh-add $HOME/.ssh/id_rsa
expect "Enter passphrase"
send "$id_rsa_pass\r"
expect eof
EOF

    unset id_rsa-pass

    #~/LOCKER
    locker_pass="$(gpg -d < "$HOME/.authinfo.gpg" | awk '/fscrypt/ { print $6 }' )"
    echo "$locker_pass" | fscrypt unlock "$HOME/LOCKER"

    unset locker_pass

    [[ -f ~/.bashrc ]] && . ~/.bashrc

    x
}
