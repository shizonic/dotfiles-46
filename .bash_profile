export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# Setup Information #

# getty1 is autologin, ~/LOCKER is ext4 encrypted with fscrypt
# Other files (e.g. id_rsa) are protected/managed by respective passphrases/agents.
# GPG functions as a master keyring

# decrypt ~/.authinfo.gpg once manually and then unlock everything else automatically
[ "$(tty)" = /dev/tty1 ] && [ -z "$DISPLAY" ] && {

    gpg_fail() {
        exit 1
    }

    trap gpg_fail INT QUIT TERM HUP

    # prepare to unlock gpg master "keyring" manually, only once.
    GPG_TTY=$(tty)
    export GPG_TTY

    gpg -d < "$HOME/.authinfo.gpg" || {
        gpg_fail
    }

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

    unset id_rsa_pass

    #~/LOCKER
    locker_pass="$(gpg -d < "$HOME/.authinfo.gpg" | awk '/fscrypt/ { print $6 }' )"
    echo "$locker_pass" | fscrypt unlock "$HOME/LOCKER"

    unset locker_pass

    [[ -f ~/.bashrc ]] && . ~/.bashrc

    x
}

exit 0
