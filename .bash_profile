export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# Setup Information #

# getty1 is autologin, ~/LOCKER is ext4 encrypted with fscrypt
# Other files (e.g. id_rsa) are protected/managed by respective passphrases/agents.
# GPG functions as a master keyring

# decrypt ~/.authinfo.gpg once manually and then unlock everything else automatically
[ "$(tty)" = /dev/tty1 ] && [ -z "$DISPLAY" ] && {

    gpg_fail() {
        exit
    }

    trap gpg_fail INT QUIT

    # prepare to unlock gpg master "keyring" manually, only once.
    gpg-connect-agent /bye
    GPG_TTY=$(tty)
    export GPG_TTY

    gpg -d < "$HOME/.authinfo.gpg" || {
        gpg_fail
    }

    #ssh-agent
    eval $(ssh-agent)
    export SSH_AUTH_SOCK SSH_AGENT_PID

    #~/.ssh/id_rsa
    expect << EOF
spawn ssh-add $HOME/.ssh/id_rsa
expect "Enter passphrase"
send "$(gpg -d < "$HOME/.authinfo.id_rsa.gpg")\r"
expect eof
EOF

    #~/LOCKER
    gpg -d < "$HOME/.authinfo.fscrypt.gpg" | fscrypt unlock "$HOME/LOCKER"

    [[ -f ~/.bashrc ]] && . ~/.bashrc

    x

    exit
}
