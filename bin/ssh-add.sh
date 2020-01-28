#!/bin/bash -e

[ $# -ne 1 ] && {
    cat<<EOF
Usage: echo PASSPHRASE | ssh-add.sh ~/.ssh/id_rsa
EOF
    exit 1
}

command -v expect || {
    echo install expect
    exit 1
}

pass="$(cat)"

eval "$(ssh-agent)"
expect << EOF
spawn ssh-add $1
expect "Enter passphrase"
send "$pass\r"
expect eof
EOF
