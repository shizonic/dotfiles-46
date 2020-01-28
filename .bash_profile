export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# unlock my gpg master "keyring" manually
eval $(keychain --eval --agents gpg 77CF5C5C65A8F9F44940A72CDD4795B51117D906)

# unlock ssh keys and encrypted directories automatically

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

if [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ];then
    x
fi
