[[ $- != *i* ]] && return

unalias ls
PS1='[\u@\h \W]\$ '

keyopen () {
    eval $(keychain --eval --agents gpg D8B69F47DF12D190A5EAF311436087C84C06B40B)
    # eval $(keychain --eval --agents ssh id_rsa)
    emacsclient -e "(keychain-refresh-environment)"
}

keykill () {
    keychain --agents gpg -k all
    keychain --agents ssh -k all
}

unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi
