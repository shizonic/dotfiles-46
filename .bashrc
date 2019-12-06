[[ $- != *i* ]] && return

unalias ls
PS1='[\u@\h \W]\$ '

keyopen () {
    eval $(keychain --eval --agents ssh,gpg id_rsa 77CF5C5C65A8F9F44940A72CDD4795B51117D906)
    emacsclient -e "(keychain-refresh-environment)"
}

keykill () {
    keychain --agents ssh,gpg -k all
}
