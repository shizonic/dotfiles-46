if [ $(tty) = "/dev/tty2" ]; then
    keychain --agents gpg -k all
    keychain --agents ssh -k all
fi
