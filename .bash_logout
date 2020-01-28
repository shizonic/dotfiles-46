clear

# bug tracking...
# https://github.com/google/fscrypt/issues/153   <-problem
# https://github.com/systemd/systemd/issues/8598 <-bug
# https://github.com/google/fscrypt/issues/95    <-fix in progress

[ -z "$DISPLAY" ] && [ "$(who | grep adam | wc -l)" = 1 ] && {
    #emacsclient "(kill-emacs)"
    keychain --agents ssh,gpg -k all

    cd /
    pkill -u adam -15
    ps -U $USER | egrep -v "PID|bash" | awk '{print $1}' | xargs -t kill -9
    fscrypt lock $HOME/LOCKER || {
        echo lock failed $?
        bash
    }
    for ((i=100;i>=0;i--)); do echo \"Bye, Felicia\"; done
}
