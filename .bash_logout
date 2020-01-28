clear

# ext4 native $HOME encryption bug tracking...
# https://github.com/google/fscrypt/issues/153   <-problem
# https://github.com/systemd/systemd/issues/8598 <-bug
# https://github.com/google/fscrypt/issues/95    <-fix in progress

#HACK (just encrypt important stuff in ~/LOCKER)
[ -z "$DISPLAY" ] && [ "$(who | grep adam | wc -l)" = 1 ] && {
    gpgconf --kill gpg-agent
    kill "$SSH_AGENT_PID"
    emacsclient "(kill-emacs)"

    cd /
    pkill -u adam -15
    export pid="$BASHPID"
    ps -U $USER | egrep -v "PID|$pid" | awk '{print $1}' | xargs -t kill -15
    ps -U $USER | egrep -v "PID|$pid" | awk '{print $1}' | xargs -t kill -9
    fscrypt lock $HOME/LOCKER || {
        echo lock failed $?
        bash
    }
    for ((i=100;i>=0;i--)); do echo \"Bye, Felicia\"; done
}
