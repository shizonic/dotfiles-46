# ext4 native $HOME encryption bug tracking...
# https://github.com/google/fscrypt/issues/153   <-problem
# https://github.com/systemd/systemd/issues/8598 <-bug
# https://github.com/google/fscrypt/issues/95    <-fix in progress

#HACK (just make sure we encrypt important stuff in ~/LOCKER)
[ -z "$DISPLAY" ] && [ "$(who | grep $USER | wc -l)" = 1 ] && {
    export pid="$BASHPID"
    cd /
    ps -U $USER | grep -Ev "PID|$pid" | awk '{print $1}' | xargs -t kill -15
    ps -U $USER | grep -Ev "PID|$pid" | awk '{print $1}' | xargs -t kill -9
    fscrypt lock $HOME/LOCKER
}
