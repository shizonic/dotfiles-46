# ext4 native $HOME encryption bug tracking...
# https://github.com/google/fscrypt/issues/153   <-problem
# https://github.com/systemd/systemd/issues/8598 <-bug
# https://github.com/google/fscrypt/issues/95    <-fix in progress

#HACK (just make sure we encrypt important stuff in ~/LOCKER)
[ -z "$DISPLAY" ] && [ "$(who | grep -c "$USER")" = 1 ] && {
    export pid="$BASHPID"
    cd /
    pgrep -u "$USER" | grep -Ev "$pid" | xargs -t kill -15
    pgrep -u "$USER" | grep -Ev "$pid" | xargs -t kill -9
    fscrypt lock "$HOME/LOCKER"
}
