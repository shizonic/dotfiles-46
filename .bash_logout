# https://github.com/google/fscrypt/issues/153   <-problem
# https://github.com/systemd/systemd/issues/8598 <-bug
# https://github.com/google/fscrypt/issues/95    <-fix in progress

# HACK
lock() {
    cd /
    fscrypt lock $HOME
    while true; do echo \"Bye, Felicia\"; done &
    sleep 1
}

[ -z "$DISPLAY" ] && [ "$(who | grep adam | wc -l)" = 1 ] && {
    clear
    for serv in ~/.config/systemd/user/*.service; do
        systemctl stop --user $(echo $serv | cut -d '/' -f 7 | cut -d '.' -f 1)
    done
    sleep 1
    pkill -u adam -15 &>/dev/null
    ps -U $USER | egrep -v "PID|bash" | awk '{print $1}' | xargs -t kill -9 &>/dev/null
}

trap lock EXIT
