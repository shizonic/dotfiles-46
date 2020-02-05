# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

[ -z "$INSIDE_EMACS" ] && [ "$(tty)" != /dev/tty1 ] && {
    if [ "$TERM" = vt100 ];then
        pgrep emacs &>/dev/null || emacs
    else
        screen -qxRR "$USER" -T vt100
    fi
}
