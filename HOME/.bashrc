# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

[ "$(tty)" = /dev/tty1 ] || {
    screen -qxRR "$USER"

    [ -z "$INSIDE_EMACS" ] && [ "$TERM" = vt100 ] &&
        emacsclient -create-frame --alternate-editor="" -e "(xterm-mouse-mode)"
}
