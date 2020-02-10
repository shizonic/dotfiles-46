# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

[ -z "$INSIDE_EMACS" ] && [ "$(tty)" != /dev/tty1 ] &&
    emacs -nw
