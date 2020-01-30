# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

[ "$TERM" != "dumb" ] && emacsclient -t -e "(shell)" -e "(delete-other-windows)" -e "(xterm-mouse-mode)"
