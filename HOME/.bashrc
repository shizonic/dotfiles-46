# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

[ -z "$INSIDE_EMACS" ] && emacs
