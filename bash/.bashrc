# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

PS1='$ '

[ "$(id -u)" = 0 ] && PS1='# '

alias emacs="emacsclient -t -e \"(eshell)\" -e \"(xterm-mouse-mode)\""
