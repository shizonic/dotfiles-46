# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

PS1='$ '

[ "$(id -u)" = 0 ] && PS1='# '

alias emacs="emacsclient -t -e \"(about-emacs)\" -e \"(xterm-mouse-mode)\""
