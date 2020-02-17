case $- in
    *i*) ;;
    *) return;;
esac

PS1='$(pwd) $ '

kiss() { PAGER="$EDITOR" /bin/kiss "$@" ; }
