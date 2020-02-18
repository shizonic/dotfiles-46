case $- in
    *i*) ;;
    *) return;;
esac

PS1='$(pwd) $ '

# prefer a different pager for kiss
kiss() { PAGER="$EDITOR" /bin/kiss "$@" ; }
