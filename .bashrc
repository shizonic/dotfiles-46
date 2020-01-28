[[ $- != *i* ]] && return

PS1='$ '

[ "$(id -u)" = 0 ] && PS1='# '


alias emacs="emacsclient -t -e \"(about-emacs)\" -e \"(xterm-mouse-mode)\""
