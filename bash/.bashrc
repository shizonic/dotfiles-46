# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

PS1='$ '

restow(){
    cd ~/dotfiles
    mkdir -p ~/{bin,.emacs.d/straight/versions,.config/{mpv,spm,dunst}}
    stow -R bin bash config emacs misc wallpaper ssh gnupg
}

# ESHELL !
emacsclient -t -e "(eshell)" -e "(xterm-mouse-mode)"
