# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

PS1='$ '

[ "$(id -u)" = 0 ] && PS1='# '

alias emacs="emacsclient -t -e \"(about-emacs)\" -e \"(xterm-mouse-mode)\""

external=VGA-1
internal=LVDS-1
switch_to_external_display() {
    xrandr --output "$internal" --off --output "$external" --auto
    feh --no-fehbg --bg-max ~/.wallpaper
}

switch_to_internal_display() {
    xrandr --output "$external" --off --output "$internal" --auto
    feh --no-fehbg --bg-max ~/.wallpaper
}
