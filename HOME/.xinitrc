touchpad=$(xinput list | awk '/TouchPad/ { print $7 }')
xinput set-prop ${touchpad#id=} "libinput Tapping Enabled" 1
xinput set-prop ${touchpad#id=} "libinput Accel Speed" 0.4

xset r rate 200 50

external=VGA-1
internal=LVDS-1
if xrandr | grep -q "$external connected"; then xrandr --output "$internal" --off --output "$external" --auto; fi

xsetroot -cursor_name left_ptr
feh --no-fehbg --bg-max ~/.wallpaper

while pgrep X; do xsetroot -name " $(date) "; sleep 60; done &
picom --backend glx &
dunst &
spm &
xss-lock -- slock &
pgrep emacs || emacs --fg-daemon &

notify-send "$(uname -a)"
notify-send "$(emacs --version)"

exec dwm
