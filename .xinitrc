external=VGA-1
internal=LVDS-1
touchpad="$(xinput list | awk '/TouchPad/ { print $7 }')"

if xrandr | grep -q "$external connected"; then
    xrandr --output "$internal" --off --output "$external" --auto
else
    xrandr --output "$external" --off --output "$internal" --auto
fi

xset s off -dpms #Disable DPMS and prevent screen from blanking

[ -n "$touchpad" ] && {
    xinput set-prop ${touchpad#id=} "libinput Tapping Enabled" 1
    xinput set-prop ${touchpad#id=} "libinput Accel Speed" 0.4
}

xsetroot -cursor_name left_ptr

st &

exec dwm
