xset s off -dpms #Disable DPMS and prevent screen from blanking

touchpad=$(xinput list | awk '/TouchPad/ { print $7 }')
xinput set-prop ${touchpad#id=} "libinput Tapping Enabled" 1
xinput set-prop ${touchpad#id=} "libinput Accel Speed" 0.4

external=VGA-1
internal=LVDS-1
if xrandr | grep -q "$external connected"; then xrandr --output "$internal" --off --output "$external" --auto; fi

xsetroot -cursor_name left_ptr

exec emacs
