#!/bin/sh

external=VGA-1
internal=LVDS-1

wall() {
    feh --no-fehbg --bg-max ~/.wallpaper
}

switch_external() {
    xrandr --output "$internal" --off --output "$external" --auto
    wall
}

switch_internal() {
    xrandr --output "$external" --off --output "$internal" --auto
    wall
}

if xrandr | grep -q "$external connected"; then
    switch_external
else
    switch_internal
fi
