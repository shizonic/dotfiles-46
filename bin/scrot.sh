#!/bin/sh
[ -d "$HOME/Pictures/screenshots" ] || mkdir -p ~/Pictures/screenshots
import -window root "$HOME/Pictures/screenshots/scrot-$(date +%N).png"
