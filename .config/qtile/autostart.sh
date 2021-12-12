#!/bin/bash

xsetroot -cursor_name left_ptr

feh --bg-scale ~/.config/qtile/img/paint-colorful-overlay.jpg

# Turn off black Screensaver
# My current laptop has an issue that sometimes causes system to freeze
# on the black screen, requiring a hard reboot to fix.
xset s off -dpms

# Turn off system beep in console:
xset b off
xset b 0 0 0

gnome-screensaver &
nm-applet &
blueman-applet &
