#!/bin/bash

xsetroot -cursor_name left_ptr

feh --bg-scale /home/jkadlcik/.config/qtile/img/kde-wallpaper.png

# Turn off black Screensaver
# My current laptop has an issue that sometimes causes system to freeze
# on the black screen, requiring a hard reboot to fix.
xset s off -dpms

gnome-screensaver &
nm-applet &
