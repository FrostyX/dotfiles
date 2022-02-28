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

# Enable tap-to-click
# Use `xinput list` to see available devices
# Use e.g. `xinput list-props "Synaptics TM3289-021"` to see device properties
xinput set-prop "Synaptics TM3289-021" "libinput Tapping Enabled" 1 # alarak
xinput set-prop "ELAN0672:00 04F3:3187 Touchpad" "libinput Tapping Enabled" 1 # zeratul

nm-applet &
blueman-applet &
