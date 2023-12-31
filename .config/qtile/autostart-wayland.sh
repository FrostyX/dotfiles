#!/bin/bash

# Wayland has no business loading ~/.Xresources, we need to do that manually
# This is maybe needed for Emacs terminals. Otherwise, I dont use urxvt anymore
xrdb -load ~/.Xresources


xsetroot -cursor_name left_ptr


swaybg --image ~/.config/qtile/img/paint-colorful-overlay.jpg &


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
# xinput set-prop "Synaptics TM3289-021" "libinput Tapping Enabled" 1 # alarak
# xinput set-prop "ELAN0672:00 04F3:3187 Touchpad" "libinput Tapping Enabled" 1 # zeratul


# We probably need to drop the --indicator parameter because
# it IMHO does not support left click
nm-applet --indicator &

blueman-applet &
ulauncher --hide-window &
