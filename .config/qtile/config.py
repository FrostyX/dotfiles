#-*- coding: utf-8 -*-

"""
FrostyX's Qtile config

Don't be dumb and test it with Xephyr first
https://wiki.archlinux.org/index.php/Xephyr

    Xephyr -br -ac -noreset -screen 1600x600 :1 &
    DISPLAY=:1 qtile &
    DISPLAY=:1 urxvt &

"""

import re
import subprocess
from datetime import date
from os import uname
from os.path import expanduser
from libqtile.config import Key, Screen, Group, Drag, Click, Match, Rule
from libqtile.command.client import Client
from libqtile.lazy import lazy
from libqtile import layout, bar, widget, hook

from contrib import (VimwikiUnfinished,
                     Newsboat,
                     DaysCounter,
                     Mu,
                     CurrentLayoutTextIcon)


terminal     = "gnome-terminal"
run_backup   = "gmrun"
run          = "rofi -show run"
applications = "rofi -show drun"
#bright_up    = "xbacklight -inc 10"
#bright_down  = "xbacklight -dec 10"
bright_up    = "light -A 5"
bright_down  = "light -U 5"
lock         = "gnome-screensaver-command -l"
scrot        = ""
scrot_all    = ""
battery      = "BAT0"
suspend      = "systemctl suspend"

player_prev = "playerctl previous --player=spotify"
player_next = "playerctl next --player=spotify"
player_play_pause = "playerctl play-pause --player=spotify"


if True:
    vol_cur  = None
    vol_up   = "pactl set-sink-volume @DEFAULT_SINK@ +2%"
    vol_down = "pactl set-sink-volume @DEFAULT_SINK@ -2%"
    mute     = "pactl set-sink-mute @DEFAULT_SINK@ toggle"
else:
    vol_cur  = "amixer -D pulse get Master"
    vol_up   = "amixer -q -D pulse sset Master 2%+"
    vol_down = "amixer -q -D pulse sset Master 2%-"
    mute     = "amixer -q -D pulse set Master toggle"


hostname = uname()[1]
if hostname == "chromie":
    battery = "BAT1"
    scrot = "/home/jkadlcik/.bin/screenshot.sh"
    scrot_all = "/home/jkadlcik/git/qtile-screenshot/qtile-screenshot.py -o /home/jkadlcik/images/scrot"
    # https://github.com/FrostyX/qtile-screenshot/blob/master/qtile-screenshot.py

# New work laptop is not named yet
elif hostname in ["localhost.localdomain", "sagemcom.ip"]:
    scrot = "/home/jkadlcik/.bin/screenshot.sh"
    terminal = "urxvt256c -e tmux"
    lock = "i3lock -i /home/jkadlcik/.dotfiles/.config/qtile/img/bsod.png"

elif hostname in ["zeratul"]:
    terminal = "urxvt256c -e tmux"
    lock = "i3lock -i /home/jkadlcik/.dotfiles/.config/qtile/img/bsod.png"


mod = "mod1" # Left alt
sup = "mod4" # Left win-key

keys = [
    # Switch window focus to other pane(s) of stack
    Key([mod], "Tab", lazy.layout.next()),

    Key([mod], "Return", lazy.spawn(terminal)),
    Key([mod], "F1", lazy.spawn(terminal)),
    Key([mod], "F2", lazy.spawn(run_backup)),
    Key([mod], "p", lazy.spawn(run)),
    Key([mod, sup], "p", lazy.spawn(applications)),

    # Toggle between different layouts as defined below
    Key([mod], "space", lazy.next_layout()),
    Key([mod], "F4", lazy.window.kill()),

    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),

    Key([mod], "w", lazy.screen.toggle_group()),

    # cycle to previous and next group
    Key([mod], "h", lazy.screen.prev_group(skip_managed=True)),
    Key([mod], "l", lazy.screen.next_group(skip_managed=True)),

    Key([sup], "f", lazy.window.toggle_fullscreen()),
    Key([sup], "t", lazy.window.toggle_floating()),

    # Process `gnome-screensaver` must run
    Key([mod, sup], "l", lazy.spawn(lock)),

    # Multihead magic
    Key([sup], "h", lazy.prev_screen()),
    Key([sup], "l", lazy.next_screen()),

    # Function keys
    Key([], "XF86AudioRaiseVolume", lazy.spawn(vol_up)),
    Key([], "XF86AudioLowerVolume", lazy.spawn(vol_down)),
    Key([], "XF86AudioMute", lazy.spawn(mute)),
    Key([], "XF86MonBrightnessUp", lazy.spawn(bright_up)),
    Key([], "XF86MonBrightnessDown", lazy.spawn(bright_down)),
    Key([], "Print", lazy.spawn(scrot)),
    Key([sup], "Print", lazy.spawn(scrot_all)),

    # Multimedia
    Key([sup], "Left", lazy.spawn(player_prev)),
    Key([sup], "Right", lazy.spawn(player_next)),
    Key([sup], "Down", lazy.spawn(player_play_pause)),

    # Quiting
    Key([mod], "F10", lazy.spawn(suspend)),
]


# dnf install fontawesome-fonts
# https://fortawesome.github.io/Font-Awesome/cheatsheet/
# For v4.7 see https://fontawesome.com/v4.7.0/cheatsheet/
icons = {
    "logo": "",     # fa-redhat
    "temp": "",     # fa-fire-extinguisher
    "battery": "",  # fa-battery-three-quarters
    "light": "",    # fa-lightbulb-o
    "volume": "",   # fa-bullhorn
    "rss": "",      # fa-rss
    "sync": "",     # fa-sync-alt
    "tasks": "",    # fa-calendar-check-o
    "repeat": "",   # fa-repeat
    "email": "",    # fa-at
    "gmail": "",      # fa-google

    "chat": "",      # fa-comment-dots
    "web": "",      # fa-internet-explorer
    "terminal": "", # fa-keyboard
    "dev": "",      # fa-heart
    "doc": "",      # fa-folder
    "misc": "",     # fa-file
    "ssh": "",      # fa-hashtag
    "virtual": "", # fa-cogs
    "games": "",     # fa-playstation
    "music": "",    # fa-headphones

    "max": "",       # fa-window-maximize
    "monadtall": "", # fa-columns
    "treetab": "",   # fa-tree

    "systray": "",  # fa-fedora
}


def get_layout_icon(name):
    return {
        "max": icons["max"],
        "monadtall": icons["monadtall"],
        "treetab": icons["treetab"],
    }.get(name, name)


workspaces = [
    {"name": "i", "key": "i", "label": icons["chat"], "matches": [Match(wm_class=["Pidgin"])]},
    {"name": "r", "key": "r", "label": icons["web"], "matches": [Match(wm_class=["Chromium-browser", "Firefox", "Google-chrome"])]},
    {"name": "f", "key": "f", "label": icons["terminal"], "matches": [Match(wm_class=["dolphin", "Thunar", "File-roller"])]},
    {"name": "d", "key": "d", "label": icons["dev"], "matches": [Match(wm_class=["Lispworks", "jetbrains-pycharm", "Eclipse" ])]},
    {"name": "q", "key": "q", "label": icons["doc"], "matches": [Match(wm_class=["Acroread", "Zathura", "Evince"])]},
    {"name": "n", "key": "n", "label": icons["misc"], "matches": [Match(wm_class=["Claws-mail"])]},
    {"name": "c", "key": "c", "label": icons["ssh"]},
    {"name": "v", "key": "v", "label": icons["virtual"], "matches": [Match(wm_class=["VirtualBox"])]},
    {"name": "g", "key": "g", "label": icons["games"], "matches": [Match(wm_class=["Wine", "Python2.7", "Steam", "Progress"])]}, # Python2.7 is playonlinux; Progress is steam updater
    {"name": "o", "key": "o", "label": icons["music"], "matches": [Match(wm_class=["Vlc", "Totem"])]},
]

groups = []
for workspace in workspaces:
    matches = workspace["matches"] if "matches" in workspace else None
    groups.append(Group(workspace["name"], label=workspace["label"], matches=matches, layout="max"))
    keys.append(Key([mod], workspace["key"], lazy.group[workspace["name"]].toscreen(toggle=False)))
    keys.append(Key([mod, sup], workspace["key"], lazy.window.togroup(workspace["name"])))


# float dialog windows
@hook.subscribe.client_new
def dialogs(window):
    floating = ["gmrun", "gcr-prompter"]
    try:
        wm_type = window.window.get_wm_type()
        wm_class = window.window.get_wm_class()[0]
        transient_for = window.window.get_wm_transient_for()
        if wm_type == 'dialog' or transient_for or wm_class in floating:
            window.floating = True
    except:
        pass


# Preivew: https://chriskempson.github.io/base16/#eighties
# Codes: https://chriskempson.github.io/base16/css/base16-eighties.css
colors = {
    "greybg": "#2d2d2d",
    "greyfg": "#d3d0c8",
    "red": "#f2777a",
    "blue": "#6699cc",
    "lgrey": "#747369",
    "green": "#99cc99",
}

base16_chalk = {
    "black" : "#151515",
    "red": "#fb9fb1",
    "green": "#acc267",
    "yellow": "#ddb26f",
    "blue": "#6fc2ef",
    "magenta": "#e1a3ee",
    "cyan": "#12cfc0",
    "white": "#d0d0d0",
    "gray": "#505050",
}

# http://docs.qtile.org/en/latest/manual/ref/layouts.html
layout_theme = {
    "border_width": 1,
    "border_focus": colors["blue"],
    "border_normal": colors["lgrey"],
    "margin": 10,
    "single_margin": 10,
}
layouts = [
    layout.MonadTall(**layout_theme),
    layout.TreeTab(**layout_theme),
    layout.xmonad.MonadTall(ratio=0.75, **layout_theme),
    layout.max.Max(**layout_theme),
]
floating_layout = layout.Floating(**layout_theme)

widget_defaults = dict(
    font='Arial',
    fontsize=12,
    padding=3,
)


def num_screens():
    process = subprocess.Popen(["xrandr"], stdout=subprocess.PIPE)
    out = process.communicate()[0].decode("utf-8").split("\n")
    i = 0
    for line in out:
        if " connected " in line:
            i += 1
    return i


style = {
    "padding": 5,
}

sep = {
    "foreground": colors["lgrey"],
    "padding": 15,
}


def create_screen(primary=False):
    return Screen(
        # Let's have a gap on the bottom, but instead of showing a wallpaper,
        # make it seamless with emacs and termianl backgrounds
        bottom=bar.Bar([widget.TextBox("")], 15, background=base16_chalk["black"]),

        top=bar.Bar([

            widget.Spacer(length=5),

            # Logo
            widget.TextBox(
                text=icons["logo"],
                fontsize=14,
                mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn("urxvt")},
                foreground=base16_chalk["magenta"],
                padding_y=5,
                **style
            ),
            widget.Sep(**sep),


            # Workspaces
            widget.GroupBox(
                highlight_method="text",
                urgent_alert_method="text",
                this_current_screen_border=base16_chalk["blue"],
                active=base16_chalk["white"],
                inactive=base16_chalk["gray"],
                rounded=False,
                fontsize=14,
                hide_unused=True,
                **style,
            ),
            widget.Sep(**sep),


            # Current layout
            CurrentLayoutTextIcon(
                fun=get_layout_icon,
                length=20,
                foreground=base16_chalk["green"],
                **style
            ),
            widget.Sep(**sep),


            widget.TaskList(
                icon_size=0,
                background=colors["greybg"],
                foreground=base16_chalk["white"],

                highlight_method="text",
                border=base16_chalk["blue"],
                urgent_border=base16_chalk["red"],
            ),


            # Notify
            # We want low priority color to be also red because some
            # applications (not looking at you Spotify) are using that color for
            # highlights.
            widget.Spacer(length=100),
            widget.Notify(
                default_timeout=15,
                foreground=base16_chalk["white"],
                foreground_low=base16_chalk["red"],
                foreground_urgent=base16_chalk["red"],
                **style
            ),
            widget.Spacer(length=100),


            # Emails
            # widget.TextBox(
            #     text=icons["email"],
            #     foreground=base16_chalk["green"],
            #     **style
            # ),
            # Mu(
            #     "/home/jkadlcik/Mail",
            #     "/seznam/I/BOX",
            #     "frostyx@email.cz",
            #     foreground=base16_chalk["green"],
            #     **style
            # ),
            # widget.TextBox(
            #     text=icons["gmail"],
            #     foreground=base16_chalk["green"],
            #     **style
            # ),
            # Mu(
            #     "/home/jkadlcik/Mail",
            #     "/gmail/*",
            #     "jakub.kadlcik@gmail.com",
            #     foreground=base16_chalk["green"],
            #     **style
            # ),
            # widget.Sep(**sep),


            # Temp
            widget.TextBox(
                text=icons["temp"],
                foreground=base16_chalk["yellow"],
                **style
            ),
            widget.ThermalSensor(
                threshold=65,
                foreground=base16_chalk["yellow"],
                foreground_alert=colors["red"],
                **style
            ),
            widget.Sep(**sep),


            # Battery
            widget.TextBox(
                text=icons["battery"],
                foreground=base16_chalk["magenta"],
                **style
            ),
            widget.Battery(
                battery_name=battery,
                foreground=base16_chalk["magenta"],
                format="{percent:2.0%}",
                low_foreground=colors["red"],
                **style
            ),
            widget.Sep(**sep),


            # Light
            widget.TextBox(
                text=icons["light"],
                foreground=base16_chalk["blue"],
                **style
            ),
            widget.Backlight(
                brightness_file="/sys/class/backlight/intel_backlight/actual_brightness",
                max_brightness_file="/sys/class/backlight/intel_backlight/max_brightness",
                foreground=base16_chalk["blue"],
                **style
            ),
            widget.Sep(**sep),


            # Volume
            widget.TextBox(
                text=icons["volume"],
                foreground=base16_chalk["green"],
                **style
            ),
            widget.Volume(
                get_volume_command=(vol_cur.split() if vol_cur else None),
                foreground=base16_chalk["green"],
                **style
            ),
            widget.Sep(**sep),


            # Available updates
            widget.TextBox(
                text=icons["sync"],
                foreground=base16_chalk["yellow"],
                **style
            ),
            widget.CheckUpdates(
                distro="Fedora",
                display_format="{updates}",
                no_update_string="0",
                foreground=base16_chalk["yellow"],
                colour_no_updates=base16_chalk["yellow"],
                colour_have_updates=base16_chalk["yellow"],
                **style,
            ),
            widget.Sep(**sep),


            # Time
            widget.Clock(
                timezone="Europe/Prague",
                format="%H:%M",
                foreground=base16_chalk["magenta"],
                **style
            ),
            widget.Sep(**sep),


            # Date
            widget.Clock(
                timezone="Europe/Prague",
                format="%d. %m. (%b) %Y",
                foreground=base16_chalk["blue"],
                **style
            ),
            widget.Sep(**sep),


            # Week
            widget.Clock(
                timezone="Europe/Prague",
                format="#%W",
                foreground=base16_chalk["green"],
                **style
            ),
            widget.Sep(**sep),


            # The meaning of this date is a private matter
            # DaysCounter(
            #     starting_date=date(year=2019, month=2, day=3),
            #     foreground=base16_chalk["yellow"],
            # ),
            # widget.Sep(**sep),


            # Systray
            (widget.Systray() if primary else widget.TextBox(
                icons["systray"],
                fontsize=14,
                foreground=base16_chalk["magenta"]
            )),


            widget.Spacer(length=5),
        ], 25, background=colors["greybg"]),
    )

# Generate the same screen and panel configuration for each monitor
screens = []
for i in range(num_screens()):
    primary = i == 1
    screens.append(create_screen(primary))

# This won't be that simple as returning screens as they are or returning them
# in a reversed order. They are somehow being recognized in a random order and
# we need to define a correct order based on their IDs or inputs.
#
# But before doing so, reversing the screens usually temporarily fixes the issue
# of what screen should be left/right of each other.
# @FIXME
# screens.reverse()


# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]
follow_mouse_focus = False
bring_front_click = False


dgroups_key_binder = None
dgroups_app_rules = [
    # floating windows
    Rule(Match(wm_class=['Synfigstudio', 'Wine', 'Xephyr', 'postal2-bin']), float=True),
]
main = None
cursor_warp = False
auto_fullscreen = True
wmname = "LG3D"

# Autostart
@hook.subscribe.startup_once
def autostart():
    home = expanduser("~")
    subprocess.Popen([home + "/.config/qtile/autostart.sh"])


# xrandr --output DP2 --auto --right-of eDP1
@hook.subscribe.screen_change
def restart_on_randr(qtile, ev):
    # qtile.cmd_restart()
    pass



# Go to the group in the proper monitor, instead of switching the current one
# https://github.com/qtile/qtile/issues/1271#issuecomment-458107043
