import re
import subprocess
from os import uname
from os.path import expanduser
from libqtile.config import Key, Screen, Group, Drag, Click, Match
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook


terminal     = "gnome-terminal"
run          = "gmrun"
vol_cur      = "amixer -D pulse get Master"
vol_up       = "amixer -q -D pulse sset Master 2%+"
vol_down     = "amixer -q -D pulse sset Master 2%-"
mute         = "amixer -q -D pulse set Master toggle"
bright_up    = "xbacklight -inc 10"
bright_down  = "xbacklight -dec 10"
lock         = "gnome-screensaver-command -l"
scrot        = ""


hostname = uname()[1]
if hostname == "unused-4-222.brq.redhat.com":
	scrot = "/home/jkadlcik/.bin/screenshot.sh"


mod = "mod1" # Left alt
sup = "mod4" # Left win-key

keys = [
	# Switch window focus to other pane(s) of stack
	Key([mod], "Tab", lazy.layout.next()),

	Key([mod], "Return", lazy.spawn(terminal)),
	Key([mod], "F1", lazy.spawn(terminal)),
	Key([mod], "F2", lazy.spawn(run)),

	# Toggle between different layouts as defined below
	Key([mod], "space", lazy.nextlayout()),
	Key([mod], "F4", lazy.window.kill()),

	Key([mod, "control"], "r", lazy.restart()),
	Key([mod, "control"], "q", lazy.shutdown()),

	Key([mod], "w", lazy.screen.togglegroup()),

	# cycle to previous and next group
	Key([mod], "h", lazy.screen.prev_group(skip_managed=True)),
	Key([mod], "l", lazy.screen.next_group(skip_managed=True)),

	Key([sup], "f", lazy.window.toggle_fullscreen()),
	Key([sup], "t", lazy.window.toggle_floating()),

	# Process `gnome-screensaver` must run
	Key([mod, sup], "l", lazy.spawn(lock)),

	# Multihead magic
	Key([sup], "h", lazy.to_screen(0)),
	Key([sup], "l", lazy.to_screen(1)),

	# Multimedia
	Key([], "XF86AudioRaiseVolume", lazy.spawn(vol_up)),
	Key([], "XF86AudioLowerVolume", lazy.spawn(vol_down)),
	Key([], "XF86AudioMute", lazy.spawn(mute)),
	Key([], "XF86MonBrightnessUp", lazy.spawn(bright_up)),
	Key([], "XF86MonBrightnessDown", lazy.spawn(bright_down)),
	Key([], "Print", lazy.spawn(scrot)),
]


workspaces = [
	{"name": "i", "key": "i", "matches": [Match(wm_class=["Pidgin"])]},
	{"name": "r", "key": "r", "matches": [Match(wm_class=["Chromium-browser", "Firefox", "Google-chrome"])]},
	{"name": "f", "key": "f", "matches": [Match(wm_class=["Dolphin", "Thunar", "File-roller"])]},
	{"name": "d", "key": "d", "matches": [Match(wm_class=["Lispworks", "jetbrains-pycharm", "Eclipse" ])]},
	{"name": "q", "key": "q", "matches": [Match(wm_class=["Acroread", "Zathura", "Evince"])]},
	{"name": "n", "key": "n", "matches": [Match(wm_class=["Claws-mail"])]},
	{"name": "c", "key": "c"},
	{"name": "v", "key": "v", "matches": [Match(wm_class=["VirtualBox"])]},
	{"name": "g", "key": "g", "matches": [Match(wm_class=["Wine", "Python2.7", "Steam", "Progress"])]}, # Python2.7 is playonlinux; Progress is steam updater
	{"name": "o", "key": "o", "matches": [Match(wm_class=["Vlc"])]},
]

groups = []
for workspace in workspaces:
	matches = workspace["matches"] if "matches" in workspace else None
	groups.append(Group(workspace["name"], matches=matches))
	keys.append(Key([mod], workspace["key"], lazy.group[workspace["name"]].toscreen()))
	keys.append(Key([mod, sup], workspace["key"], lazy.window.togroup(workspace["name"])))


# float dialog windows
@hook.subscribe.client_new
def dialogs(window):
	floating = ["gmrun", "gcr-prompter"]
	wm_type = window.window.get_wm_type()
	wm_class = window.window.get_wm_class()[0]
	transient_for = window.window.get_wm_transient_for()
	if wm_type == 'dialog' or transient_for or wm_class in floating:
		window.floating = True

colors = {
	"grey": "#555555",
	"red": "#DD1144",
	"blue": "#445588",
	"lgrey": "#b8b6b1",
	"green": "#008080",
}

# http://docs.qtile.org/en/latest/manual/ref/layouts.html
layout_theme = {
	"border_width": 1,
	"border_focus": colors["blue"],
	"border_normal": colors["lgrey"],
}
layouts = [
	layout.MonadTall(**layout_theme),
	layout.TreeTab(),
	layout.xmonad.MonadTall(ratio=0.75, **layout_theme),
	layout.max.Max(),
]
floating_layout = layout.Floating(**layout_theme)

widget_defaults = dict(
	font='Arial',
	fontsize=12,
	padding=3,
)

class _WindowTabs(widget.WindowTabs):
	separator = "    |    "
	def update(self):
		widget.WindowTabs.update(self)
		# Very ugly hack
		names = self.text.split(self.separator)
		self.text = self.separator.join(names)
		self.bar.draw()


def num_screens():
	process = subprocess.Popen(["xrandr"], stdout=subprocess.PIPE)
	out = process.communicate()[0].split("\n")
	i = 0
	for line in out:
		if " connected " in line:
			i += 1
	return i

screens = [
	Screen(
		top=bar.Bar([
			# Temp
			widget.TextBox(text="Temp:"),
			widget.ThermalSensor(threshold=65, foreground_alert=colors["red"]),
			widget.Sep(padding=15),

			# Battery
			widget.TextBox(text="Battery:"),
			widget.Battery(battery_name="BAT1", low_foreground=colors["red"]),
			widget.Sep(padding=15),

			# Light
			widget.TextBox(text="Light:"),
			widget.Backlight(
				brightness_file="/sys/class/backlight/intel_backlight/actual_brightness",
				max_brightness_file="/sys/class/backlight/intel_backlight/max_brightness"
			),
			widget.Sep(padding=15),

			# Volume
			widget.TextBox(text="Volume:"),
			widget.Volume(get_volume_command=vol_cur.split()),
			widget.Sep(padding=15),

			widget.Notify(foreground_low=colors["red"][1:], foreground_urgent=colors["red"][1:]),
			widget.Spacer(),
			widget.Clock(timezone="Europe/Prague", format="%H:%M  %d. %m. (%b) %Y"),
		], 25),

		bottom=bar.Bar([
			widget.GroupBox(highlight_method="block", this_current_screen_border=colors["blue"]),
			widget.Sep(padding=15),
			widget.CurrentLayout(),
			widget.Sep(padding=15),
			widget.Prompt(),
			_WindowTabs(),
			widget.Systray(),
		], 25),
	)
]

if num_screens() == 2:
	screens.append(
		Screen(
			bottom=bar.Bar([
				widget.GroupBox(highlight_method="block", this_current_screen_border=colors["blue"]),
				widget.Sep(padding=15),
				widget.CurrentLayout(),
				widget.Sep(padding=15),
				widget.Prompt(),
				_WindowTabs(),
				widget.Systray(),
			], 25)))


# Drag floating layouts.
mouse = [
	Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
	Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
	Click([mod], "Button2", lazy.window.bring_to_front())
]
follow_mouse_focus = False
bring_front_click = False


dgroups_key_binder = None
dgroups_app_rules = []
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
	qtile.cmd_restart()
