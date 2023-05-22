#!/bin/sh

xinput set-prop Synaptics\ TM3289-021 libinput\ Tapping\ Enabled 1 &

# background programs
export $(dbus-launch) # dbus must come first
dbus-update-activation-environment DISPLAY XAUTHORITY WAYLAND_DISPLAY & # for gnome-keyring
feh --randomize --bg-fill ${HOME}/pictures/wallpapers/* --no-fehbg &
dwmblocks &
xautolock -time 10 -locker slock &
redshift &
pipewire >/dev/null 2>&1 &
ibus-daemon -rxR >/dev/null 2>&1 &
picom &
qpwgraph -m &
export $(gnome-keyring-daemon --start --components=secrets,ssh)

exec dwm
