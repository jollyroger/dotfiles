# Starting up screensaver
xscreensaver -no-splash &

# Enable compositing support
xcompmgr -n &

# Setting the wallpaper
feh --scale-bg ~/.wallpaper.xpm &

# Enable system tray
stalonetray &

#$$HOME/scripts/dzen2/statusbar/status.sh &
exec xmonad
