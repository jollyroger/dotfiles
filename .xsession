# Starting up screensaver
xscreensaver -no-splash &

# Enable compositing support
#xcompmgr -c &

# Setting the wallpaper
xpmroot ~/.wallpaper.xpm &

# Enable system tray
stalonetray &

#$$HOME/scripts/dzen2/statusbar/status.sh &
exec xmonad
