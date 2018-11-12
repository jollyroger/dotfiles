#export XDG_CURRENT_DESKTOP=gnome

# Starting up screensaver
#xscreensaver -no-splash &

# Enable compositing support
xcompmgr -n &

# Setting the wallpaper
#feh --bg-scale ~/.wallpaper &

# Setting keyboard layouts
setxkbmap -layout "us,ua" -variant "," -option "grp:caps_toggle,grp_led:caps,compose:ralt,terminate:ctrl_alt_bksp"

# Enable system tray
stalonetray &

# Enable scanner 
/opt/brother/scanner/brscan-skey/brscan-skey &

nm-applet &

# Autostarting some applications
#cadence --minimized &
volumeicon &
#gpg-agent --daemon &
#NUT-Monitor -F deinformer -H &
#skype &
#urxvtd -q -f -o

#$$HOME/scripts/dzen2/statusbar/status.sh &
exec xmonad
