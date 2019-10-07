# Solve issues with missing icons for KDE applications like dolphin, see
# https://bugs.launchpad.net/ubuntu/+source/dolphin/+bug/1509562, also be sure
# to install qt5-style-plugins
export XDG_CURRENT_DESKTOP=gnome

# Starting up screensaver
#xscreensaver -no-splash &

# Enable compositing support
xcompmgr -n &

# Setting the wallpaper
feh --bg-scale ~/.wallpaper &

# Setting keyboard layouts
setxkbmap -layout "us,ua" -variant "," -option "grp:caps_toggle,grp_led:caps,compose:ralt,terminate:ctrl_alt_bksp"

# Enable system tray
stalonetray &

# Enable scanner
/opt/brother/scanner/brscan-skey/brscan-skey &

nm-applet &

# Autostarting some applications
cadence --minimized &
volumeicon &
NUT-Monitor -F deinformer -H &
#skype &

## https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=835394
gpg-connect-agent updatestartuptty /bye &

exec xmonad
