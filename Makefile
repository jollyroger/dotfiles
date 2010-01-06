#!/usr/bin/make -f

USER_SETTINGS = home
SYSTEM_SETTINGS = etc

install: install-system install-user

install-system: hal-config xorg-config

install-user: console-apps-config x11-apps-config

console-apps-config: shell-config vim-config gpg-agent-config

x11-apps-config: wm-config vimperator-config tkabber-config

hal-config:
	cp -R $(SYSTEM_SETTINGS)/hal /etc

xorg-config:
	cp -R $(SYSTEM_SETTINGS)/X11 /etc

shell-config:
	cp $(USER_SETTINGS)/.zshrc $(USER_SETTINGS)/.profile $(HOME)/

vim-config:
	cp -R $(USER_SETTINGS)/.vimrc $(USER_SETTINGS)/.vim $(HOME)/

gpg-agent-config:
	cp -R $(USER_SETTINGS)/.gnupg $(HOME)/
	echo "Do not forget to add your keys using ssh-add"

wm-config:
	cp -R $(USER_SETTINGS)/.xmonad $(HOME)/
	cp $(USER_SETTINGS)/.stalonetray $(HOME)/
	cp $(USER_SETTINGS)/.xmobarrc $(HOME)/
	cp $(USER_SETTINGS)/.wallpaper.xpm $(HOME)/

vimperator-config:
	cp $(USER_SETTINGS)/.vimperatorrc $(HOME)/

tkabber-config:
	cp -R $(USER_SETTINGS)/.tkabber $(HOME)/
