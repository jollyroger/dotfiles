#!/bin/sh

# Install 
sudo apt install pass dmenu fonts-powerline
ln -s /usr/share/doc/pass/examples/dmenu/passmenu ~/.bin/passmenu

sudo apt install fonts-powerline

#TODO: install 
# * https://github.com/powerline/fonts
# * https://github.com/junegunn/vim-plug
#
# Run this after installing vimrc and vim-plug
#vim +PlugInstall\|qa
