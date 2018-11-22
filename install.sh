#!/bin/sh

install_dotfiles() {
    stow $* -Svt $HOME src
}


list_conflicts() {
    install_dotfiles -n 2>&1 1>/dev/null | \
        grep existing | \
        sed -e 's/^[a-zA-Z* ]*: \(.*\)$/\1/'
}


# Backup all existing files to be replaced
backup() {
BACKUP_DIR=$PWD/backup-`date -Iminutes`

list_conflicts | while read file_path ; do
    dir=$(dirname "$file_path")
    mkdir -p "$BACKUP_DIR/$dir"
    mv "$HOME/$file_path" "$BACKUP_DIR/$dir"
    done
}

restore() {
echo Restoring files:
if [ -d "$1" ] ; then
    BACKUP_DIR="$1"
else
    BACKUP_DIR="$PWD"
fi

cd $BACKUP_DIR
find -type f |cut -c 3-| while read file_path ; do
    echo $file_path
    dir=$(realpath $HOME/$(dirname "$file_path"))
    mkdir -p "$dir"
    cp -a "$file_path" "$dir"
    done
cd $OLDPWD
echo Done.
}

install_global_deps() {
sudo apt install pass dmenu fonts-powerline stow git
ln -s /usr/share/doc/pass/examples/dmenu/passmenu ~/.bin/passmenu
}

install_fonts() {
set -e
TMP_FONTS_DIR=`mktemp -d`

git clone https://github.com/powerline/fonts $TMP_FONTS_DIR --depth=1
$TMP_FONTS_DIR/install.sh
rm -rf $TMP_FONTS_DIR
}

install_vim_plug() {
    curl -fLo src/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

#backup
#install_global_deps
#install_fonts
#install_vim_powerline
#vim +PlugInstall\|qa
