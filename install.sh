#!/bin/sh

REPO_DIR=$(dirname $(readlink -f "$0"))
cd "$REPO_DIR"

install_dotfiles() {
    ls --color=none packages | xargs stow -vt $HOME -d packages
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
        echo "Usage: restore <backup_dir>"
        exit 1
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
    sudo apt install
        pass \
        dmenu \
        fonts-powerline \
        fonts-hack \
        stow \
        curl \
        git
    ln -s /usr/share/doc/pass/examples/dmenu/passmenu ~/.bin/passmenu
}

install_vim_plugins() {
    curl -fLo $REPO_DIR/packages/vim/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    vim +PlugInstall +qa
}

install_global_deps
install_vim_plugins
install_dotfiles
