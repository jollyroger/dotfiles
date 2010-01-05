# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# add Cabal executables in PATH
if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$HOME/.cabal/bin:$PATH"
fi


# Setting variables for using gpg-agent. This includes ssh-agent emulation also
# (see /etc/X11/Xdefaults.d/90gpg-agent)
GPG_ENV_FILENAME=${HOME}/.gnupg/gpg-agent-info-`hostname`
if [ -f "$GPG_ENV_FILENAME" ]; then
    . $GPG_ENV_FILENAME
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
fi

# Taken from man gpg-agent
export GPG_TTY=`tty`
