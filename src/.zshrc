[ -e ~/.profile ] && . ~/.profile

# Force to use emacs mode
bindkey -e

# Set xterm title
#case $TERM in (xterm*|rxvt)
#    precmd () { print -Pn "\e]0;%n@%m: %~\a" }
#    preexec () { print -Pn "\e]0;%n@%m: $1\a" }
#    ;;
#esac

# History settings
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

[ -d /usr/lib/ccache ] && export PATH=/usr/lib/ccache:$PATH
 
setopt HIST_VERIFY 		# Show command from history before actually execute it
setopt APPEND_HISTORY 		# Append history to the end of the history file
setopt HIST_EXPIRE_DUPS_FIRST 	# Remove duplicated events when histfile is overfilled
setopt HIST_FIND_NO_DUPS 	# Do not show history duplicates while search
setopt HIST_REDUCE_BLANKS 	# Reduce unncessary blanks before putting event to history
setopt HIST_NO_STORE 		# Do not store history invoking commands in history
setopt HIST_IGNORE_SPACE	# Do not put event started with space to history

# Basic options
unsetopt beep

# Compinstall
zstyle :compinstall filename '~/.zshrc'
# expand s-a to sites-available
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
autoload -Uz compinit && compinit
autoload -U colors && colors

# User aliases
alias ls='ls --color'
alias grep='grep --color'
alias timidity='timidity -in'
alias salt-summary='salt --state-verbose=False'
alias salt-short='salt --state-output=changes'

# Propmt settings
export PROMPT='[%n@%m:%c]%# '
export RPROMPT='[%?][%*]'

# QEMU SOUND PARAMETER
export QEMU_AUDIO_DRV=dsound

# Mail settings
export MAIL=/var/mail/$USER

case "${TERM}" in
  cons25*|linux) # plain BSD/Linux console
    bindkey '\e[H'    beginning-of-line   # home 
    bindkey '\e[F'    end-of-line         # end  
    bindkey '\e[5~'   delete-char         # delete
    bindkey '[D'      emacs-backward-word # esc left
    bindkey '[C'      emacs-forward-word  # esc right
    ;;
  *rxvt*) # rxvt derivatives
    bindkey '\e[3~'   delete-char         # delete
    bindkey '\eOc'    forward-word        # ctrl right
    bindkey '\eOd'    backward-word       # ctrl left
    # workaround for screen + urxvt
    bindkey '\e[7~'   beginning-of-line   # home
    bindkey '\e[8~'   end-of-line         # end
    bindkey '^[[1~'   beginning-of-line   # home
    bindkey '^[[4~'   end-of-line         # end
    ;;
  *xterm*) # xterm derivatives
    bindkey '\e[H'    beginning-of-line   # home
    bindkey '\e[F'    end-of-line         # end
    bindkey '\e[3~'   delete-char         # delete
    bindkey '\e[1;5C' forward-word        # ctrl right
    bindkey '\e[1;5D' backward-word       # ctrl left
    # workaround for screen + xterm
    bindkey '\e[1~'   beginning-of-line   # home
    bindkey '\e[4~'   end-of-line         # end
    ;;
  screen)
    bindkey '^[[1~'   beginning-of-line   # home
    bindkey '^[[4~'   end-of-line         # end
    bindkey '\e[3~'   delete-char         # delete
    bindkey '\eOc'    forward-word        # ctrl right
    bindkey '\eOd'    backward-word       # ctrl left
    bindkey '^[[1;5C' forward-word        # ctrl right
    bindkey '^[[1;5D' backward-word       # ctrl left
    ;;
esac

# Debian-specific variables
export DEBFULLNAME="Andrii Senkovych"
export DEBEMAIL="andrii@senkovych.com"
export QUILT_REFRESH_ARGS="-p ab --no-timestamps --no-index"
export QUILT_PATCHES=debian/patches

alias midiplay="fluidsynth -a alsa -m alsa_seq -l -i /usr/share/sounds/sf2/FluidR3_GM.sf2"
alias pmount="pmount -A"

export AWS_CONFIG_FILE=~/.awsrc

export GPG_TTY=$(tty)
if [[ -n "$SSH_CONNECTION" ]] ;then
    export PINENTRY_USER_DATA="USE_CURSES=1"
fi
