. ~/.profile

# Force to use emacs mode
bindkey -e

# Set xterm title
case $TERM in (xterm*|rxvt)
precmd () { print -Pn "\e]0;%n@%m: %~\a" }
preexec () { print -Pn "\e]0;%n@%m: $1\a" }
;;
esac

# History settings
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
 
setopt HIST_VERIFY 		# Show command from history before actually execute it
setopt APPEND_HISTORY 		# Append history to the end of the history file
setopt HIST_EXPIRE_DUPS_FIRST 	# Remove duplicated events when histfile is overfilled
setopt HIST_FIND_NO_DUPS 	# Do not show history duplicates while search
setopt HIST_REDUCE_BLANKS 	# Reduce unncessary blanks before putting event to history
setopt HIST_NO_STORE 		# Do not store history invoking commands in history
setopt HIST_IGNORE_SPACE	# Do not put event started with space to history

# Basic options
setopt beep

# Compinstall
zstyle :compinstall filename '~/.zshrc'
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
autoload -Uz compinit
compinit

# User aliases
alias ls='ls --color'
alias grep='grep --color'

# Color definitions
fg_black='%{\033[30m%}'
fg_red=$'%{\e[1;31m%}'
fg_green='%{\033[32m%}'
fg_yellow='%{\033[33m%}'
fg_blue='%{\033[34m%}'
fg_magenta='%{\033[35m%}'
fg_cyan='%{\033[36m%}'
fg_white='%{\033[37m%}'
fg_normal='%{\033[0m%}'

# Propmt settings
export PROMPT='[%n@%m:%c]%# '
export RPROMPT='[%*]'

# QEMU SOUND PARAMETER
export QEMU_AUDIO_DRV=dsound

# Mail settings
export MAIL=/var/mail/$USER

bindkey '\E[5~' history-beginning-search-backward # Search history backward with PgUp
bindkey '\E[6~' history-beginning-search-forward  # Search history forward with PgDown

bindkey '\E[1;5D' backward-word # Word navigation back with Ctrl+Left
bindkey '\E[1;5C' forward-word  # Word navigation forward with Ctrl+Right

#export LESS_TERMCAP_mb=$'\E[01;31m'
#export LESS_TERMCAP_md=$'\E[01;37m'
#export LESS_TERMCAP_me=$'\E[0m'
#export LESS_TERMCAP_se=$'\E[0m'
#export LESS_TERMCAP_so=$'\E[01;44;33m'
#export LESS_TERMCAP_ue=$'\E[0m'
#export LESS_TERMCAP_us=$'\E[01;36m'
