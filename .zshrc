# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt beep
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/jolly_roger/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

alias ls='ls --color'
alias grep='grep --color'

fg_black='%{\033[30m%}'
fg_red=$'%{\e[1;31m%}'
fg_green='%{\033[32m%}'
fg_yellow='%{\033[33m%}'
fg_blue='%{\033[34m%}'
fg_magenta='%{\033[35m%}'
fg_cyan='%{\033[36m%}'
fg_white='%{\033[37m%}'
fg_normal='%{\033[0m%}'
export PROMPT='[%n@%m:%c]%# '
export RPROMPT='[%*]'

#QEMU SOUND PARAMETER
QEMU_AUDIO_DRV=dsound
