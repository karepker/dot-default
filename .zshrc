# Lines configured by zsh-newuser-install
HISTFILE=~/.cache/zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory extendedglob nomatch notify
unsetopt autocd beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/karepker/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export ZSH=/usr/share/oh-my-zsh/
ZSH_THEME="robbyrussell"

source $ZSH/oh-my-zsh.sh
