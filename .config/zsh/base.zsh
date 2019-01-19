# The bulk of my zsh setup.
#
# Anything that is common to all my environments can go here, the only exception
# is stuff that needs not to exist in a certain environment.
#
# This file should be sourced by each leaf environment's specific .zshrc file
# first, then any settings in it can be overwritten as necessary.

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

export ZSH=/usr/share/oh-my-zsh
export ZSH_PERSONAL=${HOME}/.config/zsh
export ZSH_CUSTOM=${ZSH_PERSONAL}/custom
ZSH_THEME="robbyrussell"

source $ZSH/oh-my-zsh.sh
