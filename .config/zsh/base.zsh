# The bulk of my zsh setup.
#
# Anything that is common to all my environments can go here, the only exception
# is stuff that needs not to exist in a certain environment.
#
# This file should be sourced by each leaf environment's specific .zshrc file
# first, then any settings in it can be overwritten as necessary.

#
# Lines configured by zsh-newuser-install
#
HISTFILE=~/.cache/zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory extendedglob nomatch notify
unsetopt autocd beep
bindkey -e

#
# The following lines were added by compinstall
#
zstyle :compinstall filename '/home/karepker/.zshrc'

autoload -Uz compinit
compinit

#
# Appearance
#

PROMPT='%F{green}%#%f '
RPROMPT='%F{blue}%m%f:%F{yellow}%~%f  '

#
# Hooks
#

# Runs before every prompt.
# Inspiration: https://github.com/robbyrussell/oh-my-zsh/issues/5700#issuecomment-316111109
function precmd () {
	echo -ne "\033]0; ${PWD##*/}\007"
}

#
# Environment variables
#
export EDITOR=vim
export GCC_COLORS=true
export LESS='-R'

#
# Aliases
#
alias cp='cp -L'  # Why the hell would I ever want to copy a symlink?
alias cgrep='grep --color=auto -RIn'
alias lessr='less -R'

# Change into the canonical directory name
alias cdc='cd "$(pwd -P)"'

# This makes less sense when I'm using zsh, but keep it for backwards
# compatibility
alias soba="source ${HOME}/.zshrc"

# I'm not used to the zsh git plugin aliases.
alias gs='git status'  # Take precedence over ghostscript `gs` command.
alias gb='git branch'
alias gd='git difftool'

# Makes ghostscript callable.
alias ghostscript='/usr/bin/gs'

alias ed='gvim'

# Aliases only useful on GNU/Linux platforms.
if [[ "$(uname -s)" == "Linux" ]]; then
	alias ls='ls --color=auto'
	alias open='xdg-open' # `open` like OS X
fi

# Aliases only useful on macOS platforms.
if [[ "$(uname -s)" == "Darwin" ]]; then
	alias ls='ls -Ghf'
	alias gvim='mvim'
fi

#
# PATH management
#
typeset -U path
path+=${HOME}/bin
