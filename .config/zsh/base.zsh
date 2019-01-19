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
# oh-my-zsh setup.
#
export ZSH=/usr/share/oh-my-zsh
export ZSH_PERSONAL=${HOME}/.config/zsh
export ZSH_CUSTOM=${ZSH_PERSONAL}/custom
ZSH_THEME="robbyrussell"

source $ZSH/oh-my-zsh.sh

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

# For compiling C++ code, options that I use every time.
alias g++='g++ -Wall -Wextra -Werror -pedantic-errors -std=c++14'
alias clang++='clang++ -Wall -Wextra -Werror -pedantic-errors -std=c++14'

alias pylint='pylint --extension-pkg-whitelist=numpy'

# I'm not used to the zsh git plugin aliases.
alias gs='git status'  # Take precedence over ghostscript `gs` command.
alias gb='git branch'
alias gd='git difftool'

# Makes ghostscript callable.
alias ghostscript='/usr/bin/gs'

alias ed='gvim'

# Aliases only useful on GNU/Linux platforms.
if [[ "$(uname -s)" != "Linux" ]]; then
	alias ls='ls --color=auto'
	alias open='xdg-open' # `open` like OS X
fi

# Aliases only useful on macOS platforms.
if [[ "$(uname -s)" != "Darwin" ]]; then
	alias ls='ls -Ghf'
	alias gvim='mvim'
fi

