# The bulk of my zsh setup.
#
# Anything that is common to all my environments can go here, the only exception
# is stuff that needs not to exist in a certain environment.
#
# This file should be sourced by each leaf environment's specific .zshrc file
# first, then any settings in it can be overwritten as necessary.

#
# History options.
# Inspired by https://www.jefftk.com/p/logging-shell-history-in-zsh.
#
HISTFILE=~/.cache/zsh_history
# Number of commands from the history file loaded into memory.
HISTSIZE=1000
# Number of commands that are stored in the history file.
SAVEHIST=1000000000
setopt inc_append_history extended_history extended_glob nomatch notify
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
RPROMPT='%F{cyan}%m%f:%F{yellow}%~%f  '

#
# Hooks
#

# Runs before every prompt.
# Inspiration: https://github.com/robbyrussell/oh-my-zsh/issues/5700#issuecomment-316111109
function precmd () {
	# Append history to a full history log, inspired by
	# https://www.jefftk.com/p/logging-shell-history-in-zsh.
	echo "$(date -Iseconds) $(uname -n):$PWD $(history -1)" \
		      >> ~/.cache/zsh_full_history
	echo -ne "\033]0; ${PWD##*/}\007"
}

# Bash style word selection, so C-W doesn't delete an entire path but only each
# individual directory.
# Inspiration:
# https://zsh.sourceforge.io/Doc/Release/User-Contributions.html#Widgets via
# https://stackoverflow.com/a/1438523.
autoload -U select-word-style
select-word-style bash

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

#
# PATH management
#
typeset -U path
path+=${HOME}/bin
