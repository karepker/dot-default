# Aliases

platform="Linux"
# Determine platform
if [[ "$(uname -s)" == "Darwin" ]]; then
	platform="OS X"
else
	platform="Linux"
fi

if [[ $platform == "Linux" ]]; then
	alias ls='ls --color=auto'
	# `open` like OS X
	alias open='xdg-open'
elif [[ $platform == "OS X" ]]; then
	alias ls='ls -Ghf'
	alias gvim='mvim'
fi

alias cp='cp -L'  # Why the hell would I ever want to copy a symlink?
alias cgrep='grep --color=auto -RIn'
alias lessr='less -R'

# Change into the canonical directory name
alias cdc='cd $(pwd -P)'

alias soba="source ${HOME}/.bashrc"

alias ed="emacsclient -c -n"

# Emacs aliases
alias emacs_kill="emacsclient -e '(kill-emacs)'"
alias emacs_start='emacs --daemon'

# Git aliases
alias gs='git status'  # Take precedence over ghostscript `gs` command.
alias gb='git branch'
alias gd='git diff'

# Makes ghostscript callable.
alias ghostscript='/usr/bin/gs'
