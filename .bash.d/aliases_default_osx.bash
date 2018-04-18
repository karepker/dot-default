# This file contains aliases only useful on OS X platforms.
if [[ "$(uname -s)" != "Darwin" ]]; then
    return
fi

alias ls='ls -Ghf'
alias gvim='mvim'
