# This file contains aliases only useful on GNU/Linux platforms.
if [[ "$(uname -s)" != "Linux" ]]; then
    return
fi

alias ls='ls --color=auto'
alias open='xdg-open' # `open` like OS X
