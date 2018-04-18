# Aliases

alias cp='cp -L'  # Why the hell would I ever want to copy a symlink?
alias cgrep='grep --color=auto -RIn'
alias lessr='less -R'

# Change into the canonical directory name
alias cdc='cd $(pwd -P)'

alias soba="source ${HOME}/.bashrc"

# Emacs aliases
alias emacs_kill="emacsclient -e '(kill-emacs)'"
alias emacs_start='emacs --daemon'
