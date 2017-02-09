# ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='\u@\h:\w$ '
# Sets the prompt title to whatever the current directory is.
PROMPT_COMMAND='echo -ne "\033]0; ${PWD##*/}\007"'

# Modularize bash settings.
if [[ -d ${HOME}/.bash.d ]]; then
    for f in ${HOME}/.bash.d/*; do
        . $f
    done
fi

export EDITOR=vim
export GCC_COLORS=true
export LESS="-R"

# Personal additions to $PATH.
if [[ -d ${HOME}/path ]]; then
	PATH=${HOME}/path:${PATH}
fi

# Ruby additions to $PATH.
GEMS_PATH=".gem/ruby/2.4.0/bin"
if [[ -d "${HOME}/${GEMS_PATH}" ]]; then
	PATH=${PATH}:"${HOME}/${GEMS_PATH}"
fi

[[ -f ${HOME}/links/third_party/git-completion.bash ]] && \
	. ${HOME}/links/third_party/git-completion.bash
