# ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='\u@\h:\w$ '
# Sets the prompt title to whatever the current directory is.
PROMPT_COMMAND='echo -ne "\033]0; ${PWD##*/}\007"'

# Modularize bash settings.
if [[ -d ${HOME}/.bash.d ]]; then
	# We want nullglob so iterating over an empty list isn't an error.
	shopt -s nullglob

	# Source aliases.
    for f in ${HOME}/.bash.d/alias_*.bash; do
        . "$f"
    done

	# Source exports.
    for f in ${HOME}/.bash.d/export_*.bash; do
        . "$f"
    done

	# Unset nullglob because it can be weird:
	# https://unix.stackexchange.com/a/204944
	shopt -u nullglob
fi

# Personal additions to $PATH.
if [[ -d ${HOME}/path ]]; then
	PATH=${HOME}/path:${PATH}
fi

# Ruby additions to $PATH.
GEMS_PATH=".gem/ruby/2.5.0/bin"
if [[ -d "${HOME}/${GEMS_PATH}" ]]; then
	PATH=${PATH}:"${HOME}/${GEMS_PATH}"
fi

[[ -f ${HOME}/links/third_party/git-completion.bash ]] && \
	. ${HOME}/links/third_party/git-completion.bash
