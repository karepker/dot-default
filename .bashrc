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
    for f in ${HOME}/.bash.d/alias/*.bash; do
        . "$f"
    done

	# Source exports.
    for f in ${HOME}/.bash.d/export/*.bash; do
        . "$f"
    done

	# Add custom directories to path.
    for f in ${HOME}/.bash.d/path/*.bash; do
		while read line; do
			path="${line/#\~/$HOME}"
			[[ -d "$path" ]] && export PATH="${PATH}:$path"
		done < "$f"
    done

	# Source completions.
	# The bash-completion default script has a default location for sourcing
	# completions, see https://github.com/scop/bash-completion#faq, but not
	# every environment I'm using has a version of the bash-completion package
	# with this feature, so hold completions in this additional directory as
	# well.
    for f in ${HOME}/.bash.d/completion/*.bash; do
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
