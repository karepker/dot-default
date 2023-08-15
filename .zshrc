# Source all scripts in ~/.config/zsh/ for modularity.
for script in $HOME/.config/zsh/*.zsh; do
	source $script
done
