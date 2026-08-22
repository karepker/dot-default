# `dot-default`

Defaults dotfile configurations, maintained with
[dotfiler](https://github.com/svetlyak40wt/dotfiler).

## Vim plugins

Special note on installing vim plugins, since I don't want to bother with git
submodules right now.

I use vim's default package manager.

Here's a recipe for installing plugins:

```
mkdir -p ~/.config/vim/pack/plugin/start/
cd ~/.config/vim/pack/plugin/start
# For each plugin in this list, clone and install it.
while read -r plugin; do
    git clone https://github.com/$plugin
done <<EOF
tpope/vim-surround
tpope/vim-repeat
tpope/vim-abolish
tpope/vim-eunuch
tpope/vim-unimpaired
kana/vim-textobj-user
kana/vim-textobj-function
Julian/vim-textobj-variable-segment
sgur/vim-textobj-parameter
glts/vim-textobj-comment
sickill/vim-sunburst
EOF
```

The `plugin` subdirectory name in the directory above is arbitrary. vim's
package manager allows one to group plugins into packages. I just have one group
for now that's called `plugin`.

## "Default" plugins

These were originally plugins that I included in my `dot-default` repo:

* `tpope/vim-surround`: Select surrounding delimiters
* `tpope/vim-repeat`: Repeat custom vim commands.
* `tpope/vim-abolish`: Easier word manipulations.
* `tpope/vim-eunuch`: Easier unix commands for vim.
* `tpope/vim-unimpaired`: Common matching commands.

Custom vim text objects.

* `kana/vim-textobj-user`: Base plugin to provide custom text objects.
* `kana/vim-textobj-function`: Function object mapped to "f/F".
* `Julian/vim-textobj-variable-segment`: Segments of camel- or snake-cased words with "v".
* `sgur/vim-textobj-parameter`: Function arguments with ",".
* `glts/vim-textobj-comment`: Comment blocks with "c/C".

* `sickill/vim-sunburst`: Color schemes.

## "Personal" plugins

These were originally plugins that I included in my `dot-personal` repo:

* `octol/vim-cpp-enhanced-highlight`: Better C++ highlighting for C++11/14

Google vim plugins.

* `google/vim-maktaba`: Google's vim plugin library.
* `google/vim-glaive`: Configure google vim plugins.
* `google/vim-codefmt`: Google's code formatting plugin.
