" This set of [0-9]-*.vim 'plugins' contains all Vundle related settings. It is
" broken up into multipe files to allow for a modular set of plugins that
" may depend on environment.
"
" Files correspond to:
"
" * 0-initialization.vim (this file): Mandatory Vundle initialization.
" * {1-9}-*.vim: Vundle plugins to load.
" * 9-termination.vim: Mandatory Vundle termination steps.
"
" Because vim will load files in lexicographic order, users may sequence plugin
" loads by putting them in higher numbered [1-9]-*.vim files. Plugin loads may
" also be sequenced as they usually are: by line number within a file.
"
" This 'plugin' contains all mandatory Vundle intialization.

set nocompatible
filetype off

" Set the runtime path to include Vundle and initialize.
set runtimepath+=~/.vim/bundle/Vundle.vim
" Install to a non-standard directory because storing this in Dropbox messes up
" all the permissions.
call vundle#begin('~/.vundle')

" Let Vundle manage itself.
Plugin 'VundleVim/Vundle.vim'
