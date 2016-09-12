" Vim syntax file
" Language: LC2K assembly instruction set
" Maintainer: Brian Fulper
" Last Revision: 30 Jan 2013

if exists("b:current_syntax")
    finish
endif

syn keyword asMem  sw lw
syn keyword asMath add nand
syn keyword asOps  beq jalr
syn keyword asEnd  noop halt

" Match syntax
syn match asNum '[-]\?\d'

syn match asPre '\.fill'

syn match asComment "#.*$"  contains=asTodo
syn match asComment ";.*$"  contains=asTodo
syn match asComment "//.*$" contains=asTodo

syn match asLabel '[a-zA-Z][a-zA-Z0-9]*'

syn keyword asTodo  TODO XXX FIXME

" Highlight syntax
hi def link asEnd       Special
hi def link asMem       Statement
hi def link asMath      Type
hi def link asOps       Identifier
hi def link asNum       Constant
hi def link asPre       PreProc
hi def link asComment   Comment
hi def link asTodo      Todo
hi def link asLabel		Label
