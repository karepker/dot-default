setlocal expandtab
setlocal softtabstop=2
setlocal tabstop=2
setlocal shiftwidth=2

" Turn off extra whitespace at the end of lines when in markdown mode.
augroup highlight_extra_whitespace
	autocmd!
augroup END

" Show trailing whitespace instead as dots.
setlocal listchars=trail:.
setlocal list

" Functions to display/not display list characters
function! SetListIfMarkdown()
	if (&filetype ==# 'markdown')
		setlocal list
	endif
endfunction

function! SetNoListIfMarkdown()
	if (&filetype ==# 'markdown')
		setlocal nolist
	endif
endfunction

" Turn off trailing whitespace highlighting in insert mode.
" This doesn't quite work because it activates for all files. Why?
augroup list_trailing_whitespace
	autocmd!
	autocmd InsertEnter * :call SetNoListIfMarkdown()
	autocmd InsertLeave * :call SetListIfMarkdown()
augroup END

" Turn on spelling for markdown files.
setlocal spell
