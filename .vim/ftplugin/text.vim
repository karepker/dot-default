setlocal textwidth=0
" undo highlighting of text that goes over 80 characters
if exists('+colorcolumn')
	setlocal colorcolumn=0
else
	au! TextOverWidth BufWinEnter * 
endif
