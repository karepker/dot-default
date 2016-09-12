if exists("did_load_filetypes")
	finish
endif
augroup filetypedetect
	au BufRead,BufNewFile *.as setfiletype lc2k
augroup END
