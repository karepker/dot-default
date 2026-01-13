" Functions for toggling and setting text wrap modes.

" Toggles between hard line breaks and soft line breaks.
function! wrap#ToggleWrap()
	" If textwidth is currently set (hard breaks are ON), we turn them OFF.
	if &l:textwidth > 0
		" Save the current textwidth so we can restore it later.
		let b:hard_break_textwidth = &l:textwidth
		" Disable hard breaks.
		setlocal textwidth=0
		setlocal colorcolumn=0
	" If textwidth is not set (hard breaks are OFF), we turn them ON.
	else
		" Check if we have a previously saved textwidth value.
		if exists('b:hard_break_textwidth')
			" Restore the saved value.
			let &l:textwidth = b:hard_break_textwidth
			" Clean up the variable to reset the state.
			unlet b:hard_break_textwidth
		else
			" Prefer the global textwidth as a fallback. If that's not set, just
			" use 80.
			let &l:textwidth = (&g:textwidth > 0) ? &g:textwidth : 80
		endif
		let &l:colorcolumn = &l:textwidth
	endif
	" Always toggle the visual linebreak option.
	setlocal linebreak!
endfunction

" Sets hard wrap mode for ftplugins. Optionally accepts a textwidth argument.
function! wrap#SetHardWrap(...)
	let l:width = (a:0 > 0 && a:1 > 0) ? a:1 : ((&g:textwidth > 0) ? &g:textwidth : 80)
	let &l:textwidth = l:width
	let &l:colorcolumn = l:width
	setlocal nolinebreak
endfunction

" Sets soft wrap mode for ftplugins.
function! wrap#SetSoftWrap()
	setlocal textwidth=0
	setlocal colorcolumn=0
	setlocal linebreak
endfunction
