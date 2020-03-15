nnoremap <F5> :call RunR()<CR>

function RunR()
	silent !urxvt -e sh -c "Rscript % && read -n 1 && exit"&
endfunction
