set spell

set shiftwidth=2
set tabstop=4
set expandtab

set textwidth=79

nnoremap <Leader>w :call WriteAndPDF()<CR>
nnoremap <Leader>wq :call WriteAndPDF()<CR>:q<CR>

function WriteAndPDF()
	write
    "silent !pandoc -t html --css ~/.config/nvim/ftplugin/pandoc.css % -o %<.pdf
	
	!pandoc % -H ~/.config/nvim/ftplugin/head.tex -V 'fontsize: 12pt' -o %<.pdf & 

endfunction
