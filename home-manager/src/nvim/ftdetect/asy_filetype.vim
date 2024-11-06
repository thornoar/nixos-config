" Vim filetype detection file
" Language: Asymptote
augroup filetypedetect
au BufNewFile,BufRead *.asy set filetype=asy
augroup END
filetype plugin on
