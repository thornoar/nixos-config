" Vim filetype detection file
" Language: Typst
augroup filetypedetect
au BufNewFile,BufRead *.typ set filetype=typst
augroup END
filetype plugin on
