
function! ScratchBuffer()
        edit *scratch*
        setlocal buftype=nofile bufhidden=hide noswapfile
        lcd ~
endfunction

map <Leader>sb :call ScratchBuffer()<CR>
