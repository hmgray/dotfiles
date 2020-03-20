
function! ScratchBuffer()
        edit *scratch*
        setlocal buftype=nofile bufhidden=hide
        lcd ~
endfunction

map <Leader>sb :call ScratchBuffer()<CR>
