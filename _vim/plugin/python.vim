function! VimuxPySlime()
  call VimuxSendText(@v)
  call VimuxSendKeys("Enter")
  call VimuxSendKeys("Enter")
endfunction

autocmd FileType python vnoremap <Leader>vs "vy :call VimuxPySlime()<CR>
