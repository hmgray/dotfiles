function! VimuxSlime()
  call VimuxSendText(@v)
  call VimuxSendKeys("Enter")
endfunction

map <Leader>vl :VimuxRunLastCommand<CR>
map <Leader>vp :VimuxPromptCommand<CR>
map <Leader>vq :VimuxCloseRunner<CR>
vmap <Leader>vs "vy :call VimuxSlime()<CR>
nmap <Leader>vs vip<Leader>vs<CR>
map <Leader>vx :VimuxInterruptRunner<CR>
