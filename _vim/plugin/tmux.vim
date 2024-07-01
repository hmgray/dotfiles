" When using `tmux`, create a vertical or horizontal split pane in the current
" working directory.
map <Leader>" :silent !tmux split-window -v<CR>
map <Leader>% :silent !tmux split-window -h<CR>

" Send `text` to the currently marked `tmux` pane.  By default, `tmux` allows
" marking a pane with `C-m`.
function! tmux#SendText(text)
	let l:text = ('"' . escape(a:text, '\"$') . '"')
	call system("tmux send-keys -t'{marked}' " . l:text)
endfunction

" Send a visually selected region or the current block to the marked pane.
vnoremap <silent> <Leader>s m'"ry :call tmux#SendText(@r)<CR>``
nnoremap <silent> <Leader>s m'vip"ry :call tmux#SendText(@r)<CR>``

" Repeat the last command in the marked pane by sending an upward arrow
" followed by return
nnoremap <silent> <Leader>r :call tmux#SendText("\u001b[A\n")<CR>
