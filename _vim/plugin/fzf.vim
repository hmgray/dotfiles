" These shortcuts are only set if `fzf` is installed; the FZFGrep function
" requires Ripgrep (`rg`).

if executable("fzf")
	if filereadable("/usr/share/doc/fzf/examples/fzf.vim")
		source /usr/share/doc/fzf/examples/fzf.vim
	endif
	nnoremap <Leader>l :FZFBLines<CR>
	nnoremap <Leader>b :FZFBuffers<CR>
	nnoremap <Leader>f :FZFFiles<CR>
	nnoremap <Leader>g :FZFGrep<CR>
endif

function! s:grep_jump(l)
  let keys = split(a:l, ':')
	exec "e " . keys[0]
  exec keys[1]
  normal! ^zz
endfunction

function! s:buffer_lines()
  let l:lines = []
	call extend(l:lines, map(getline(0,"$"), '(v:key + 1) . "\t" . v:val '))
  return l:lines
endfunction

function! s:header_lines()
	let l:blines = s:buffer_lines()
  let l:hlines = []
	for line in l:blines
		if line =~ "^[0-9]*[	]#[ ]" || line =~ "^[0-9]*[	]##[ ]"
			call add(l:hlines, line)
		endif
	endfor
  return l:hlines
endfunction

function! s:line_jump(l)
  let keys = split(a:l, '\t')
  exec keys[0]
  normal! ^zz
endfunction

function! s:other_buffers()
	let l:buffers = []
	for n in range(1, bufnr('$'))
		if bufname("%") != bufname(n) && bufname(n) != bufname("#") && buflisted(bufname(n))
			call add(l:buffers, bufname(n))
		endif
	endfor
	call insert(l:buffers, bufname("#"))
	return l:buffers
endfunction

command! FZFBLines call fzf#run({
			\ 'source':  <sid>buffer_lines(),
			\ 'sink':    function('<sid>line_jump'),
			\ 'options': '--tac --nth=2..',
			\ 'down':    '60%'
			\})

command! FZFBuffers call fzf#run({
			\ 'source':  <sid>other_buffers(),
			\ 'sink':    'buffer',
			\ 'down':    '60%'
			\})

command! FZFFiles call fzf#run({
		\ 'source': "find * -type f -not -path '*/\.*'",
		\ 'sink':   'e',
		\ 'down': '60%'
		\})

command! FZFGrep call fzf#run({
			\ 'source':  'rg -S -n "' . input("FZFGrep: ") . '"',
			\ 'sink':    function('<sid>grep_jump'),
			\ 'down':    '60%'
			\})
