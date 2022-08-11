set background=dark
highlight clear 

let g:colors_name = 'acmesque'
if exists('syntax_on')
  syntax reset
endif

" black 				#080808
" blonde				#fcfcce
" blue 					#e6ffff
" chartreuse		#cccc7c
" cream					#ffffea
" light gray		#acaca2
" dark gray			#505050
" dark yellow   #cccc7c
" faded black		#303030
" pale red			#ffe6e6
" pale yellow		#ffffca
" wheat					#eaed8d

highlight! Normal guibg=#ffffea guifg=#080808

highlight! Comment guibg=bg guifg=#303030 gui=italic
highlight! Conceal guibg=bg guifg=fg gui=NONE
highlight! Constant guibg=bg guifg=#101010 gui=bold
highlight! CursorLineNr guifg=#080808
highlight! Directory guibg=bg guifg=fg gui=bold
highlight! FoldColumn guibg=#fcfcce guifg=fg
highlight! Folded guibg=#cccc7c guifg=fg gui=italic
highlight! Identifier guibg=bg guifg=fg gui=bold
highlight! LineNr guibg=bg guifg=#505050 gui=italic
highlight! MatchParen guibg=#acaca2 guifg=fg
highlight! NonText guibg=bg guifg=#ffffea
highlight! PreProc guibg=bg guifg=fg gui=bold
highlight! Special guibg=bg guifg=fg gui=bold
highlight! SpecialKey guibg=bg guifg=fg gui=bold
highlight! Statement guibg=bg guifg=fg gui=italic
highlight! StatusLine guibg=#e6ffff guifg=#080808 gui=NONE
highlight! Type guibg=bg guifg=fg gui=bold
highlight! VertSplit guibg=#e6ffff guifg=#e6ffff
highlight! Visual guibg=#eaed8d guifg=fg
highlight! WarningMsg guibg=#ffe6e6 guifg=fg gui=bold
highlight! WildMenu guibg=#080808 guifg=#e6ffff gui=NONE

highlight! link CursorLine Normal
highlight! link ErrorMsg WarningMsg
highlight! link IncSearch Search
highlight! link MoreMsg Comment
highlight! link Pmenu StatusLine
highlight! link PmenuSbar StatusLine
highlight! link PmenuSel Visual
highlight! link PmenuThumb WildMenu
highlight! link Question Comment
highlight! link Search Visual
highlight! link StatusLineNC StatusLine
highlight! link String Comment
highlight! link Title Directory
highlight! link Todo Search
highlight! link VisualNOS Visual
highlight! link vimFunction Identifier

let g:colors_name = "acmesque"
