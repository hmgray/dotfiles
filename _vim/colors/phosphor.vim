set background=dark
highlight clear 

let g:colors_name = 'phosphor'
if exists('syntax_on')
  syntax reset
endif

hi Cursor	guifg=#226622 guibg=#55ff55
hi lCursor	guifg=#226622 guibg=#55ff55
hi CursorIM	guifg=#226622 guibg=#55ff55
hi CursorLineNr guifg=#55ff55
hi Directory	guifg=#55ff55 guibg=#000000
hi DiffAdd	guifg=#55ff55 guibg=#226622 gui=none
hi DiffChange	guifg=#55ff55 guibg=#226622 gui=none
hi DiffDelete	guifg=#113311 guibg=#113311 gui=none
hi DiffText	guifg=#55ff55 guibg=#339933 gui=bold
hi ErrorMsg	guifg=#55ff55 guibg=#339933
" hi VertSplit	guifg=#339933 guibg=#339933
hi VertSplit	guifg=#113311 guibg=#113311
hi Folded	guifg=#44cc44 guibg=#113311
hi FoldColumn	guifg=#44cc44 guibg=#226622
hi IncSearch	guifg=#226622 guibg=#55ff55 gui=none
hi LineNr	guifg=#44cc44 guibg=#000000
hi MatchParen guifg=#44cc44 guibg=#113311
hi ModeMsg	guifg=#113311 guibg=#000000
hi MoreMsg	guifg=#44cc44 guibg=#000000
hi NonText	guifg=#113311 guibg=#000000
hi Normal	guifg=#44cc44 guibg=#000000
hi Question	guifg=#44cc44 guibg=#000000
hi Search	guifg=#113311 guibg=#44cc44 gui=none
hi SpecialKey	guifg=#44cc44 guibg=#000000
hi StatusLine	guifg=#113311 guibg=#000000 gui=none
hi StatusLineNC	guifg=#113311 guibg=#000000 gui=none
hi Title	guifg=#55ff55 guibg=#113311 gui=bold
hi Visual	guifg=#55ff55 guibg=#226622 gui=none
hi VisualNOS	guifg=#44cc44 guibg=#000000
hi WarningMsg	guifg=#55ff55 guibg=#000000
hi WildMenu	guifg=#226622 guibg=#55ff55

hi Comment	guifg=#226622 guibg=#000000
hi Constant	guifg=#55ff55 guibg=#226622
hi Special	guifg=#44cc44 guibg=#226622
hi Identifier	guifg=#55ff55 guibg=#000000
hi Statement	guifg=#55ff55 guibg=#000000 gui=bold
hi PreProc	guifg=#339933 guibg=#000000
hi Type		guifg=#55ff55 guibg=#000000 gui=bold
hi Underlined	guifg=#55ff55 guibg=#000000 gui=underline
hi Error	guifg=#55ff55 guibg=#339933
hi Todo		guifg=#113311 guibg=#44cc44 gui=none

hi! link CursorLine Normal
hi! link LineNr StatusLine
hi! link Pmenu StatusLine
hi! link PmenuSbar StatusLine
hi! link PmenuSel Visual
hi! link PmenuThumb WildMenu

let g:colors_name = "phosphor"
