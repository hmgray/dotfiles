set background=dark
highlight clear

if exists("syntax_on")
	syntax reset
endif

let g:colors_name = "deeper-blue"

hi Normal       guifg=grey80         guibg=#181a26
hi Cursor       guifg=slategrey      guibg=khaki  
hi CursorLineNr ctermfg=14 guifg=Yellow
hi VertSplit    guifg=grey40         guibg=#c2bfa5 gui=none cterm=reverse
hi Folded       guifg=grey40         guibg=black ctermfg=grey ctermbg=darkgrey
hi FoldColumn   guifg=grey20         guibg=black ctermfg=4 ctermbg=7
hi IncSearch    guifg=green          guibg=black cterm=none ctermfg=yellow ctermbg=green
hi ModeMsg      guifg=goldenrod      cterm=none ctermfg=brown
hi MoreMsg      guifg=SeaGreen       ctermfg=darkgreen
hi NonText      guifg=RoyalBlue      guibg=#181a26 cterm=bold ctermfg=blue
hi Question     guifg=springgreen    ctermfg=green
hi Search       guifg=wheat          guibg=peru cterm=none ctermfg=grey ctermbg=blue
hi SpecialKey   guifg=yellowgreen    ctermfg=darkgreen
hi StatusLine   guifg=black          guibg=#c2bfa5 gui=none cterm=bold,reverse
hi StatusLineNC guifg=grey40         guibg=#c2bfa5 gui=none cterm=reverse
hi Title        guifg=gold           gui=bold cterm=bold ctermfg=yellow
hi Statement    guifg=CornflowerBlue ctermfg=lightblue
hi Visual       guifg=grey80        guibg=DodgerBlue4 gui=none cterm=reverse
hi WarningMsg   guifg=salmon         ctermfg=1
hi String       guifg=moccasin       ctermfg=darkcyan
hi Comment      guifg=grey50         term=bold ctermfg=11 
hi Constant     guifg=#ffa0a0        ctermfg=brown
hi Special      guifg=grey80         ctermfg=brown
hi Identifier   guifg=salmon         ctermfg=red
hi Include      guifg=red            ctermfg=red
hi PreProc      guifg=DeepSkyBlue1   ctermfg=LightGreen

hi Operator     guifg=Red            ctermfg=Red
hi Define       guifg=DeepSkyBlue1   gui=bold ctermfg=yellow
hi Type         guifg=CornflowerBlue ctermfg=2
hi Function     guifg=LightGreen     ctermfg=brown
hi Structure    guifg=green          ctermfg=green
hi LineNr       guifg=grey50         ctermfg=3
hi Ignore       guifg=grey40         cterm=bold ctermfg=7
hi Todo         guifg=orangered      guibg=yellow2
hi Directory    ctermfg=darkcyan
hi ErrorMsg     guifg=White          guibg=Red cterm=bold cterm=bold ctermfg=7 ctermbg=1
hi WildMenu     ctermfg=0            ctermbg=3
hi DiffAdd      ctermbg=4
hi DiffChange   ctermbg=5
hi DiffDelete   cterm=bold           ctermfg=4 ctermbg=6
hi DiffText     cterm=bold           ctermbg=1
hi Underlined   cterm=underline      ctermfg=5
hi Error        guifg=White          guibg=Red cterm=bold ctermfg=7 ctermbg=1
hi SpellErrors  guifg=White          guibg=Red cterm=bold ctermfg=7 ctermbg=1

" popup menu
hi Pmenu      guifg=#c0c0c0 guibg=#404080
hi PmenuSel   guifg=#c0c0c0 guibg=#2050d0
hi PmenuSbar  guifg=blue    guibg=darkgray
hi PmenuThumb guifg=#c0c0c0

highlight! link CursorLine Normal
