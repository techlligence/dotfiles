" Set the hybrid numbers
set number relativenumber

set termguicolors
syntax on

" Remap the escape key 
inoremap jj <Esc>

" To copy from vim to the system clipboard
set clipboard+=unnamedplus

call plug#begin()
Plug 'rafi/awesome-vim-colorschemes'
Plug 'tomasiser/vim-code-dark'
Plug 'lervag/vimtex'
Plug 'SirVer/ultisnips'
call plug#end()


" Setting up vimtex enviroment
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
set conceallevel=1
let g:tex_conceal='abdmg'

" Set the colorscheme and keep the backround transparent
colorscheme deus 
set cursorline
highlight CursorLine ctermbg=Yellow cterm=bold guibg=#2b2b2b 
hi Normal guibg=NONE ctermbg=NONE

let g:UltiSnipsSnippetDirectories=["~/.config/nvim/mysnippets"]
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<c-k>'

" To draw images
inoremap <C-f> <Esc>: silent exec '.!inkscape-figures create "'.getline('.').'" "'.b:vimtex.root.'/figures/"'<CR><CR>:w<CR>
nnoremap <C-f> : silent exec '!inkscape-figures edit "'.b:vimtex.root.'/figures/" > /dev/null 2>&1 &'<CR><CR>:redraw!<CR>
