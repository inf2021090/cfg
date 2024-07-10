call plug#begin()


"icons and colorsheme-gruvebox
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ryanoasis/vim-devicons'

" nerd tree
Plug 'preservim/nerdtree'

" markdown support
Plug 'plasticboy/vim-markdown'

"vimwiki
Plug 'vimwiki/vimwiki'

" yuck
Plug 'elkowar/yuck.vim'

" latex support
Plug 'lervag/vimtex'
Plug 'lervag/vimtex', { 'tag': 'v2.15' }

Plug 'ap/vim-css-color' " CSS Color Preview
Plug 'ryanoasis/vim-devicons' " Developer Icons
Plug 'chrisbra/Colorizer' " hex color view 
call plug#end()

set nocompatible
set wrap
set number
set relativenumber

set autoindent
set tabstop=4
set shiftwidth=4
set smarttab
set softtabstop=4
set mouse=a

filetype plugin on
syntax on

" gruvbox
syntax enable
set background=dark
colorscheme gruvbox

" spelling
"set spelllang=en_us
"set spell

" airline
let g:airline_theme='gruvbox'
let g:airline#extensions#tabline#enabled = 1

" pdf viewer
let g:vimtex_view_method = 'zathura'

" latex compiler 
let g:vimtex_compiler_method = 'latexmk'

" vimwiki path
let g:vimwiki_list = [{'path': '~/Documents/vimwiki/'}]

" Set up nerdtree
let g:NERDTreeWinPos = "left"
let g:NERDTreeShowHidden = 1
let g:NERDTreeQuitOnOpen = 1
let g:NERDTreeIgnore = ['\.aux$', '\.out$', '\.log$']

""" KEY MAPPINGS 
let mapleader = " "

let maplocalleader = "\\"

" Toggle NERDTree with <Leader>e
nnoremap <Leader>e :NERDTreeToggle<CR>

" Example VimWiki mappings
nnoremap <Leader>ww :VimwikiIndex<CR>
