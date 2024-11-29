syntax on
filetype plugin indent on
set tabstop=4
set shiftwidth=4
set expandtab
set guifont=Iosevka\ 20
set guioptions-=m
set guioptions-=T
set noesckeys
set relativenumber
set number
set ignorecase
set smartcase
set incsearch
set cinoptions=l1
set modeline
set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
set autoindent
set autochdir
colorscheme habamax

" duplicate line with cursor position preservation 
nnoremap <C-,> :let current_col = col('.')<CR>yyp:call cursor('.', current_col)<CR>

autocmd BufEnter * if &filetype == "go" | setlocal noexpandtab
autocmd BufNewFile,BufRead ?\+.c3 setf c

map gf :e <cfile><CR>
