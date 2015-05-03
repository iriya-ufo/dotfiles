filetype plugin indent on

" Encodings
set enc=utf-8
set fenc=utf-8
set fencs=utf-8,euc-jp,sjis,cp932,iso-2022-jp
set fileformats=unix,mac,dos

" Buffer
set hidden

" Backup
set backup
set backupdir=$HOME/.vim/backup

" Swap
set swapfile
set directory=$HOME/.vim/swap

" Undo
set undofile
set undodir=$HOME/.vim/undo

" Search
set noignorecase
set nosmartcase
set incsearch
set hlsearch
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz
cnoremap <expr> / getcmdtype() == '/' ? '\/' : '/'
cnoremap <expr> ? getcmdtype() == '?' ? '\?' : '?'
nmap <Esc><Esc> :noh<CR>

" Window
colorscheme desert
set number
set nowrap
set list
set listchars=tab:»-,trail:-,extends:»,precedes:«,eol:$,nbsp:%
set mouse=
set completeopt-=preview

" Matching braces
set showmatch
set matchtime=3
set matchpairs& matchpairs+=<:>

" Omni completion
hi Pmenu ctermbg=blue
hi PmenuSel term=bold ctermfg=white ctermbg=darkred
hi PMenuSbar ctermbg=blue

" Status line
set laststatus=2
set statusline=%<%f\%m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P

" Cursorline
augroup vimrc-auto-cursorline
    autocmd!
    autocmd CursorMoved,CursorMovedI,WinLeave * setlocal nocursorline
    autocmd CursorHold,CursorMovedI * setlocal cursorline
augroup END

" Indent
set autoindent
set smartindent
set smarttab
set expandtab
set softtabstop=0
set tabstop=4
set shiftwidth=4
set shiftround

" Go
set rtp+=$GOROOT/misc/vim
set completeopt=menu,preview

" Markdown
autocmd BufNewFile,BufRead *.{md,mdwn,mkd,mkdn,mark*} set filetype=markdown
let g:markdown_fenced_languages = [
    \  'css',
    \  'go',
    \  'html',
    \  'javascript',
    \  'js=javascript',
    \  'json=javascript',
    \  'python',
    \  'ruby',
    \  'sass',
    \  'sh',
    \  'xml',
    \]

syntax enable
