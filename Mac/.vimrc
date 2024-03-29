set nocompatible
filetype off
syntax on

set termguicolors
colorscheme desert

" 文字コードの設定
set encoding=utf8
set fileencodings=utf-8,ucs-bom,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,euc-jp,sjis,cp932
set termencoding=utf-8

" 検索文字列が小文字の場合は大文字小文字を区別なく検索する(noignorecase)
set ignorecase
" 検索文字列に大文字が含まれている場合は区別して検索する(nosmartcase)
set smartcase
" インクリメンタルサーチ
set incsearch
" n, N キーで「次の（前の）検索候補」を画面の中心に表示する
nnoremap n nzz
nnoremap N Nzz

" 全角スペースを視覚化
highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=darkgray
match ZenkakuSpace /　/

nmap <Esc><Esc> :nohlsearch<CR><Esc>
set number
set backspace=eol,start
set history=200
set wrapscan
set hlsearch
set wildmenu
set shiftwidth=2
set laststatus=2
set showmatch
set textwidth=0
set hidden
set noautoindent
set nosmartindent
compiler ruby
set expandtab
set tabstop=2 shiftwidth=2 softtabstop=2
filetype plugin on

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

if has("autocmd")
    " ファイルタイプ別インデント、プラグインを有効にする
    filetype plugin indent on
    " カーソル位置を記憶する
    autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif
endif

" Markdown モードの設定
if has("autocmd")
  augroup MyAutoCmd
    autocmd!
    " md 等の拡張子を Markdown と判断させるための設定
    autocmd BufNewFile,BufRead *.{md,mdwn,mkd,mkdn,mark*} set filetype=markdown
    " イタリックフォントを無効にする設定
    autocmd FileType markdown hi! def link markdownItalic Normal
  augroup END
endif

" tmux settings
if !has('gui_running') && &term =~ '^\%(screen\|tmux\)'
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
