set nocompatible
filetype off
syntax on

" 日本語の設定
set enc=utf8
set encoding=utf8
set fileencoding=utf-8
set fileencodings=utf-8
set termencoding=utf-8

" 検索文字列が小文字の場合は大文字小文字を区別なく検索する(noignorecase)
set ignorecase
" 検索文字列に大文字が含まれている場合は区別して検索する(nosmartcase)
set smartcase
" インクリメンタルサーチ
set incsearch

" 全角スペースを視覚化
highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=darkgray
match ZenkakuSpace /　/

set backspace=eol,start
set history=200
set wrapscan
set hlsearch
set wildmenu
set shiftwidth=2
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
