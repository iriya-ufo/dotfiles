" -------------------
" 色の設定
" -------------------
syntax on

" 全角スペースを視覚化
highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=white
match ZenkakuSpace /　/

" タブ幅
set ts=4 sw=3
set softtabstop=3
set expandtab

" -------------------
" 日本語の設定
" -------------------
set termencoding=utf-8
set encoding=utf-8
set fileencodings=iso-2022-jp,utf-8,cp932,euc-jp

" -------------------
" 検索
" -------------------
" 検索文字列が小文字の場合は大文字小文字を区別なく検索する(noignorecase)
set ignorecase
" 検索文字列に大文字が含まれている場合は区別して検索する(nosmartcase)
set smartcase
" インクリメンタルサーチ
set incsearch

" -------------------
" バッファ関連
" -------------------
set hidden           " 切り替え時のundoの効果持続等

" -------------------
" その他
" -------------------
set scrolloff=5 " スクロール時の余白確保
set showmatch
set history=50
set list
set listchars=tab:\ \ ,extends:<,trail:\ 
set laststatus=2
set directory=/tmp
set wildmode=full:list
set statusline=[%L]\ %t\ %y%{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}%r%m%=%c:%l/%L
:set fenc=utf-8
:set fencs=iso-2022-jp,euc-jp,cp932,utf-8
:set enc=utf-8
