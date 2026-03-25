##==============================
## Homebrew
##==============================
eval "$(/opt/homebrew/bin/brew shellenv)"

##==============================
## PATH と環境変数
## ログイン時に一度だけ設定する
##==============================
export PATH=$HOME/.local/bin:$PATH

export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

export RLWRAP_HOME='/Users/iriya/.rlwrap'
export BREAK_CHARS="\"#'(),;\`\\|!?[]{}"
export WORDCHARS="*?_-.[]~=&!#$%^(){}<>"
export HOMEBREW_EDITOR='vim'
export HOMEBREW_CASK_OPTS='--appdir=/Applications'
export PAGER='lv'
export BAT_PAGER='less'

##==============================
## SSH エージェント (keychain)
## ログイン時に一度だけ起動する
##==============================
keychain ~/.ssh/id_rsa
. $HOME/.keychain/$HOST-sh

##==============================
## tmux 自動起動
## tmux の外にいるときだけ実行（ログインシェル起動時のみ）
##==============================
if [ -z "$TMUX" ]; then
  tmux attach -t default || tmux new -s default
fi
