# PATH ENV
export PATH=/usr/local/bin:/usr/local/sbin:$PATH
# GOPATH
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

fpath=(~/.zsh/completions ~/.zsh/functions ${fpath})

# autoload completions
autoload -Uz cd-gitroot
autoload -Uz git-escape-magic
git-escape-magic

# autoload fzf functions
for widget_name in ~/.zsh/functions/*; do
  local function_name="${widget_name:t}"
  zle -N "${function_name}"
  autoload -Uz "${function_name}"
done

bindkey -e     # emacs-like
bindkey '^r'   fh
bindkey '^xf'  cdf
bindkey '^xd'  fd
bindkey '^xk'  fkill
bindkey '^xp'  frepo
bindkey '^xgs' fshow
bindkey '^xs'  fssh

# 重複した PATH の削除
typeset -U path PATH
source $HOME/.zsh_common

# 環境変数
export RLWRAP_HOME='/home/iriya/.rlwrap'
export BREAK_CHARS="\"#'(),;\`\\|!?[]{}"
export WORDCHARS="*?_-.[]~=&!#$%^(){}<>"

# Alias
alias -g LC='|lv|cat'
alias -g bitread="cat <<EOF | gosh bitcode.scm | nkf -w"
alias g="git"
alias gosh="rlwrap -b '(){}[],#;| ' gosh"
alias sbcl="rlwrap -b \$BREAK_CHARS sbcl"
alias ccl="rlwrap -b \$BREAK_CHARS /usr/local/ccl/dx86cl64"
alias diff='colordiff -u'
alias be='bundle exec'
alias man='env LANG=C man'
alias lg='lazygit'
alias fig='docker-compose'
alias dps='docker ps'
alias dpsa='docker ps -a'
alias dvls='docker volume ls'
alias dimg='docker images'
alias drmi='docker rmi'

# asdf
. $HOME/.asdf/asdf.sh
