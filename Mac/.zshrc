# PATH ENV
export PATH=/usr/local/bin:/usr/local/sbin:$PATH
# GOPATH
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

fpath=(/usr/local/share/zsh-completions /usr/local/share/zsh/functions ${fpath})
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
export RLWRAP_HOME='/Users/iriya/.rlwrap'
export BREAK_CHARS="\"#'(),;\`\\|!?[]{}"
export WORDCHARS="*?_-.[]~=&!#$%^(){}<>"
export HOMEBREW_EDITOR='vim'
export HOMEBREW_CASK_OPTS='--appdir=/Applications'
export BAT_PAGER='less'

# Docker for Mac
unset DOCKER_CERT_PATH
unset DOCKER_HOST
unset DOCKER_MACHINE_NAME
unset DOCKER_TLS_VERIFY

if [[ $(uname) = "Darwin" ]]; then
    alias ldd="echo ldd is not on OSX. use otool -L."
    alias strace="echo strace is not on OSX. use dtruss"
fi

alias -g LC='|lv|cat'
alias -g bitread="cat <<EOF | gosh bitcode.scm | nkf -w"
alias g="git"
alias gosh="rlwrap -b '(){}[],#;| ' gosh"
alias sbcl="rlwrap -b \$BREAK_CHARS sbcl"
alias ccl="rlwrap -b \$BREAK_CHARS /usr/local/ccl/dx86cl64"
alias updatedb='sudo /usr/libexec/locate.updatedb'
alias diff='colordiff -u'
alias dig='dig +noedns'
alias be='bundle exec'
alias man='env LANG=C man'
alias top='htop'
alias lg='lazygit'
alias fig='docker compose'
alias dps='docker ps'
alias dpsa='docker ps -a'
alias dvls='docker volume ls'
alias dimg='docker images'
alias drmi='docker rmi'

keychain ~/.ssh/id_dsa ~/.ssh/id_rsa
. $HOME/.keychain/$HOST-sh

# PostgreSQL
export PGDATA=/usr/local/var/postgres

# AWS CLI Completion
source /usr/local/share/zsh/site-functions/_aws

# Terraform Completion
if type terraform &> /dev/null; then
    complete -o nospace -C terraform terraform
fi

# zsh-syntax-highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# asdf
source /usr/local/opt/asdf/libexec/asdf.sh

# Added by OrbStack: command-line tools and integration
source ~/.orbstack/shell/init.zsh 2>/dev/null || :
