##==============================
## PATHと環境変数
##==============================
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

fpath=(/opt/homebrew/share/zsh-completions /opt/homebrew/share/zsh/functions ${fpath})
fpath=(~/.zsh/completions ~/.zsh/functions ${fpath})

export RLWRAP_HOME='/Users/iriya/.rlwrap'
export BREAK_CHARS="\"#'(),;\`\\|!?[]{}"
export WORDCHARS="*?_-.[]~=&!#$%^(){}<>"
export HOMEBREW_EDITOR='vim'
export HOMEBREW_CASK_OPTS='--appdir=/Applications'
export PAGER='lv'
export BAT_PAGER='less'

##==============================
## autoload
##==============================
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

# 重複した PATH の削除
typeset -U path PATH
source $HOME/.zsh_common

##==============================
## Keybind
##==============================
bindkey -e     # emacs-like
bindkey '^r'   fh
bindkey '^xf'  cdf
bindkey '^xd'  fd
bindkey '^xk'  fkill
bindkey '^xp'  frepo
bindkey '^xgs' fshow
bindkey '^xs'  fssh

##==============================
## alias
##==============================
if [[ $(uname) = "Darwin" ]]; then
    alias ldd="echo ldd is not on OSX. use otool -L."
    alias strace="echo strace is not on OSX. use dtruss"
fi

eval `gdircolors -b $HOME/.dir_colors`
LS_OPTIONS='-v --show-control-chars -h --color=auto'
alias ls="gls $LS_OPTIONS"
alias ll="gls -l $LS_OPTIONS"
alias la="gls -A $LS_OPTIONS"
alias lla="gls -lA $LS_OPTIONS"
alias lal='lla'

alias grep='grep --color=auto'
alias df='df -h'
alias du='du -h'
alias pd=popd
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'

alias -g L="| lv"
alias -g G="| grep"
alias -g W='| wc'
alias -g H='| head'
alias -g T='| tail'
alias -g N='| nkf'
alias -g S='| sort'

alias -s ps=gv
alias -s eps=gv
alias -s dvi=dvipdfmx
alias -s tex=platex
alias -s pdf=gv

alias -g LC='|lv|cat'
alias -g bitread="cat <<EOF | gosh bitcode.scm | nkf -w"
alias lv='lv -z -la -Ou8 -c'
alias nv="nvim"
alias g="git"
alias gosh="rlwrap -b '(){}[],#;| ' gosh"
alias sbcl="rlwrap -b \$BREAK_CHARS sbcl"
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

##==============================
## Tools Setting
##==============================
# keychain
keychain ~/.ssh/id_dsa ~/.ssh/id_rsa
. $HOME/.keychain/$HOST-sh

# Terraform Completion
if type terraform &> /dev/null; then
    complete -o nospace -C terraform terraform
fi

# zsh-syntax-highlighting
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Added by OrbStack: command-line tools and integration
source ~/.orbstack/shell/init.zsh 2>/dev/null || :

# mise
eval "$(mise activate zsh)"
