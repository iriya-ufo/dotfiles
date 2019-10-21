export PATH=/usr/local/bin:/usr/local/sbin:$PATH
export GOPATH=$HOME/project/go
export PATH=$PATH:$GOPATH/bin

bindkey -e     # emacs-like

# 重複した PATH の削除
typeset -U path PATH
source $HOME/.zsh_common

# 環境変数
export RLWRAP_HOME='/Users/iriya/.rlwrap'
export BREAK_CHARS="\"#'(),;\`\\|!?[]{}"
export WORDCHARS="*?_-.[]~=&!#$%^(){}<>"

alias -g LC='|lv|cat'
alias gosh="rlwrap -b '(){}[],#;| ' gosh"
alias -g bitread="cat <<EOF | gosh bitcode.scm | nkf -w"
alias sbcl="rlwrap -b \$BREAK_CHARS sbcl"
alias ccl="rlwrap -b \$BREAK_CHARS /usr/local/ccl/dx86cl64"
alias diff='colordiff -u'
alias be='bundle exec'
alias man='env LANG=C man'
alias fig='docker-compose'

# gem function
function gem() {
   $HOME/.rbenv/shims/gem $*
   if [ "$1" = "install" ] || [ "$1" = "i" ] || [ "$1" = "uninstall" ] || [ "$1" = "uni" ]
   then
      rbenv rehash
      rehash
   fi
}

# rbenv
eval "$(rbenv init -)"
