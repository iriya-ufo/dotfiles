export PATH=/usr/local/bin:/usr/local/sbin:$PATH
fpath=(~/.zsh/functions/Completion(N-/) /usr/local/share/zsh-completions /usr/local/share/zsh/functions ${fpath})

# 重複した PATH の削除
typeset -U path PATH
source $HOME/.zsh_common

# 環境変数
export RLWRAP_HOME='/Users/iriya/.rlwrap'
export BREAK_CHARS="\"#'(),;\`\\|!?[]{}"
export HOMEBREW_EDITOR='vim'
export HOMEBREW_CASK_OPTS='--appdir=/Applications'

# Docker for Mac
unset DOCKER_CERT_PATH
unset DOCKER_HOST
unset DOCKER_MACHINE_NAME
unset DOCKER_TLS_VERIFY

autoload -Uz cd-gitroot

alias -g LC='|lv|cat'
alias gosh="rlwrap -b '(){}[],#;| ' gosh"
alias -g bitread="cat <<EOF | gosh bitcode.scm | nkf -w"
alias updatedb='sudo /usr/libexec/locate.updatedb'
alias diff='colordiff -u'
alias be='bundle exec'
alias man='env LANG=C man'
alias fig='docker-compose'

keychain ~/.ssh/id_dsa ~/.ssh/id_rsa
. $HOME/.keychain/$HOST-sh

bindkey -e

mcmd() {
    local cflags="-mcpu=7450 -O3 -pipe -fomit-frame-pointer -fsigned-char -maltivec"
    env CFLAGS="${cflags}" \
        CXXFLAGS="${cflags}" $@
        # PATH="/usr/lib/ccache/bin:$PATH" \
}

# git-escape-magic (cf. https://github.com/knu/zsh-git-escape-magic)
autoload -Uz git-escape-magic
git-escape-magic

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

# PostgreSQL
export PGDATA=/usr/local/var/postgres

# zsh history search by percol
function exists { which $1 &> /dev/null }
if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(fc -l -n 1 | eval $tac | percol --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }

    zle -N percol_select_history
    bindkey '^R' percol_select_history
fi
