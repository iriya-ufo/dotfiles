export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=/sbin:/usr/sbin:/usr/local/sbin:$PATH
export PATH=/usr/local/stow/bin:$PATH
source $HOME/.zsh_common
alias ak='env ACCEPT_KEYWORDS=~x86'
export LANG=ja_JP.UTF-8
export EDITOR=vim
alias ssh='env TERM=xterm ssh'
alias -g LC='|lv|cat'
alias gosh="rlwrap -b '(){}[],#;| ' gosh"

bindkey -e

mcmd() {
    local cflags="-mcpu=7450 -O3 -pipe -fomit-frame-pointer -fsigned-char -maltivec"
    env CFLAGS="${cflags}" \
        CXXFLAGS="${cflags}" $@
        # PATH="/usr/lib/ccache/bin:$PATH" \
}

function cdup() {
    echo
    cd ..
    zle reset-prompt
}

zle -N cdup
bindkey '\^' cdup
