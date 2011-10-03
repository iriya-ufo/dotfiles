source $HOME/.zsh_common
alias -g LC='|lv|cat'
alias tgif-ja='LANG=jp_JP.eucJP tgif &'
alias -g bitread="cat <<EOF | gosh bitcode.scm | nkf -w"
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
