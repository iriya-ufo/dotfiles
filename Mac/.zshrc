source $HOME/.zsh_common
export PATH=/usr/local/bin:/usr/texbin:$PATH
fpath=(~/.zsh/functions/Completion /usr/local/share/zsh/functions ${fpath})

alias -g LC='|lv|cat'
alias -g bitread="cat <<EOF | gosh bitcode.scm | nkf -w"
alias gosh="rlwrap -b '(){}[],#;| ' gosh"

keychain ~/.ssh/id_dsa
. $HOME/.keychain/$HOST-sh

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

# rbenv
eval "$(rbenv init -)"
