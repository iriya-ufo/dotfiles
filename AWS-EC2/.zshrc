source $HOME/.zsh_common
fpath=(~/.zsh/functions/Completion(N-/) ${fpath})

alias -g LC='|lv|cat'

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

function cdup() {
    echo
    cd ..
    zle reset-prompt
}

zle -N cdup

# VCS Setting 
autoload -Uz vcs_info
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
RPROMPT="%1(v|%F{green}%1v%f|)"
