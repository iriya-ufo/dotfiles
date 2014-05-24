source $HOME/.zsh_common
fpath=(~/.zsh/functions/Completion(N-/) /usr/local/share/zsh/functions ${fpath})

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

# gem function
function gem() {
   $HOME/.rbenv/shims/gem $*
   if [ "$1" = "install" ] || [ "$1" = "i" ] || [ "$1" = "uninstall" ] || [ "$1" = "uni" ]
   then
      rbenv rehash
      rehash
   fi
}

function cdup() {
    echo
    cd ..
    zle reset-prompt
}

zle -N cdup

# rbenv
eval "$(rbenv init -)"

# VCS Setting 
autoload -Uz vcs_info
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
RPROMPT="%1(v|%F{green}%1v%f|)"
