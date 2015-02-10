export PATH=/usr/local/bin:/usr/local/sbin:$PATH
export PATH=/usr/local/texlive/2014/bin/x86_64-darwin:/usr/texbin:$PATH
fpath=(~/.zsh/functions/Completion(N-/) /usr/local/share/zsh-completions /usr/local/share/zsh/functions ${fpath})
# 重複した PATH の削除
typeset -U path PATH
source $HOME/.zsh_common

# 環境変数
export RLWRAP_HOME='/Users/iriya/.rlwrap'

autoload -Uz cd-gitroot

if [[ $(uname) = "Darwin" ]]; then
   alias ldd="echo ldd is not on OSX. use otool -L."
   alias strace="echo strace is not on OSX. use dtruss"
fi

alias -g LC='|lv|cat'
alias -g bitread="cat <<EOF | gosh bitcode.scm | nkf -w"
alias gosh="rlwrap -b '(){}[],#;| ' gosh"
alias updatedb='sudo /usr/libexec/locate.updatedb'
alias diff='colordiff -u'
alias swift='xcrun /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/swift'
alias be='bundle exec'
alias man='env LANG=C man'
alias jsc='/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc'
alias top='htop'
alias sbcl='rlwrap sbcl'
alias ccl='rlwrap /usr/local/ccl/dx86cl64'

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

# VCS Setting 
autoload -Uz vcs_info
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
RPROMPT="%1(v|%F{green}%1v%f|)"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
