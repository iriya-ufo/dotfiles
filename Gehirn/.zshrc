##############################
#           Prompt           #
##############################
autoload -Uz colors && colors
setopt prompt_subst
setopt transient_rprompt

autoload -Uz add-zsh-hook
autoload -Uz vcs_info
add-zsh-hook precmd vcs_info

zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:hg:*' get-revision true
zstyle ':vcs_info:hg:*' branchformat '%b'

zstyle ':vcs_info:*' formats '%s' '%b' '%c' '%u'
zstyle ':vcs_info:*' max-exports 4
zstyle ':vcs_info:*' stagedstr '+'
zstyle ':vcs_info:*' unstagedstr '-'

function vcs_prompt_info () {
    if [ -z $vcs_info_msg_0_ ]; then
        return 0
    fi

    echo -n "%{$fg[green]%}${vcs_info_msg_0_}%{$fg[yellow]%}:"
    echo -n "%{$fg[green]%}${vcs_info_msg_1_} "
    echo -n "%{$fg[yellow]%}${vcs_info_msg_2_}${vcs_info_msg_3_}%{$reset_color%} "
}

keymap_prompt_color='cyan'
PROMPT='$(vcs_prompt_info)%{$fg[$keymap_prompt_color]%}%c %(?:%{$fg_bold[green]%}:%{$fg_bold[red]%})$%{$reset_color%} '
PROMPT2='%{$fg_bold[magenta]%}%_ %%%{$reset_color%} '
SPROMPT='%{$fg_bold[red]%}%R%{$reset_color%}->%{$fg_bold[green]%}%r%{$reset_color%}? [%{$fg[green]%}y%{$reset_color%}, %{$fg[red]%}n%{$reset_color%}, %{$fg[yellow]%}e%{$reset_color%}, %{$fg[red]%}a%{$reset_color%}] '

# Change color of prompt on vi normal mode
# http://memo.officebrook.net/20090226.html
function zle-line-init zle-keymap-select {
    case $KEYMAP in
        vicmd)
            keymap_prompt_color='blue'
            ;;
        main|viins)
            keymap_prompt_color='cyan'
            ;;
    esac
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

##############################
#         Completion         #
##############################
autoload -U compinit && compinit

setopt correct  # Correcting spell miss
setopt list_packed  # show candidacy compactly
setopt nolistbeep  # show candidacy without beep
setopt auto_menu  # Automatically use menu completion after the second consecutive request for completion, for example by pressing the TAB key repeatedly. This option is overridden by MENU_COMPLETE.
setopt auto_param_keys  # If a parameter name was completed and the next character typed is one of those that have to come directly after the name (like }, :, etc.), they are placed there automatically.
setopt auto_param_slash  # if a parameter is completed whose content is the name of a directory, then add a trailing slash.
setopt auto_remove_slash  # When the last character resulting from a completion is a slash and the next character typed is a word delimiter, remove the slash.
setopt mark_dirs  # Append a trailing / to all directory names resulting from filename generation.
setopt list_types  # When listing files that are possible completions, show the type of each file with a trailing identifying mark.
setopt complete_in_word  # If unset, the cursor is moved to the end of the word if completion is started. Otherwise it stays where it is and completion is done from both ends.
setopt print_eight_bit
setopt magic_equal_subst  # All unquoted arguments of the form identifier=expression appearing after the command name have file expansion (that is, where expression has a leading `~' or `=') performed on expression as if it were a parameter assignment. The argument is not otherwise treated specially: in other words, it is subsequently treated as a single word, not as an assignment.
unsetopt menu_complete  # On an ambiguous completion, instead of listing possibilities or beeping, insert the first match immediately. Then when completion is requested again, remove the first match and insert the second match, etc. When there are no more matches, go back to the first one again. reverse-menu-complete may be used to loop through the list in the other direction. This option overrides AUTO_MENU.

autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

if [[ "${OSTYPE}" = linux* ]]; then
    zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}  # 補完候補を色付き表示
fi
zstyle ':completion:*:default' menu select=1 # 補完候補を←↓↑→で選択
zstyle ':completion:*' use-cache true  # cache candidacy of completion
zstyle ':completion:*:processes' command 'ps x'  # kill で 'ps x' のリストから選択可能
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([%0-9]#)*=0=01;31'  # kill の候補を色付き表示

##############################
#          History           #
##############################
HISTFILE=$HOME/.zsh_history  # file for saving history
SAVEHIST=10000  # count of history saved on HISTFILE
HISTSIZE=10000  # count of history saved on memory
setopt share_history  # share history among other terminal
setopt inc_append_history # history will be saved on HISTFILE incrementally
setopt hist_ignore_all_dups  # remove duplicate from history
setopt hist_ignore_dups  # if command matches latest history, it is not append to history
setopt hist_save_no_dups  # remove duplicate from HISTFILE
setopt hist_ignore_space  # if command starts with space, it is not append to history
setopt hist_reduce_blanks  # history will be stripped
setopt hist_no_store  # "history" command will not be recorded on history

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end  # select previous history with Ctrl-P
bindkey "^N" history-beginning-search-forward-end  # select next history with Ctrl-N

##############################
#      Moving directory      #
##############################
setopt auto_cd  # change directory without cd command
setopt auto_pushd  # pushd automatically
setopt pushd_ignore_dups  # remove duplicate from directory stack
setopt pushd_to_home  # pushd with no arguments == pushd $HOME
setopt pushd_silent  # no lines or chars will be printed when will execute pushd or popd

##############################
#          Aliases           #
##############################
alias where="command -v"
alias ls="ls --color"
alias la="ls -a"
alias lf="ls -F"
alias ll="ls -l -h"
alias lla="ls -al"

alias du="du -h"
alias df="df -h"
alias su="su -l"

##############################
#            LESS            #
##############################
export LESS='-R'
export LESSCHARSET=utf-8
if which src-hilite-lesspipe.sh > /dev/null 2>&1; then
    export LESSOPEN="| $(which src-hilite-lesspipe.sh) %s"
elif [ -x /usr/share/source-highlight/src-hilite-lesspipe.sh ]; then
    export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
elif which lesspipe > /dev/null 2>&1; then
    export LESSOPEN="| $(which lesspipe) %s"
    export LESSCLOSE="$(which lesspipe) %s %s"
fi

##############################
#           GOPATH           #
##############################
function gopath() {
    if [ -n "$_PATH_ORIG" ]; then
        export PATH=$_PATH_ORIG
    fi

    if [ -z $1 ]; then
        export GOPATH=`pwd`
    elif [ -d $1 ]; then
        export GOPATH=$1
    else
        return
    fi

    _PATH_ORIG=$PATH
    export PATH=$GOPATH/bin:$PATH
}

##############################
#         雑多な設定         #
##############################
setopt brace_ccl
setopt numeric_glob_sort
setopt path_dirs

setopt ignore_eof

setopt bsd_echo
setopt notify
setopt long_list_jobs

setopt multios
setopt short_loops
setopt always_last_prompt
setopt cdable_vars sh_word_split

REPORTTIME=3

setopt rm_star_wait
