export LANG=en_US.UTF-8
export LANGUAGE=$LANG
export LC_ALL=$LANG

# Mercurial
export HGENCODING='utf-8'

# LS_COLORS
eval `dircolors ~/.dir_colors`

#cpanminus perl5
export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:$HOME/perl5"
export PERL_MB_OPT="--install_base $HOME/perl5"
export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"
export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB"
export PATH="$HOME/perl5/bin:$PATH"

#nodejs
export PATH="$HOME/.npm/bin:$PATH"
export NODE_PATH=`npm root -g`
