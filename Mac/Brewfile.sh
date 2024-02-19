#!/bin/bash

# Cask app installed into /usr/local/Caskroom
brew install --cask alacritty
brew install --cask alfred
brew install --cask authy
brew install --cask dbeaver-community
brew install --cask discord
brew install --cask dropbox
brew install --cask google-chrome
brew install --cask insomnia
brew install --cask karabiner-elements
brew install --cask ngrok
brew install --cask notion
brew install --cask slack
brew install --cask toggl
brew install --cask visual-studio-code
brew install --cask vivaldi
brew install --cask vlc
brew install --cask zoom

# Java
brew install --cask java
# If you want to use another Java Version
# $ export JAVA_HOME=`/usr/libexec/java_home -v 14`
# $ PATH=${JAVA_HOME}/bin:${PATH}

brew install ansible
brew install asciinema
brew install asdf
brew install awscli
brew install awsebcli
brew install bat
brew install colordiff
brew install coreutils
brew install fzf
brew install gauche
brew install gh
brew install ghq
brew install git
brew install git-secrets
brew install grip
brew install httperf
brew install jq
brew install keychain
brew install lazygit
brew install lv
brew install nkf
brew install nmap
brew install orbstack
brew install pstree
brew install reattach-to-user-namespace
brew install ripgrep
brew install rlwrap
brew install tig
brew install tmux
brew install tree
brew install wget
brew install zsh
brew install zsh-completions
brew install zsh-syntax-highlighting

# Go
brew install go
GOPATH=$HOME/.go go get -u github.com/motemen/gore/cmd/gore
GOPATH=$HOME/.go go get github.com/uudashr/gopkgs/cmd/gopkgs@latest
GOPATH=$HOME/.go go get -u github.com/kisielk/errcheck
GOPATH=$HOME/.go go get golang.org/x/tools/gopls@latest
GOPATH=$HOME/.go go get -u github.com/spf13/cobra/cobra
GOPATH=$HOME/.go go get github.com/mdempsky/gocode
GOPATH=$HOME/.go go get github.com/ramya-rao-a/go-outline
GOPATH=$HOME/.go go get github.com/stamblerre/gocode
GOPATH=$HOME/.go go get github.com/rogpeppe/godef
GOPATH=$HOME/.go go get github.com/sqs/goreturns

brew install hugo

# All clean up
brew cleanup
