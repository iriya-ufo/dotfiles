#!/bin/bash

# Cask app installed into /usr/local/Caskroom
brew cask install iterm2
brew cask install google-chrome
brew cask install dropbox
brew cask install google-japanese-ime
brew cask install gyazo
brew cask install slack
brew cask install thunderbird
brew cask install alfred
brew cask install google-photos-backup-and-sync
brew cask install insync
brew cask install discord
brew cask install docker
brew cask install cmd-eikana
brew cask install shiftit
brew cask install ngrok
brew cask install postman
brew cask install toggl
brew cask install visual-studio-code
brew cask install vlc
brew cask install dbeaver-community
brew cask install zoomus

# Java
brew cask install java
# If you want to use another Java Version
# $ export JAVA_HOME=`/usr/libexec/java_home -v 14`
# $ PATH=${JAVA_HOME}/bin:${PATH}

brew install ansible
brew install asciinema
brew install awscli
brew install awsebcli
brew install colordiff
brew install coreutils
brew install fzf
brew install gauche
brew install ghq
brew install git
brew install git-secrets
brew install grip
brew install httperf
brew install jq
brew install keychain
brew tap jesseduffield/lazydocker
brew install lazydocker
brew install lazygit
brew install lv
brew install nkf
brew install nmap
brew install nodenv
brew install pstree
brew install pyenv
brew install rbenv
brew install rbenv-gemset
brew install reattach-to-user-namespace
brew install rlwrap
brew install the_silver_searcher
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
GOPATH=$HOME/.go go get -u golang.org/x/lint/golint
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
