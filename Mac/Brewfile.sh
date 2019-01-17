#!/bin/bash
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
brew tap caskroom/cask
brew cask install iterm2
brew cask install google-chrome
brew cask install dropbox
brew cask install evernote
brew cask install google-japanese-ime
brew cask install gyazo
brew cask install slack
brew cask install thunderbird
brew cask install alfred
brew cask install google-photos-backup-and-sync
brew cask install insync
brew cask install docker
brew cask install cmd-eikana
brew cask install shiftit
brew cask install ngrok
brew cask install skype
brew cask install visual-studio-code

brew install ansible
brew install asciinema
brew install awscli
brew install awsebcli
brew install colordiff
brew install coreutils
brew install gauche
brew install git
brew install git-secrets
brew install grip
brew install httperf
brew install jq
brew install keychain
brew install lv
brew install nkf
brew install nmap
brew install pstree
brew install rbenv
brew install rbenv-gemset
brew install reattach-to-user-namespace
brew install rlwrap
brew install the_silver_searcher
brew install tig
brew install tmux
brew install wget
brew install zsh
brew install zsh-completions

# Go
brew install go
GOPATH=$HOME/.go go get github.com/rogpeppe/godef               # 関数定義等の参照パッケージ
GOPATH=$HOME/.go go get -u github.com/nsf/gocode                # 補完パッケージ
GOPATH=$HOME/.go go get -v github.com/uudashr/gopkgs/cmd/gopkgs # Go パッケージ
GOPATH=$HOME/.go go get github.com/golang/lint/golint           # flycheckでシンタックスエラーを検知
GOPATH=$HOME/.go go get github.com/kisielk/errcheck             # flycheckでシンタックスエラーを検知

# All clean up
brew cleanup
