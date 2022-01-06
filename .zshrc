source /usr/share/zsh/share/antigen.zsh

REPORTTIME=3
HISTFILE=~/.zhistory
HISTSIZE=5000
SAVEHIST=5000

export PATH="$PATH":"$HOME/.pub-cache/bin"

export PROJECTS_DIR="/mnt/stuff/projects"
export GAMES_DIR="/mnt/games"
export EDITOR="nano"
export VISUAL="code -r"

alias cls="clear"
alias 'git commit'='git commit -m'
alias gc='git commit -m'
alias ls="lsd --group-dirs first"
alias l="ls"
alias la="lsd -la --group-dirs first"
alias cpwd="pwd | xclip -sel CLIP"
alias cdp="cd $PROJECTS_DIR"
alias mkd="mkdir -pv"
alias md="mkd"
alias df="df -h"
alias free="free -mh"

alias e="$EDITOR"
alias ee="$VISUAL"
alias cp="cp -i"
alias df='df -h'
alias free='free -m'
alias gitu='git add . && git commit && git push'

antigen use oh-my-zsh

antigen bundle git
antigen bundle archlinux
antigen bundle vscode
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-docker

# Load the theme
antigen theme aussiegeek

antigen apply