# Szymon Uglis 'l7ssha' .zshrc config file

source /usr/share/zsh/share/antigen.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle command-not-found
antigen bundle archlinux
antigen bundle bundler

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle facetframer/zshnip
antigen bundle arzzen/calc.plugin.zsh

antigen theme crunch
antigen apply

export PROJECTS_DIR="/mnt/stuff/projects"
export GAMES_DIR="/mnt/games"
export EDITOR="nano"
export VISUAL="vscodium -r"

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
alias free="free -m"

alias e="$EDITOR"
alias ee="$VISUAL"

cheat() {
    curl cheat.sh/$1
}

# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}
