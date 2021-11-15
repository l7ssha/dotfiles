if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

USE_POWERLINE="true"
emulate -L zsh

# Determine terminal capabilities.
{
if ! zmodload zsh/langinfo zsh/terminfo ||
    [[ $langinfo[CODESET] != (utf|UTF)(-|)8 || $TERM == (dumb|linux) ]] ||
    (( terminfo[colors] < 256 )); then
    # Don't use the powerline config. It won't work on this terminal.
    local USE_POWERLINE=false
    # Define alias `x` if our parent process is `login`.
    local parent
    if { parent=$(</proc/$PPID/comm) } && [[ ${parent:t} == login ]]; then
    alias x='startx ~/.xinitrc'
    fi
fi
} 2>/dev/null

if [[ $USE_POWERLINE == false ]]; then
    # Use 8 colors and ASCII.
    source /usr/share/zsh/p10k-portable.zsh
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=black,bold'
else
    # Use 256 colors and UNICODE.
    source /usr/share/zsh/p10k.zsh
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=244'
fi

setopt extendedglob                                             # Extended globbing. Allows using regular expressions with *
setopt nocaseglob                                               # Case insensitive globbing
setopt numericglobsort                                          # Sort filenames numerically when it makes sense
setopt nobeep                                                   # No beep
setopt autocd                                                   # if only directory path is entered, cd there.

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'       # Case insensitive tab completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' rehash true                              # automatically find new executables in path 
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
HISTFILE=~/.zhistory
HISTSIZE=10000
SAVEHIST=10000

export PATH="$PATH":"$HOME/.pub-cache/bin"

export PROJECTS_DIR="/mnt/stuff/projects"
export GAMES_DIR="/mnt/games"
export EDITOR="nano"
export VISUAL="vscodium -r"

export ZSH_AUTOSUGGEST_USE_ASYNC=true

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
alias cp="cp -i"                                                # Confirm before overwriting something
alias df='df -h'                                                # Human-readable sizes
alias free='free -m'                                            # Show sizes in MB
alias gitu='git add . && git commit && git push'

# Theming section  
autoload -U compinit colors zcalc
# compinit -d
colors

source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

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

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
