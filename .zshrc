# Szymon Uglis 'l7ssha' .zshrc config file

source /usr/share/zsh/share/antigen.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Use oh-my-zsh lib
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle command-not-found
antigen bundle archlinux
antigen bundle bundler

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle facetframer/zshnip

antigen bundle arzzen/calc.plugin.zsh

# Load the theme.
antigen theme crunch

# Tell Antigen that you're done.
antigen apply

alias cls=clear

# Github
alias 'git commit'='git commit -m'
alias gc='git commit -m'

#ls
alias l="lsd --group-dirs first"
alias ls="lsd --group-dirs first"
alias la="lsd -la --group-dirs first"

alias cpwd="pwd | xclip -sel CLIP"

export EDITOR="nano"
export VISUAL="vscodium -r"

alias e="$EDITOR"
alias ee="$VISUAL"
