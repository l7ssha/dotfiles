# Szymon Uglis 'l7ssha' .zshrc config file

source /usr/share/zsh/share/antigen.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Use oh-my-zsh lib
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git:
antigen bundle heroku
antigen bundle command-not-found
antigen bundle archlinux
antigen bundle bundler

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle facetframer/zshnip

antigen bundle arzzen/calc.plugin.zsh

# Load the theme.
antigen theme agnoster

# Tell Antigen that you're done.
antigen apply

export GOPATH=$HOME/go
path+=('/opt/dart-sdk-dev/bin')
path+=('/home/l7ssha/go/bin')
path+=('/home/l7ssha/.flutter/bin')

export MPD_HOST=127.0.0.1
export MPD_PORT=1337
export JAVA_HOME="/usr/lib/jvm/java-10-openjdk"
export IDEA_JDK="/usr/lib/jvm/java-10-openjdk"
export JAVA_JDK="/usr/lib/jvm/java-10-openjdk"

path+=('/home/l7ssha/.pub-cache/bin')

alias cls=clear
alias code=code-insiders
alias npm='sudo npm --unsafe-perm'
alias dotnet='sudo dotnet'

alias screenshoot='maim -s | xclip -selection clipboard -t image/png'

# Package managers
alias pacman='sudo pacman'
#alias yaourt='yaourt --noconfirm'

# Github
alias 'git commit'='git commit -m'
alias gc='git commit -m'

# TMUX
tmux attach
