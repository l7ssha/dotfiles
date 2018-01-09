# Created by newuser for 5.4.2
source /usr/share/zsh/share/antigen.zsh

antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle heroku
antigen bundle command-not-found
antigen bundle rails
antigen bundle ruby
antigen bundle archlinux
antigen bundle bundler

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions

# Load the theme.
antigen theme minimal

# Aliases
alias cls=clear
alias code-insiders=code

# Path
path+=('/opt/dotnet')
path+=('/opt/visual-studio-code-insiders/bin')

# Commands
cd ~

# Tell Antigen that you're done.
antigen apply
