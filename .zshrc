# Szymon Uglis 'l7ssha' .zshrc config file

source /usr/share/zsh/share/antigen.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Use oh-my-zsh lib
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git:
antigen bundle heroku
antigen bundle command-not-found
antigen bundle rails
antigen bundle ruby
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

# Autoload
setopt EXTENDED_GLOB
for file in ~/.zsh/*.zsh; do
    if [[ -e $file ]]; then
        . $file
    fi
done

# Commands disabled for now, becouse of low computer
# neofetch
#LC_CTYPE=en_US.UTF-8
#LC_ALL=en_US.UTF-8
