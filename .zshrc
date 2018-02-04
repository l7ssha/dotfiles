# Szymon uGlis 'l7ssha' .zshrc config file

source /usr/share/zsh/share/antigen.zsh

# Use oh-my-zsh lib
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
antigen bundle facetframer/zshnip

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

# Commands
cd ~

# fzf fuzzy finder
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

PATH="/home/l7ssha/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/l7ssha/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/l7ssha/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/l7ssha/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/l7ssha/perl5"; export PERL_MM_OPT;
