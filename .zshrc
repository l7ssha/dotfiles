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

# Load the theme.
antigen theme minimal

# Tell Antigen that you're done.
antigen apply

# Autoload
setopt EXTENDED_GLOB
for file in ~/.zsh/*.zsh; do
    if [[ -e $file ]]; then
        . $file
    fi
done

# Tmux auto attach
if [[ "$TERM" != "screen" ]] &&
        [[ "$SSH_CONNECTION" == "" ]]; then
    # Attempt to discover a detached session and attach
    # it, else create a new session

    WHOAMI=$(whoami)
    if tmux has-session -t $WHOAMI 2>/dev/null; then
        tmux -2 attach-session -t $WHOAMI
    else
        tmux -2 new-session -s $WHOAMI
    fi
else
    MOTD=/etc/motd.tcl
    if [ -f $MOTD ]; then
        $MOTD
    fi
fi

# Commands
cd ~

# fzf fuzzy finder
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

PATH="/home/l7ssha/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/l7ssha/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/l7ssha/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/l7ssha/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/l7ssha/perl5"; export PERL_MM_OPT;
