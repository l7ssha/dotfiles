# Setup fzf
# ---------
if [[ ! "$PATH" == */home/l7ssha/.fzf/bin* ]]; then
  export PATH="$PATH:/home/l7ssha/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/l7ssha/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/l7ssha/.fzf/shell/key-bindings.zsh"

