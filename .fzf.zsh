# Setup fzf
# ---------
if [[ ! "$PATH" == */home/mlauter/.fzf/bin* ]]; then
  export PATH="$PATH:/home/mlauter/.fzf/bin"
fi

# Man path
# --------
if [[ ! "$MANPATH" == */home/mlauter/.fzf/man* && -d "/home/mlauter/.fzf/man" ]]; then
  export MANPATH="$MANPATH:/home/mlauter/.fzf/man"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/mlauter/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/mlauter/.fzf/shell/key-bindings.zsh"

