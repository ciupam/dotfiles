export VISUAL=nvim
export EDITOR="$VISUAL"
export PATH="$PATH:$HOME/.local/bin:/sbin"
# Appends history prompts in tmux with bash & multiple panes setup
export PROMPT_COMMAND="history -a; history -n"

[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
[ -d "$HOME/.cargo" ] && source "$HOME/.cargo/env"
