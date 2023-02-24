export VISUAL=nvim
export EDITOR="$VISUAL"
export PATH="$PATH:$HOME/.local/bin:/sbin"

[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
[ -d "$HOME/.cargo" ] && source "$HOME/.cargo/env"
