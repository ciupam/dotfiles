export VISUAL=vim
export EDITOR="$VISUAL"
export PATH="$PATH:$HOME/.emacs.d/bin:$HOME/.local/bin:/sbin"
set -o vi

[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
[ -d "$HOME/.cargo" ] && . "$HOME/.cargo/env"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
