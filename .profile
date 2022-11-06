export VISUAL=nvim
export EDITOR="$VISUAL"
export PATH="$PATH:$HOME/.local/bin"

# HiDPI scaling
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5
export QT_AUTO_SCREEN_SET_FACTOR=0
export QT_SCALE_FACTOR=2
export QT_FONT_DPI=96

[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    # startx dwm -- vt1 &
    xinit -- :1
fi

[ -d "$HOME/.cargo" ] && source "$HOME/.cargo/env"
