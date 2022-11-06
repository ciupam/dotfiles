[ -f ~/.Xresources ] && xrdb -merge -I$HOME ~/.Xresources
#xrandr --output Virtual-1 --scale 1.5x1.5 --panning 3840x2400

exec dwm
