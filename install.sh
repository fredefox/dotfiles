#!/usr/bin/bash
# Untested!!

ln -s ~/.emacs.d ~/.conf/emacs

tee ~/.xinitrc << EOF
#!/usr/bin/env bash

setxkbmap -option grp:win_space_toggle -layout us,dk
xrdb -merge ~/.Xresources
EOF

tee ~/.Xresources << EOF
#include ".config/Xresources/general"
#include ".config/Xresources/urxvt"
#include ".config/Xresources/theme/monokai-dark
EOF
