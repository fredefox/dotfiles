#!/usr/bin/sh

set -x

DOTFILES=`pwd`

cd ${XDG_CONFIG_HOME:-$HOME/.config}

packages=`cat <<EOF
git
i3
urxvt
zsh
environment.d
EOF`

for package in $packages;
do
  ln -s $DOTFILES/$package .
done

sudo apt install emacs curl atool curl zsh xclip
