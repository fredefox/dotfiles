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

apt install \
    atool \
    autoconf \
    curl \
    curl \
    emacs \
    imagemagick \
    libgccjit-{10..14}-dev \
    libgif-dev \
    libgnutls28-dev \
    libgtk-3-dev \
    libmagickwand-dev \
    libncurses-dev \
    libtinfo6 \
    libxpm-dev \
    makeinfo \
    texi2html \
    texinfo \
    xclip \
    zsh

