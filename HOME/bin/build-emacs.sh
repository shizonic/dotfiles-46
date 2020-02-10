#!/bin/sh -e

cd ~/dotfiles/emacs

make clean

git checkout emacs-27

git pull --rebase

autoreconf

./configure \
    --prefix=/usr \
    --with-modules \
    --with-xft \
    --with-x-toolkit=athena \
    --without-toolkit-scroll-bars \
    --without-dbus \
    --without-gconf \
    --without-gsettings \
    --with-xpm=no \
    --with-gnutls=yes \
    --with-json=yes

make
make install
