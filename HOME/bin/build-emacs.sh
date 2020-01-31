#!/bin/sh -e

cd ~/dotfiles/emacs

make clean

git checkout emacs-27

git pull --rebase

autoreconf

./configure \
    --prefix=$HOME/.local \
    --with-modules \
    --without-all \
    --without-x \
    --with-x-toolkit=no \
    --with-x=no \
    --with-gnutls=yes

make
make install
