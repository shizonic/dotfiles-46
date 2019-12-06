#!/bin/bash
ln -sf ~/repos/dotfiles/.* ~/
rm ~/.git
ln -sf ~/repos/dotfiles/* ~/
ln -sf ~/repos/dotfiles/.config/* ~/.config
ln -sf ~/repos/dotfiles/.gnupg/* ~/.gnupg
