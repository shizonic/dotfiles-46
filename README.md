## Replicate
```bash
git clone --recursive git@github.com:a-schaefers/dotfiles.git

cp -rf $USBSTICK/{ssh,gpg} ~/dotfiles

mkdir -p ~/{bin,.emacs.d/straight/{versions,repos},.config/mpv}

cd ~/dotfiles && HOME/bin/stow HOME
```
