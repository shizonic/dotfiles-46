# Dotfiles

![scrot1](screenshots/2020-01-26-203638_1366x768_scrot.png)

![scrot2](screenshots/2020-01-27-032512_1366x768_scrot.png)

## Replicate
```bash
fscrypt encrypt ~/LOCKER
mv ~/.config/chromium ~/LOCKER
ln -s ~/LOCKER/chromium ~/.config

git clone --recursive git@github.com:a-schaefers/dotfiles.git
cp -r $USBSTICK/{ssh,gpg} ~/dotfiles

~/dotfiles/bin/restow

cd ~/dotfiles/suckless-desktop/dwm/ && ./build
cd ~/dotfiles/suckless-desktop/dmenu && ./build
cd ~/dotfiles/suckless-desktop/st && ./build

build-emacs.sh
```
