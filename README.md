![scrot1](screenshots/2020-01-26-203638_1366x768_scrot.png)

![scrot2](screenshots/2020-01-27-032512_1366x768_scrot.png)

# Install
```bash
# 1. encrypt ~/LOCKER directory using ext4 native encryption...
# (store misc. things that should be encrypted like a browser in here...)
mv ~/.config/chromium ~/LOCKER
ln -s ~/LOCKER/chromium ~/.config

# 2. mv my ssh and gpg stuff from usb stick to dotfiles (it's okay because of .gitignore...)
git clone git@github.com:a-schaefers/dotfiles.git
mv $USBSTICK/{ssh,gpg} ~/dotfiles

# 3. create symlink farm
cd ~/dotfiles
mkdir -p ~/{bin,.emacs.d/straight/versions,.config/{mpv,spm,dunst}}
stow bin bash config emacs gnupg misc ssh wallpaper
```
