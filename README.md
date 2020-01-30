![scrot1](screenshots/2020-01-26-203638_1366x768_scrot.png)

![scrot2](screenshots/2020-01-27-032512_1366x768_scrot.png)

# Dotfiles

## Rationale :: Quickly replicate my $HOME on a new machine...

- 1. encrypt ~/LOCKER directory using ext4 native encryption...

```bash
fscrypt encrypt ~/LOCKER
# Store misc. stuff that should be encrypted (like browser files) in here...
mv ~/.config/chromium ~/LOCKER
ln -s ~/LOCKER/chromium ~/.config
```

- 2. Clone dotfiles

```bash
git clone git@github.com:a-schaefers/dotfiles.git
```

- 3. cp my ssh and gpg stow directories from a usb stick to dotfiles

NOTE: This is  only safe because of .gitignore and of course these files are protected by passphrases and permissions...

```bash
cp -r $USBSTICK/{ssh,gpg} ~/dotfiles
```

- 4. create symlink farm
cd ~/dotfiles
mkdir -p ~/{bin,.emacs.d/straight/versions,.config/{mpv,spm,dunst}}
stow bin bash config emacs gnupg misc ssh wallpaper
```
