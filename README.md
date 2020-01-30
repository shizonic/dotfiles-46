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

- 3. Copy non version controlled stow directories, (like ssh and gpg), to ~/dotfiles

NOTE: obviously this stuff is in .gitignore...

```bash
cp -r $USBSTICK/{ssh,gpg} ~/dotfiles
```

- 4. create symlink farm
cd ~/dotfiles
mkdir -p ~/{bin,.emacs.d/straight/versions,.config/{mpv,spm,dunst}}
stow bin bash config emacs gnupg misc ssh wallpaper
```

### But why not just use full disk, or even $HOME encryption?

1. Both Full disk encryption and $HOME disk encryption are less robust.

Example 1 - full disk encryption

If Full disk encryption has a problem, it might leave you with an unbootable system.

Example 2 - $HOME encryption

If PAM has a problem, (and PAM always has problems), it can lock you out of your $HOME directory.

### Solution:

Encrypt only files and directories that need to be encrypted.

### But what about Evil Maid attacks?

In most cases, for most people, a guard dog should suffice.
