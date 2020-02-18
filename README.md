# [mostly] About Emacs

Productivity Principles:

- Don't interrupt my flow.

If Emacs does something to interrupt flow, I turn it off. This does not apply only to Emacs. I use Emacs and Linux mainly because it allows me to configure my development environment in a way that keeps me in the flow as much as possible. This includes modeline garbage from erc or company-mode completion popups, they are all turned off. "Spell check" is also turned off because I studied English and typing in school, and do not appreciate the often unhelpful distractions of "Spell Check." Likewise "linting" is not always-on for the same reason. I don't want to be surprised by anything on my screen. This includes desktop notifications, cell-phone rings and social media beeps.

- Left Control and Left Alt are swapped everywhere.

Old keyboards from back in the day were actually designed with control closer to the space bar. With this simple swap, one can press Left Control with one's left thumb; likewise Right Alt may be pressed with the right thumb-- ensuring proper ergonomics whilst using modifiers (goodbye "Emacs pinky!") and rendering modal editing mostly unnecessary

- If Emacs loads in more than 1 second, it is bloated

AND/OR poorly configured...

- Use a modular Emacs configuration

Prefer lots of simple files as pluggable modules instead of monolithic, hard to navigate 1000 line init.el or worse .org file.

- Use fewer comments, except when necessary. Prefer self-documented code and systems.

If the code requires tons of comments it's probably crap. This includes so-called "literate programming," a bad idea from the start. In Emacs, C-h v and C-h f, etc. may be used to inspect what "this or that" function or variable may be doing.

- Use Emacs built-ins instead of packages when possible.

For example,

Use completion-at-point informed by an lsp server or just simple hippie-expand instead of company-mode.

Use flymake instead of flycheck.

Prefer xref-find-definitions instead of dumb-jump or something similar.

Prefer lsp (eglot) over non-lsp related packages, unless lsp support is poor for a particular language.

- Use simple Elisp and don't turn on stuff / change defaults if unsure about what it does.

- Always follow security best practices:

Basically follow this https://wiki.archlinux.org/index.php/Security guide, and aside from some other common sense practices such as, GPG keys and signatures. (Web of trust!) SSH keys. Passphrases and agents. Disk/directory/file/transfer encryption. Checksum checking. Keep systems up-to-date using the latest of upstream software packages (these updates contain security fixes and this approach is simpler than backporting and patching old versions of software.)

...

**Do source code audits. Don't just run scripts without viewing them. One reason I use KISS (https://k1ss.org) Linux is I am able to audit the entire system single-handedly. How many distros can you do this on? Likewise, using straight.el, we are able to audit Emacs packages as we have time, and stick to known safe (and stable) packages because of reproduceability. Say goodbye to the wild-west that is MELPA!**

When writing code, always sanity check input, variables, etc.

Use less software and services instead of more.

Monitor system behavior by keeping an eye on processes, services, logs, network traffic, iptables rules, filesystem permissions, etc. If neccessary inspect running processes using tools like gdb and strace. Doing this stuff is actually a lot of fun.

For obvious reasons, only use open source software.

## Replicate
```bash
git clone --recursive git@github.com:a-schaefers/dotfiles.git

mkdir -p ~/{bin,.emacs.d/straight/{versions,repos},.config/mpv}

cd ~/dotfiles && HOME/bin/stow HOME
```

![scrot](scrot1.png)

![scrot](scrot2.png)
