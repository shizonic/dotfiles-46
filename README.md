# [mostly] About Emacs

## Productivity Principles:

- Don't interrupt my flow. Flow is money.

If Emacs does something to interrupt flow, I turn it off. This does not apply only to Emacs. I use Emacs and Linux mainly because it allows me to configure my development environment in a way that keeps me in the flow as much as possible. This includes modeline garbage from erc or company-mode completion popups, they are all turned off. "Spell check" is also turned off because I studied English and typing in school, and do not appreciate the often unhelpful distractions of "Spell Check." Likewise "linting" is not always-on for the same reason. I don't want to be surprised by anything on my screen. This includes desktop notifications, cell-phone rings and social media beeps.

- Use Emacs as a Linux window manager

This is a great way to improve flow. Exwm is to be preferred for this reason. Now the brain is not required to separate Emacs from window managing, and switching back and forth from a browser to code feels more natural this way. Just don't fall into "ricing", the default Emacs "light" theme is actually perfect.

- Left Control and Left Alt are swapped.

Old keyboards from back in the day were actually designed with control closer to the space bar. With this simple swap, one can press Left Control with one's left thumb; likewise Right Alt may be pressed with the right thumb-- ensuring proper ergonomics whilst using modifiers (goodbye "Emacs pinky!") and rendering modal editing unnecessary.

On that note, a mechanical keyboard is a necessity and the best way to prevent RSI, not vim. We may be hackers, but first of all, we are typists. Click clack clack, rat a tat tat. I recommend cherry blues.

- If Emacs loads in more than 1 second, it is bloated.

AND/OR poorly configured...

- Use a modular Emacs configuration

Prefer lots of simple files as [un]pluggable modules instead of a monolithic, 1000 line init.el (or worse .org file.)

- Use fewer comments, except when necessary. Prefer self-documented code and systems.

If the code requires tons of comments it's probably crap. This includes so-called "literate programming," a bad idea from the start. In Emacs, C-h v and C-h f, etc. may be used to inspect what "this or that" function or variable may be doing.

- Use Emacs built-ins instead of packages whenever possible. And when deciding between packages that perform the same function, prefer the simpler and/or time-tested solution, to newer solutions or to solutions that have many dependencies.

For example,

Prefer ido-mode instead of ivy or helm. Not only is ido-mode built-in, and faster for most tasks, but it also only uses the minibuffer! Why do modern developers think it's a good idea to shift the entire screen and cover up code with popup menus when we don't have to? This is distracting and gets in the way of flow.

Prefer paredit-mode instead of lispy, (Much fewer dependencies...)

Use completion-at-point via TAB informed by an eglot lsp server or just simple hippie-expand via M-/ instead of company-mode.

Use flymake instead of flycheck.

Prefer xref-find-definitions informed by an lsp server instead of dumb-jump or something even worse, like CTAGS/GTAGS/GNUGLOBAL etc.

Basically just install eglot and relevant language servers if needed. Most of this (IDE crap) is crap anyways, and can get in the way of "flow." At the end of the day, Emacs will never be as good of an IDE as visual studio or even jetbrains. Use an IDE when you want an IDE. Use an Emacs when you want an Emacs. What's wrong with using "both" approaches?

- Use simple Elisp and don't turn on stuff / change defaults if unsure about what it does.

Defaults are a great way to stay in the flow. Instead of constantly tweaking my setup, I learn defaults, which are probably going to be well thought-out. Because we are staying as close to vanilla Emacs (on steroids) as possible, which means there are a million engineering hours invested in the product I am using, and I am not going to be reinventing the wheel every day.

- Always follow security best practices:

**Do source code audits. Don't just run scripts without viewing them. One reason I use KISS (https://k1ss.org) Linux is I am able to audit the entire system single-handedly. How many distros can you do this on? Likewise, using straight.el, we are able to audit Emacs packages as we have time, and stick to known safe (and stable) packages because of reproduceability. Say goodbye to the wild-west that is MELPA!**

## Replicate
```bash
git clone --recursive git@github.com:a-schaefers/dotfiles.git

mkdir -p ~/{bin,.emacs.d/straight/{versions,repos},.config/mpv}

cd ~/dotfiles && HOME/bin/stow HOME
```
