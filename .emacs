;;; -*- lexical-binding: t; -*-

;; first things
(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      my-contacts-file "~/contacts.el"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen t
      custom-file "/dev/null"
      my-dotfiles-dir "~/repos/dotfiles"
      my-lisp-files "lisp.d"
      gc-cons-threshold 100000000
      debug-on-error t)

;; startup to eshell *only*
(add-hook 'after-init-hook '(lambda()
                              (kill-buffer "*scratch*")
                              (eshell)
                              (server-start)))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; install/require libs and pkgs
(require 'cl-lib)                               ;Common Lisp extensions
(require 'seq)                                  ;Sequence manipulation functions
(require 'subr-x)                               ;Extra Lisp functions
(straight-use-package 'dash)                    ;A modern list library
(straight-use-package 'a)                       ;Associative data structure functions
(straight-use-package 's)                       ;String manipulation library
(straight-use-package 'f)                       ;Modern API for working with files and directories
(require 'f)
(straight-use-package 'ht)                      ;The missing h ash table library
(straight-use-package 'crux)
(straight-use-package 'browse-kill-ring)
(straight-use-package 'keychain-environment)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'aggressive-indent)
(straight-use-package 'lispy)
(straight-use-package 'elisp-slime-nav)
(straight-use-package 'slime)
(require 'org)
(require 'erc)
(require 'ielm)
(require 'dired-x)
(require 'tramp)

;; misc
(global-prettify-symbols-mode 1)
(menu-bar-mode -1)

;; disable old theme before enabling a new theme
(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; modeline

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(setq-default mode-line-format
              '((:eval (simple-mode-line-render
                        ;; left
                        (format-mode-line "%* %b %l:%c %m")
                        ;; right
                        (format-mode-line (concat
                                           (format-time-string " %I:%M%p")))))))

(show-paren-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 8
              fill-column 80)

(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(ido-mode t)

(setq ido-everywhere t
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1
      visible-bell nil
      tab-always-indent 'complete
      tramp-default-method "ssh"
      vc-follow-symlinks t
      tramp-copy-size-limit nil
      save-interprogram-paste-before-kill t
      dired-auto-revert-buffer t
      max-mini-window-height nil)

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(delete-selection-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq password-cache-expiry nil
      epa-pinentry-mode 'loopback)

(fset 'yes-or-no-p 'y-or-n-p)

(winner-mode 1)

(defun my-external-browser (url)
  (start-process-shell-command "chromium" nil (concat "chromium " url)))

(setq browse-url-browser-function 'my-external-browser)

;; handy functions

(defun split-file-by-delim (FILE delim)
  ;; e.g. (split-file-by-delim "~/.bashrc" "\n")
  ;; note: useful when used also with subr-x's join-string...
  (with-temp-buffer
    (insert-file-contents FILE)
    (split-string (buffer-string) delim t)))

(defun split-string-by-delim (STRING delim)
  ;; e.g. (split-string-by-delim "23:25:35" ":")
  ;; note: useful when used also with subr-x's join-string...
  (with-temp-buffer
    (princ STRING (current-buffer))
    (split-string (buffer-string) delim t)))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

;; keychain

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(defun keychain-unlock ()
    (interactive)
    (async-shell-command
     "eval $(keychain --eval --agents ssh,gpg id_rsa 77CF5C5C65A8F9F44940A72CDD4795B51117D906); emacsclient -e '(keychain-refresh-environment)'"))

(defun keychain-lock ()
  (interactive)
  (async-shell-command "keychain --agents ssh,gpg -k all"))

;; tramp stuff

(defun my-pwd ()
  "Show the real pwd whether we are tramp-root or regular user"
  (interactive)
  (if (string-match "root@" (pwd))
      (string-trim
       (format "%s" (cddr (split-string-by-delim default-directory ":"))) "\(" "\)")
    (string-trim default-directory)))

(defun toor ()
  (interactive)
  "change Emacs internal directory to user (away from tramp root)"
  (if (string-match "root@" (pwd))
      (cd (my-pwd))))

(defun suroot ()
  (interactive)
  "change Emacs internal directory to root using su"
  (if (not (string-match "root@" (pwd)))
      (cd (concat "/su:root@"system-name":"default-directory))))

(defun tooroot ()
  (interactive)
  "toggle Emacs internal directory using su"
  (if (string-match "root@" (pwd))
      (toor)
    (suroot))
  (pwd))

(defun eshell-tramp-su ()
  "tramp su back and forth in the Eshell."
  (interactive)
  (if (string-match "*eshell" (format "%s" (current-buffer)))
      (progn
        (if (string-match "root@" (pwd))
            (progn
              (insert (concat "cd" " " (my-pwd)))
              (eshell-send-input))
          (progn
            (insert (concat "cd /su:root@"system-name":"default-directory))
            (eshell-send-input))))))

(defun my-su-edit ()
  "WE DON'T NEED SUDO, we have su!"
  (interactive)
  ;; eshell
  (when (string= "eshell-mode" major-mode)
    (eshell-tramp-su))

  ;; dired
  (when (string= "dired-mode" major-mode)
    (find-file (concat "/su:root@"system-name":"(expand-file-name "."))))

  ;; file
  (when (buffer-file-name)
    (find-file (concat "/su:root@"system-name":"(buffer-file-name)))))

;; operating system

(defun kiss ()
  "front-end for getkiss.org linux package manager"
  (interactive)
  (with-temp-buffer
    (if (not (string-match "root@" (pwd)))
        (cd (concat "/su:root@"system-name":"default-directory)))
    (setq-local
     my-read
     (read-string "kiss [b|c|i|l|r|s|u] [pkg] [pkg] [pkg] " ""))
    (async-shell-command (concat "kiss " my-read "|| echo err $?"))
    (delete-other-windows)
    (switch-to-buffer "*Async Shell Command*")))

;; nearly all of my binds are here

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-c ku") (lambda()(interactive)(toor)(keychain-unlock)))
(global-set-key (kbd "C-c kl") (lambda()(interactive)(toor)(keychain-lock)))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<C-kp-add>") (lambda()(interactive)(my-font-resizer 1)))
(global-set-key (kbd "<C-kp-subtract>") (lambda()(interactive)(my-font-resizer -1)))
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c f") 'flycheck-mode)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-x ;") 'comment-line)
(global-set-key (kbd "C-c ;") 'comment-line)
(global-set-key (kbd "C-c C-;") 'comment-line)
(global-set-key (kbd "C-c C-p") 'backward-paragraph)
(global-set-key (kbd "C-c C-n") 'forward-paragraph)
(global-set-key (kbd "C-c t r") 'region-to-termbin)
(global-set-key (kbd "C-c t b") 'buffer-to-termbin)
(global-set-key (kbd "C-c I") 'crux-find-user-init-file)
(global-set-key (kbd "C-c S") 'my-su-edit)
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
(global-set-key (kbd "C-c C-k") 'crux-kill-whole-line)
(global-set-key (kbd "C-o") 'crux-smart-open-line)
(global-set-key (kbd "C-c C-l") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c C-;") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key "%" 'match-paren)
(global-set-key (kbd "M-y") 'browse-kill-ring)
(global-set-key (kbd "C-t") 'eshell)
(define-key dired-mode-map (kbd "C-t") 'eshell)
(define-key org-mode-map (kbd "C-t") 'eshell)
(global-set-key (kbd "C-c C-t") 'eshell-here)
(global-set-key (kbd "C-x TAB") 'spacemacs/alternate-buffer)
(global-set-key (kbd "C-x w") 'spacemacs/alternate-window)

(defun dotfiles-install ()
  "Yes, I know this is not sane, but please just let me be"
  (interactive)
  (progn
    ;; Keep magit happy by using gnu diffutils instead of busybox
    (suroot)
    (start-process-shell-command "ln" nil "ln -sf /opt/gnu/diffutils/bin/* /usr/bin")
    (toor))

  (start-process-shell-command
   "ln" nil
   "DIR=~/.emacs.d/straight/versions ; \[ -d \"$DIR\" ] && rm -rf $DIR && ln -sf ~/repos/dotfiles/versions $DIR")

  (f-write-text "dotfiles" 'utf-8 "~/.emacs.d/.dotfiles")

  (make-directory "~/bin" t)
  (start-process-shell-command "ln" nil "ln -sf ~/repos/dotfiles/bin ~/")

  (setq dotfiles-xresources "Xft.dpi: 96
Xft.autohint: 0
Xft.antialias: 1
Xft.hinting: true
Xft.hintstyle: hintslight
Xft.rgba: rgb
Xft.lcdfilter: lcddefault")

  (f-write-text dotfiles-xresources 'utf-8 "~/.Xresources")

  (setq root-dot-profile (concat my-path "
export CFLAGS=\"-O2 -pipe\"
export CXXFLAGS=\"-O2 -pipe\"
export MAKEFLAGS=\"-j$(nproc)\""))
  (f-write-text root-dot-profile 'utf-8
                (concat "/su:root@"system-name":/root/.profile"))

  (setq dotfiles-profile "PATH=~/bin:$PATH

. \"$HOME/.nix-profile/etc/profile.d/nix.sh\"
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH # https://github.com/NixOS/nix/issues/2033

echo \"start X?\"
read -r && \[ -z \"$DISPLAY\" ] && sx")

  (f-write-text dotfiles-profile 'utf-8 "~/.profile")

  (setq dotfiles-gitconfig "\[user]
email = paxchristi888@gmail.com
name = Adam Schaefers
signingkey = 77CF5C5C65A8F9F44940A72CDD4795B51117D906
\[commit]
        gpgsign = true")

  (f-write-text dotfiles-gitconfig 'utf-8 "~/.gitconfig")

  (setq dotfiles-gnupg-gpg-agent-conf "default-cache-ttl 84000
max-cache-ttl 84000
allow-emacs-pinentry
allow-loopback-pinentry
pinentry-program /home/adam/bin/pinentry-emacs")

  (f-write-text dotfiles-gnupg-gpg-agent-conf 'utf-8 "~/.gnupg/gpg-agent.conf")

  (setq dotfiles-config-mpv "profile=gpu-hq
scale=ewa_lanczossharp
cscale=ewa_lanczossharp
video-sync=display-resample
interpolation
tscale=oversample")

  (make-directory "~/.config/mpv" t)
  (f-write-text dotfiles-config-mpv 'utf-8 "~/.config/mpv/mpv.conf")

  (setq dotfiles-xinitrc "#!/bin/sh

while ! xprop -root | grep -q Free; do sleep 1; done
internal=LVDS1
external=VGA1
if xrandr | grep -q \"$external connected\" ; then  xrandr --output $internal --off --output $external --auto ; fi
xset +dpms
xset s 1800
xset b off
xset dpms 0 0 1860

xset r rate 200 60

touchpad=\"$(xinput list | awk '/TouchPad/ { print $7 }')\"
xinput set-prop \"${touchpad#id=}\" 'libinput Tapping Enabled' 1
xinput set-prop \"${touchpad#id=}\" 'libinput Accel Speed' 0.4

xsetroot -solid black -cursor_name left_ptr
xrdb -merge ~/.Xresources

exec dwm")

  (f-write-text dotfiles-xinitrc 'utf-8 "~/.xinitrc"))

;; environment variables
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))
(setenv "MAKEFLAGS" "-j5")
(setenv "CFLAGS" "-O2 -pipe")
(setenv "CXXFLAGS" "-O2 -pipe")
(setenv "KISS_PATH" "/var/db/kiss/repo/core:/var/db/kiss/repo/extra:/var/db/kiss/repo/xorg:/root/community/community:/home/adam/repos/community/community")

;; PATH

(setq my-path-insert (concat
                      "/home/" user-login-name "/bin:"
                      "/home/" user-login-name "/.local/bin:"
                      "/opt/awk/awk/bin:"
                      "/opt/gnu/coreutils/bin:"
                      "/opt/gnu/findutils/bin:"
                      "/opt/gnu/diffutils/bin:"
                      "/opt/gnu/gawk/bin:"
                      "/opt/gnu/patch/bin:"))

(setq my-path-append ":/rocks/more/bin")

(setq my-path-inherited (getenv "PATH"))

(setenv "PATH"
        (string-join
         (setq my-path
               (delete-dups (split-string-by-delim
                             (setenv "PATH" (concat
                                             my-path-insert
                                             my-path-inherited
                                             my-path-append)) ":")))":"))

(setq my-path (concat "PATH=" (getenv "PATH")))

;; set (tramp-)root's path and env

(defvar my-sync-root-path t
  "Keep root's (tramp-)PATH in sync with Emacs environment")

(if (bound-and-true-p my-sync-root-path)
    (progn
      ;; add local user's path to roots path
      (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
      ;; define roots env
      (setq tramp-remote-process-environment
            '("ENV=''"
              "TMOUT=0"
              "LC_CTYPE=''"
              "EDITOR=ed"
              "PAGER=cat"
              "MAKEFLAGS=j5"
              "CFLAGS=-O2 -pipe"
              "CXXFLAGS=-O2 -pipe"
              "KISS_PATH=/var/db/kiss/repo/core:/var/db/kiss/repo/extra:/var/db/kiss/repo/xorg:/root/community/community:/home/adam/repos/community/community"))))

;; eshell hooks

;; auto ls
(add-hook 'eshell-directory-change-hook 'eshell/ls)

;; eshell alias / functions

(defun eshell/emacs (file)
  "Intercept the accidental execution of emacs"
  (find-file file))

(defun eshell/- ()
  (insert "cd -")
  (eshell-send-input))

(defun eshell/.. (&optional counter)
  (defun up ()
    (insert "cd ..")
    (eshell-send-input))
  (if (numberp counter)
      (while (> counter 0)
        (up)
        (setq counter (1- counter)))
    (up)))

;; MISC

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (eshell "new")
    (insert "ls")
    (eshell-send-input)
    (rename-buffer (concat "*eshell: " name "*"))))

;; make scripts executeable automatically
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; a shell-mode hook
(defun my-shell-mode-hook()
  (setq-local compile-command
              '((lambda()
                  (save-buffer)
                  (async-shell-command (buffer-file-name))))))
(add-hook 'sh-mode-hook 'my-shell-mode-hook)

(defun region-to-termbin (start end)
  "push the marked region to termbin.com via shell command"
  (interactive "r")
  (message "pushing region to termbin.com...")
  (shell-command-on-region start end "nc termbin.com 9999" "*Termbin*")
  (switch-to-buffer "*Termbin*"))

(defun buffer-to-termbin ()
  "push the whole buffer to termbin.com via shell command"
  (interactive)
  (message "pushing buffer to termbin.com...")
  (shell-command-on-region (point-min) (point-max) "nc termbin.com 9999" "*Termbin*")
  (switch-to-buffer "*Termbin*"))

(add-hook 'before-save-hook 'whitespace-cleanup)

(show-paren-mode 1)

(electric-pair-mode 1)

(setq magit-diff-refine-hunk t)
(setq magit-repository-directories '(("~/repos" . 1)))

(projectile-mode 1)

(aggressive-indent-global-mode 1)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "<M-tab>") 'hippie-expand)
(global-set-key (kbd "<C-tab>") 'hippie-expand)
(add-hook 'eshell-mode-hook '(lambda ()
                               (interactive) ;; hippie-expand breaks eshell!!@#$
                               (define-key eshell-mode-map (kbd "M-/") 'dabbrev-expand)))

(defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when hippie expanding in a lisp editing mode"
  (if (and (lispy-mode)
           (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

;;; LANGS

;; C

(defun c-mode-common-defaults ()
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'c-mode-common-defaults)

(defun makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'makefile-mode-defaults)

;; lisp

(add-hook 'emacs-lisp-mode-hook 'lispy-mode)
(add-hook 'ielm-mode-hook 'lispy-mode)
(add-hook 'lisp-mode-hook 'lispy-mode)
(add-hook 'slime-repl-mode-hook 'lispy-mode)

(defun my-ielm ()
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'my-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

(add-hook 'ielm-mode-hook 'eldoc-mode)

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(setq slime-default-lisp 'sbcl)
(setq slime-contribs '(slime-fancy slime-cl-indent))
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-fuzzy-completion-in-place t
      slime-enable-evaluate-in-emacs t
      slime-autodoc-use-multiline-p t)

(with-eval-after-load 'slime
  (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))


(defun my-erc ()
  (interactive)
  (erc-tls :server "chat.freenode.net" :port "6697"))

(with-eval-after-load 'erc
  (setq erc-hide-list '("JOIN" "PART" "QUIT")
        erc-autojoin-timing "ident"
        erc-prompt-for-password nil
        erc-nick "adamantium"
        erc-autojoin-channels-alist '(("freenode.net"
                                       "#kisslinux"
                                       "#oasislinux"
                                       "#phrackaged2"
                                       "#liguros"
                                       "#commanduser"
                                       "#emacs")))

  (defun my-erc-multi-line-disable (string)
    "disable sending of multi-line messages entirely to avoid accidental flooding"
    (if (string-match-p "\n+" string)
        (setq str nil)))
  (add-hook 'erc-send-pre-hook 'my-erc-multi-line-disable))

(setq gnus-use-full-window nil)
(setq gnus-site-init-file "~/.emacs")
(setq gnus-save-newsrc-file nil)
(setq gnus-startup-file "~/.emacs.d/.newsrc")
(setq message-directory "~/.emacs.d/mail/")
(setq gnus-directory "~/.emacs.d/news/")
(setq nnfolder-directory "~/.emacs.d/mail/archive")
(setq nndraft-directory "~/.emacs.d/mail/drafts")
(setq gnus-always-read-dribble-file t)

(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl)
                                  (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                                  (nnmail-expiry-wait immediate)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-debug-info t smtpmail-debug-verb t
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      gnus-message-archive-method '(nnimap "gmail")
      gnus-message-archive-group "nnimap+gmail:[Gmail]/Sent Mail"
      gnus-gcc-mark-as-read t)

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext")
  (setq mm-automatic-display (remove "text/html" mm-automatic-display)))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-visible-headers
      "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")
(setq gnus-sorted-header-list
      '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
        "^Subject:" "^Date:" "^Gnus"))
(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-group-line-format "%M%S%p%P%5y:%B %G\n";;"%B%(%g%)"
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│"
 gnus-article-browse-delete-temp t
 gnus-treat-strip-trailing-blank-lines 'last
 gnus-keep-backlog 'nil
 gnus-summary-display-arrow nil
 gnus-mime-display-multipart-related-as-mixed t
 gnus-auto-select-first nil
 smiley-style 'medium
 gnus-keep-backlog '0)

(setq gnus-no-groups-message "")

;; abook

(when (file-exists-p my-contacts-file)
  (progn
    (load-file my-contacts-file)

    ;; e.g. dummy address book (key . value) list
    ;; (setq my-contact-list '((name . foo@bar.email)
    ;;                         (nick . nick@nick.com)
    ;;                         (john . john@doe.com)))

    (setq my-contact-keys (cl-loop for (key . value) in my-contact-list
                                   collect key))

    (defun abook ()
      "Insert an email address from `my-contact-list' to the current buffer."
      (interactive)
      (let ((item my-contact-keys))
        (fset 'my-read 'completing-read)

        ;; interactive menu + convert chosen item (key) from string to data
        (setq-local interactive-chosen-key (intern (my-read "Contact Name:" item)))
        ;; match key to list and get associated email (value), convert back to string
        (setq-local email (format "%s" (cdr (assq interactive-chosen-key my-contact-list))))

        ;; output email address to buffer
        (princ email (current-buffer))))))
