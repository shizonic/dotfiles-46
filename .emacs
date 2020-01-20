;;; -*- lexical-binding: t; -*-

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen nil
      load-prefer-newer t
      custom-file "/dev/null"
      package-enable-at-startup nil
      gc-cons-threshold 50000000)

(with-eval-after-load 'gnutls
  (setq
   gnutls-verify-error t
   gnutls-min-prime-bits 2048
   gnutls-trustfiles '("/etc/ssl/cert.pem")))

(add-hook 'after-init-hook (lambda()
                             (require 'server)
                             (when (not (server-running-p))
                               (server-start))))

;;;;bootstrap straight.el

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

;;;;libs

(require 'subr-x)            ;Extra Lisp Functions
(require 'seq)               ;Sequence manipulation functions
(require 'cl-lib)            ;Common Lisp extensions
(straight-use-package 'dash) ;A modern list library
(straight-use-package 'a)    ;Associative data structure functions
(straight-use-package 's)    ;String manipulation library
(straight-use-package 'f)    ;Modern API for working with files and directories
(straight-use-package 'ht)   ;The missing hash table library
(straight-use-package 'async);Simple library for asynchronous processing in Emacs

;;;;pkgs

(straight-use-package 'bind-key)
(straight-use-package ;; TODO
 '(emacs-piper :type git :host gitlab :repo "howardabrams/emacs-piper"))
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'aggressive-indent)
(straight-use-package 'paredit)
(straight-use-package 'crux)
(straight-use-package 'keychain-environment)
(straight-use-package 'browse-kill-ring)
(straight-use-package 'elisp-slime-nav)
(straight-use-package 'slime)

;;;;bindings

;; unbind
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; minor modes may override
(bind-key "C-a" 'crux-move-beginning-of-line)
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)

;; minor modes may not override (global)
(bind-key* "C-x C-b" 'ido-switch-buffer)
(bind-key* "M-y" 'browse-kill-ring)
(bind-key* "C-c a" 'abook)
(bind-key* "C-c m" 'gnus)
(bind-key* "C-c b" 'eww)
(bind-key* "C-c B" (lambda ()
                     (interactive)
                     (call-process-shell-command "surf google.com &" nil nil 0)))
(bind-key* "C-c i" 'freenode)
(bind-key* "C-c p" 'projectile-command-map)
(bind-key* "C-c g" 'magit-status)
(bind-key* "C-c f" 'flycheck-mode)
(bind-key* "C-c t r" 'region-to-termbin)
(bind-key* "C-c t b" 'buffer-to-termbin)
(bind-key* "C-c #" 'my-su-edit)
(bind-key* "C-c I" (lambda () (interactive) (find-file user-init-file)))
(bind-key* "C-o" 'crux-smart-open-line)
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c C-k") 'crux-kill-whole-line)))
(bind-key* "C-c C-l" 'crux-duplicate-current-line-or-region)
(bind-key* "C-c ;" 'crux-duplicate-and-comment-current-line-or-region)
(bind-key* "C-x ;" 'comment-line)
(bind-key* "<f5>" 'compile)
(bind-key* "C-t" 'shell)
(bind-key* "C-c -" 'bury-buffer)
(bind-key "M-/" 'hippie-expand)

;;;;theme

;; disable an old theme before enabling a new theme
;; (not sure why this is not an Emacs default...)
(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; disable italics completely
(mapc
 (lambda (face)
   (set-face-attribute face nil :slant 'normal))
 (face-list))

;; disable color completely
(setq-default default-frame-alist '((tty-color-mode . never)))

;; true spartans should also uncomment the next line:
;; (global-font-lock-mode -1)

;; highlight functions using [only] bold.
(custom-set-faces '(font-lock-function-name-face ((t (:weight bold)))))

;; minimalist modeline
(setq-default mode-line-format
              '((:eval (format-mode-line "%* %b %l:%c"))))


;;;;settings

(setq apropos-do-all t
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      save-interprogram-paste-before-kill t
      tab-always-indent 'complete
      tramp-default-method "ssh"
      vc-follow-symlinks t
      tramp-copy-size-limit nil
      dired-auto-revert-buffer t
      password-cache-expiry nil
      epa-pinentry-mode 'loopback)

(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))

(defun nox-mouse-support ()
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") '(lambda () (interactive) (scroll-down 5)))
  (global-set-key (kbd "<mouse-5>") '(lambda () (interactive) (scroll-up 5))))

(defun nox-xsel-support ()
  (setq x-select-enable-clipboard t)

  (with-eval-after-load 'tramp
    (add-to-list 'tramp-remote-process-environment
                 (concat "DISPLAY="xsel-saved-display)))

  (defun xsel-cut-function (text &optional push)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))

  (defun xsel-paste-function()
    (let ((xsel-output (shell-command-to-string "DISPLAY=:0 xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
        xsel-output)))

  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function))

(setq xsel-saved-display (getenv "DISPLAY"))
(when (not (string-equal xsel-saved-display ""))
  (progn
    (nox-mouse-support)

    (when (executable-find "xsel")
      (nox-xsel-support))))


(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 8
              fill-column 80)

(ido-mode 1)
(ido-everywhere 1)
(setq ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess)

(show-paren-mode 1)

(require 'dired-x)
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(delete-selection-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(winner-mode 1)

(defun external-browser (url)
  (call-process-shell-command (concat "surf " url " &") nil nil 0))

(setq browse-url-browser-function 'eww-browse-url
      shr-external-browser 'external-browser
      eww-search-prefix "https://www.google.com/search?hl=en&q=")

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "^") 'eww-open-yt-dl)
  (define-key eww-mode-map (kbd "W") 'shr-copy-url))

(defvar yt-dl-player "mpv"
  "Video player used by `eww-open-yt-dl'")

;;;;functions

(defun shutdown ()
  (interactive)
  (let ((choices '("reboot" "poweroff")))
    (message "%s" (setq temp-shutdown-choice (ido-completing-read "Shutdown:" choices )))
    (with-temp-buffer
      (cd "/su::")
      (if (string= temp-shutdown-choice "reboot")
          (async-shell-command "kill -s INT 1")
        (async-shell-command "kill -s USR1 1")))))

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(defun unlock ()
  (interactive)
  (async-shell-command
   "eval $(keychain --eval --agents gpg,ssh 77CF5C5C65A8F9F44940A72CDD4795B51117D906 id_rsa); emacsclient -e '(keychain-refresh-environment)'"))

(defun lock ()
  (interactive)
  (async-shell-command "keychain --agents ssh,gpg -k all"))

(defun eww-open-yt-dl ()
  "Browse youtube videos using the Emacs `eww' browser and \"youtube-dl.\"
Specify the video player to use by setting the value of `yt-dl-player'"
  (interactive)
  (when (executable-find "youtube-dl")
    (progn
      (if (string-match  "*eww*" (format "%s"(current-buffer)))
          (eww-copy-page-url)
        (with-temp-buffer (yank)))
      (start-process-shell-command "youtube-dl" nil
                                   (concat "youtube-dl -o - " (nth 0 kill-ring) " - | " yt-dl-player " -")))))

;;;;programming

(setq grep-command "grep -r ")

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

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

(defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when hippie expanding in a lisp editing mode"
  (if (and (paredit-mode)
           (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

;;;;langs

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

(defun my-elisp-mode-hook()
  (setq-local compile-command
              '((lambda()
                  (save-buffer)
                  (async-shell-command (buffer-file-name))))))
(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'slime-repl-mode-hook 'paredit-mode)

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

;;;;irc

(defun freenode ()
  (interactive)
  (erc-tls :server "chat.freenode.net" :port "6697"))

(with-eval-after-load 'erc
  (erc-track-mode -1)
  (setq erc-hide-list '("JOIN" "PART" "QUIT")
        erc-autojoin-timing "ident"
        erc-prompt-for-password nil
        erc-nick "adamantium"
        erc-autojoin-channels-alist '(("freenode.net"
                                       "#kisslinux"
                                       "#liguros"
                                       "#commanduser"
                                       "#emacs")))

  (defun my-erc-multi-line-disable (string)
    "disable sending of multi-line messages entirely to avoid accidental flooding"
    (if (string-match-p "\n+" string)
        (setq str nil)))
  (add-hook 'erc-send-pre-hook 'my-erc-multi-line-disable))

;;;;email

(with-eval-after-load 'gnus
  (setq read-mail-command 'gnus)
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
        smtpmail-smtp-service 465
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type  'ssl
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
   gnus-group-line-format "%M%S%p%P%5y:%B %G\n" ;;"%B%(%g%)"
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
   gnus-keep-backlog '0))

;; abook

(defvar my-abook "~/contacts.el") ;; TODO .gpg

(with-eval-after-load 'gnus
  (when (file-exists-p my-abook)
    (progn
      (load-file my-abook)

      ;; e.g. dummy address book
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
          (princ email (current-buffer)))))))

;;;;dotfiles

(defun dotfiles-install ()
  (interactive)

  (require 'f)

  ;; root dotfiles
  (with-temp-buffer
    (cd "/su::")
    (shell-command "ln -sf /usr/bin/gpg2 /usr/local/bin/gpg"))

  ;; put straight pkg versions under vc
  (start-process-shell-command
   "ln" nil
   "DIR=~/.emacs.d/straight/versions ; \[ -d \"$DIR\" ] && rm -rf $DIR && ln -sf ~/repos/dot-emacs/versions $DIR")

  ;; git
  (setq dotfiles-gitconfig "\[user]
email = paxchristi888@gmail.com
name = Adam Schaefers
signingkey = 77CF5C5C65A8F9F44940A72CDD4795B51117D906
\[commit]
        gpgsign = true")
  (f-write-text dotfiles-gitconfig 'utf-8 "~/.gitconfig")

  ;; gpg
  (setq dotfiles-gnupg-gpg-agent-conf "default-cache-ttl 84000
max-cache-ttl 84000
allow-emacs-pinentry
allow-loopback-pinentry
pinentry-program /home/adam/repos/dot-emacs/pinentry-emacs")
  (f-write-text dotfiles-gnupg-gpg-agent-conf 'utf-8 "~/.gnupg/gpg-agent.conf")

  ;; xresources
  (setq dotfiles-xresources "Xft.dpi: 96
Xft.autohint: 0
Xft.antialias: 1
Xft.hinting: true
Xft.hintstyle: hintslight
Xft.rgba: rgb
Xft.lcdfilter: lcddefault")
  (f-write-text dotfiles-xresources 'utf-8 "~/.Xresources")

  ;; xinitrc
  (setq dotfiles-xinitrc "laptop_touchpad() {
    touchpad=$\(xinput list | awk '/TouchPad/ { print $7 }'\)
    xinput set-prop ${touchpad#id=} 'libinput Tapping Enabled' 1
    xinput set-prop ${touchpad#id=} 'libinput Accel Speed' 0.4

    # also check to see if external monitor is plugged in...
    external=VGA-1
    internal=LVDS-1
    if xrandr | grep -q $external connected; then xrandr --output $internal --off --output $external --auto; fi
}
command -v xinput && laptop_touchpad &

xsetroot -cursor_name left_ptr &
xrdb ~/.Xresources &
picom --backend glx &
feh --no-fehbg --bg-max ~/.wallpaper &
while pgrep X; do xsetroot -name \"$\(date\)\"; sleep 60; done &

st -e emacs &

exec dwm")

  (f-write-text dotfiles-xinitrc 'utf-8 "~/.xinitrc")

  (setq dotfiles-xinit "trap 'DISPLAY=:0 ~/.xinitrc' USR1
{
    trap '' USR1
    exec X -nolisten tcp -nolisten local :0 vt1 v -arinterval 30 -ardelay 175 > .xserver.log 2>&1
} &
wait")

  (f-write-text dotfiles-xinit 'utf-8 "~/xinit")

  (setq dotfiles-profile "export PATH=/usr/local/bin:/bin")
  (f-write-text dotfiles-profile 'utf-8 "~/.profile")
  (set-file-modes "~/.xinitrc" #o755)
  (set-file-modes "~/xinit" #o755)

  (setq dotfiles-mkshrc "if ! test \"$\(id -u\)\" -eq 0; then
    PS1='$ '
else
    PS1='# '
fi

kiss() { PATH=/bin /bin/kiss \"$@\"; }

\[ -z \"$DISPLAY\" ] && \[ \"$\(tty\)\" = \"/dev/tty1\" ] && ./xinit")
  (f-write-text dotfiles-mkshrc 'utf-8 "~/.mkshrc"))
