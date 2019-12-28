;;; -*- lexical-binding: t; -*-

;;;; EMACS OPERATING SYSTEM

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

;;;;lib

(require 'server)
(require 'tramp)
(require 'eww)
(require 'subr-x)            ;Extra Lisp functions
(require 'seq)               ;Sequence manipulation functions
(require 'cl-lib)            ;Common Lisp extensions

;;;;lib+

(straight-use-package 'dash) ;A modern list library
(straight-use-package 'a)    ;Associative data structure functions
(straight-use-package 's)    ;String manipulation library
(straight-use-package 'f)    ;Modern API for working with files and directories
(straight-use-package 'ht)   ;The missing hash table library

;;;;pkgs

(straight-use-package 'bind-key)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'aggressive-indent)
(straight-use-package 'paredit)
(straight-use-package 'elisp-slime-nav)
(straight-use-package 'slime)

;;;;manual-installed pkgs

(add-to-list 'load-path "~/repos/dotfiles/site-lisp")
(require 'keychain-environment)
(require 'browse-kill-ring)
(require 'crux)

;;;;ENV/PATH

(setq shell-file-name "/bin/sh")
(setenv "SHELL" "/bin/sh")
(setenv "PAGER" "cat")
(setenv "EDITOR" "ed")
(setenv "VISUAL" (getenv "EDITOR"))

(setq my-path-inherited (getenv "PATH")) ;; todo just /etc/profile

(setq my-path-insert (concat
                      "/home/" user-login-name "/bin:"
                      "/home/" user-login-name "/.local/bin:"
                      "/opt/awk/bin:"
                      "/opt/gnu/coreutils/bin:"
                      "/opt/gnu/findutils/bin:"
                      "/opt/gnu/diffutils/bin:"
                      "/opt/gnu/gawk/bin:"
                      "/opt/gnu/grep/bin:"
                      "/opt/gnu/patch/bin:"
                      "/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/sbin:/sbin:/bin"))

(setq my-path-append (concat ":" exec-directory))

(setenv "PATH"
        (string-join
         (delete-dups (split-string
                       (setenv "PATH" (concat
                                       my-path-insert
                                       my-path-inherited
                                       my-path-append)) ":"))":"))

(setq exec-path (split-string (getenv "PATH")  ":"))

(defvar my-sync-root-path nil ;; probably not a good idea
  "Keep root's (tramp-)PATH in sync with Emacs environment")

(when (bound-and-true-p my-sync-root-path)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; define remote tramp env
(setq tramp-remote-process-environment
      '("ENV=''"
        "TMOUT=0"
        "LC_CTYPE=''"
        "EDITOR=ed"
        "PAGER=cat"
        "MAKEFLAGS=j5"
        "CFLOAGS=-O2 -pipe"
        "CXXFLAGS=-O2 -pipe"
        "KISS_PATH=/home/adam/repos/dotfiles/kiss-overlay:/home/adam/repos/community/community:/var/db/kiss/repo/core:/var/db/kiss/repo/extra:/var/db/kiss/repo/xorg:/root/community/community"))

;;;;$ chsh -s /bin/emacs

(add-hook 'after-init-hook '(lambda()
                              (when (not (server-running-p))
                                (server-start))
                              (kill-buffer "*scratch*")
                              (eshell)))

(defun eshell/startx ()
  (setenv "DISPLAY" ":0")
  (start-process "Xorg" nil "Xorg" "-nolisten" "tcp" "-nolisten" "local" ":0" "vt7"))

(defun eshell/xinitrc ()
  (start-process "xset" nil "xset" "+dpms")
  (start-process "xset" nil "xset" "b" "off")
  (start-process "xset" nil "xset" "dpms" "0" "0" "1860")
  (start-process "xset" nil "xset" "r" "rate" "175" "50")
  (start-process "xrdb" nil "xrdb" "~/.Xresources")
  (start-process "Esetroot" nil "Esetroot" "-fit" (concat (getenv "HOME") "/.wallpaper"))
  (start-process "compton" nil "compton" "--backend" "glx")
  (start-process "dwm" nil "dwm"))

(defun eshell/sx ()
  (insert "startx && xinitrc")
  (eshell-send-input))

;;;;bind-key

;; unbind
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; minor modes may override
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-a" 'crux-move-beginning-of-line)
(bind-key "C-c C-k" 'crux-kill-whole-line)

;; minor modes may not override (global)
(bind-key* "<f5>" 'compile)
(bind-key* "C-t" 'eshell)
(bind-key* "C-x C-b" 'ibuffer)
(bind-key* "C-o" 'crux-smart-open-line)
(bind-key* "C-c g" 'magit-status)
(bind-key* "C-c p" 'projectile-command-map)
(bind-key* "C-c f" 'flycheck-mode)
(bind-key* "C-c t r" 'region-to-termbin)
(bind-key* "C-c t b" 'buffer-to-termbin)
(bind-key* "C-c #" 'my-su-edit)
(bind-key* "C-c $" 'my-switch-to-home)
(bind-key* "C-c I" 'crux-find-user-init-file)
(bind-key* "C-c C-l" 'crux-duplicate-current-line-or-region)
(bind-key* "C-c ;" 'crux-duplicate-and-comment-current-line-or-region)
(bind-key* "C-x ;" 'comment-line)
(bind-key* "M-o" 'spacemacs/alternate-buffer)
(bind-key* "C-M-o" 'spacemacs/alternate-window)
(bind-key* "M-RET" 'other-window)
(bind-key* "M-1" 'delete-other-windows)
(bind-key* "M-2" 'split-window-below)
(bind-key* "M-3" 'split-window-right)
(bind-key* "M-0" 'delete-window)
(bind-key* "M--" 'bury-buffer)
(bind-key* "M-y" 'browse-kill-ring)
(bind-key* "M-/" 'hippie-expand)

;;;;theme

(menu-bar-mode -1)
(add-hook 'prog-mode-hook (lambda () (font-lock-mode -1)))

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

;;;;settings

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      my-contacts-file "~/contacts.el"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen t
      custom-file "/dev/null"
      gc-cons-threshold 100000000
      debug-on-error nil
      apropos-do-all t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      ido-everywhere t
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1
      tab-always-indent 'complete
      tramp-default-method "ssh"
      vc-follow-symlinks t
      tramp-copy-size-limit nil
      dired-auto-revert-buffer t
      browse-url-browser-function 'eww-browse-url
      password-cache-expiry nil
      epa-pinentry-mode 'loopback)

(custom-set-variables '(epg-gpg-program  "/bin/gpg2"))

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 8
              fill-column 80)

(ido-mode t)

(show-paren-mode 1)

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

;;;;functions

(defun scrot ()
  (interactive)
  (random t)
  (start-process "import" nil "import" "-window" "root"
                 (concat (getenv "HOME") "/scrot" (format "%s" (random)) ".png")))

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

(defun split-file (FILE delim)
  (with-temp-buffer
    (insert-file-contents FILE)
    (split-string (buffer-string) delim t)))

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(defun unlock ()
  (interactive)
  (async-shell-command
   "eval $(keychain --eval --agents ssh,gpg id_rsa 77CF5C5C65A8F9F44940A72CDD4795B51117D906); emacsclient -e '(keychain-refresh-environment)'"))

(defun lock ()
  (interactive)
  (async-shell-command "keychain --agents ssh,gpg -k all"))

(defun my-pwd ()
  "Show the real pwd whether we are tramp-root or regular user"
  (interactive)
  (if (string-match "root@" (pwd))
      (string-trim
       (format "%s" (cddr (split-string default-directory ":"))) "\(" "\)")
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
            (insert (concat "cd /su::"default-directory))
            (eshell-send-input))))))

(defun my-switch-to-home ()
  (interactive)
  (cd "~/")
  (dired "."))

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

(defun ekiss ()
  "front-end for getkiss.org linux package manager"
  (interactive)
  (with-temp-buffer
    (if (not (string-match "root@" (pwd)))
        (cd "/su::"))
    (setq-local
     my-read
     (read-string "kiss [b|c|i|l|r|s|u] [pkg] [pkg] [pkg] " ""))
    (async-shell-command (concat "kiss " my-read "|| echo err $?"))
    (delete-other-windows)
    (switch-to-buffer "*Async Shell Command*")))

(defun suckless-recompile (x)
  (with-temp-buffer
    (cd "/su::")
    (async-shell-command
     (concat "kiss c " x "&& kiss b " x "&& kiss i " x))))

;;;;eshell

(add-hook 'eshell-directory-change-hook 'eshell/ls)

(defun eshell/emacs (file)
  "Intercept the accidental execution of emacs"
  (find-file file))

(defun eshell/- ()
  (insert "cd -")
  (eshell-send-input))

(defun eshell/.. (&optional counter)
  (if (numberp counter)
      (while (> counter 0)
        (insert "cd ..")
        (eshell-send-input)
        (setq counter (1- counter)))
    (progn
      (insert "cd ..")
      (eshell-send-input))))

;;;;programming

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

(add-hook 'eshell-mode-hook '(lambda ()
                               (define-key eshell-mode-map (kbd "M-/") 'dabbrev-expand)))

(defadvice he-substitute-string (after he-paredit-fix) ;; hippie-expand also breaks paredit
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

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
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

(defun erc-freenode-connect ()
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
  (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
    (with-timeout
        (3 (message "Gnus timed out."))
      ad-do-it))

  (gnus-demon-add-handler 'gnus-demon-scan-news 1 t)
  (gnus-demon-add-scanmail))

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

;;;;dotfiles

(defun dotfiles-install ()
  "Yes, I know this is not sane, but please just let me be"
  (interactive)
  (require 'f)
  (progn
    ;; Keep magit happy by using gnu diffutils instead of busybox
    ;; Seems magit/git is not respecting my PATH and hardcoded to use /bin/diff ...
    (suroot)
    (start-process-shell-command "ln" nil "ln -sf /opt/gnu/diffutils/bin/* /usr/bin")
    (start-process-shell-command "ln" nil "ln -sf /opt/gnu/patch/bin/* /usr/bin")
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

  (setq dotfiles-xinitrc "
st -e emacsclient -t -e \\(eshell\\) &
exec dwm")

  (f-write-text dotfiles-xinitrc 'utf-8 "~/.xinitrc"))

;;;;goodies

(defun my-external-browser (url)
  (start-process-shell-command "chromium" nil (concat "netsurf " url)))

(setq browse-url-browser-function 'eww-browse-url
      shr-external-browser 'my-external-browser
      eww-search-prefix "https://www.google.com/search?hl=en&q=")

(defvar yt-dl-player "mpv"
  "Video player used by `eww-open-yt-dl'")

(defun eww-open-yt-dl ()
  "Browse youtube videos using the Emacs `eww' browser and \"youtube-dl.\"
Specify the video player to use by setting the value of `yt-dl-player'"
  (interactive)
  (if (executable-find "youtube-dl")
      (progn
        (eww-copy-page-url)
        (start-process-shell-command "youtube-dl" nil
                                     (concat "youtube-dl -o - " (nth 0 kill-ring) " - | " yt-dl-player " -")))
    (progn
      (setq xbuff (generate-new-buffer "*youtube-dl not found*"))
      (with-output-to-temp-buffer xbuff
        (print "Ensure youtube-dl is installed on the system and try again...")))))

(define-key eww-mode-map (kbd "^") 'eww-open-yt-dl)
