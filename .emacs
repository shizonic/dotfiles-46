;;; -*- lexical-binding: t; -*-

;;;; THE ANTI DESKTOP EMACS OPERATING SYSTEM + Linux

(setq user-full-name "Adam Schaefers"
      user-mail-address "paxchristi888@gmail.com"
      initial-major-mode 'emacs-lisp-mode
      inhibit-startup-screen t
      load-prefer-newer t
      custom-file "/dev/null"
      tls-checktrust t
      gnutls-verify-error t
      package-archives nil
      package-enable-at-startup nil
      gc-cons-threshold 100000000)

;;;;ENV/PATH

(require 'subr-x)

(setq shell-file-name "/bin/sh")
(setenv "SHELL" "/bin/sh")

(setenv "HOME" (concat "/home/" user-login-name))
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))

(when (file-directory-p "/nix")
  (setenv "NIX_PROFILES" (concat (getenv "HOME") "/nix/var/nix/profiles/default " (getenv "$HOME") "/.nix-profile"))
  (setenv "NIX_PATH" (concat (getenv "HOME") "/.nix-defexpr/channels"))
  (setenv "NIX_SSL_CERT_FILE" (concat (getenv "HOME") "/.nix-profile/etc/ssl/certs/ca-bundle.crt")))

(defvar system-profile-path
  (string-trim (shell-command-to-string "grep -E '^export PATH' /etc/profile") "export PATH="))

(defvar my-path-insert
  (concat
   (when (file-directory-p "/nix") (concat (getenv "HOME") "/.nix-profile/bin:"))
   "/usr/local/bin:"))

(defvar my-path-append (concat ":" exec-directory))

(setenv "PATH"
        (string-join
         (setq-default exec-path
                       (delete-dups (split-string
                                     (concat
                                      my-path-insert
                                      system-profile-path
                                      my-path-append) ":"))) ":"))

(with-eval-after-load 'tramp
  (defvar my-sync-tramp-path nil
    "probably not a good idea setting this to t")

  (when (bound-and-true-p my-sync-tramp-path)
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

  ;; define remote tramp env
  (setq tramp-remote-process-environment
        ;; TODO :: find a way to set this on a per-tramp connection/machine basis
        '(;; original values
          "ENV=''"
          "TMOUT=0"
          "LC_CTYPE=''"
          "CDPATH="
          "HISTORY="
          "MAIL="
          "MAILCHECK="
          "MAILPATH="
          "autocorrect="
          "correct="

          ;; my values
          "EDITOR=ed"
          "PAGER=cat"
          "MAKEFLAGS=j5"
          "CFLOAGS=-O2 -pipe"
          "CXXFLAGS=-O2 -pipe"
          "KISS_PATH=/home/adam/repos/kiss-overlay:/home/adam/repos/community/community:/root/community/community:/var/db/kiss/repo/core:/var/db/kiss/repo/extra:/var/db/kiss/repo/xorg")))

;;;;$ chsh -s #!/bin/emacs --fg-daemon

(add-hook 'after-init-hook (lambda()
                             (require 'server)
                             (when (not (server-running-p))
                               (server-start)
                               (sx))))

(global-set-key (kbd "C-x C-c") 'kill-xsession)
(defun kill-xsession ()
  (interactive)
  (start-process-shell-command "pkill" nil "pkill -15 Xorg"))

(defun sx ()
  "A simple elisp replacement for startx/xinit scripts. Requires subr-x and async."
  (interactive)

  (setenv "DISPLAY" ":0")
  (start-process "Xorg" nil "Xorg" "-nolisten" "tcp" "-nolisten" "local" ":0" "vt1" "v" "-arinterval" "30" "-ardelay" "175")

  (async-start
   (lambda ()
     (while (not (string-match-p "XFree86_has_VT"
                                 (shell-command-to-string "xprop -root")))
       (sleep-for 0.5)))
   (lambda (result)

     ;; touchpad
     (start-process-shell-command
      "xinput" nil "touchpad=\"$(xinput list | awk '/TouchPad/ { print $7 }')\" ; xinput set-prop \"${touchpad#id=}\" 'libinput Tapping Enabled' 1 ; xinput set-prop \"${touchpad#id=}\" 'libinput Accel Speed' 0.4")

     ;; Xresources
     (start-process "xsetroot" nil "xsetroot" "-cursor_name" "left_ptr")
     (start-process "xrdb" nil "xrdb" (concat (getenv "HOME") "/.Xresources"))
     (start-process "picom" nil "picom" "--backend" "glx")
     (start-process "esetroot" nil "esetroot" "-fit" (concat (getenv "HOME") "/.wallpaper"))

     ;; Exwm xrandr - auto turn off laptop display and move to external monitor display when plugged in
     (with-eval-after-load 'exwm
       (defvar external "VGA-1")
       (defvar internal "LVDS-1")

       (defun switch-to-external-monitor ()
         (start-process
          "xrandr" nil
          "xrandr" "--output" internal "--off" "--output" external "--auto"))

       (defun  switch-to-internal-monitor ()
         (start-process
          "xrandr" nil
          "xrandr" "--output" external "--off" "--output" internal "--auto"))

       (defun xrandr ()
         ;; workaround when external is already plugged in on startup
         (when (and (string-match (concat external " connected")
                                  (shell-command-to-string "xrandr"))
                    (< (string-to-number (emacs-uptime "%s")) 5))
           (switch-to-internal-monitor))

         (if (string-match (concat external " connected")
                           (shell-command-to-string "xrandr"))
             (switch-to-external-monitor)
           (switch-to-internal-monitor)))

       (require 'exwm-randr)
       (add-hook 'exwm-randr-screen-change-hook 'xrandr)
       (add-hook 'exwm-randr-screen-change-hook (lambda ()
                                                  (start-process "esetroot" nil "esetroot" "-fit" (concat (getenv "HOME") "/.wallpaper"))))
       (exwm-randr-enable)

       (exwm-enable))

     ;; Exwm
     (start-process ;;unifont-12
      "emacsclient" nil "emacsclient" "-c" "-e" "(eshell)" "-e" "(set-frame-font \"unifont-24\" nil t)"))))

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

(require 'seq)               ;Sequence manipulation functions
(require 'cl-lib)            ;Common Lisp extensions
(straight-use-package 'dash) ;A modern list library
(straight-use-package 'a)    ;Associative data structure functions
(straight-use-package 's)    ;String manipulation library
(straight-use-package 'f)    ;Modern API for working with files and directories
(straight-use-package 'ht)   ;The missing hash table library
(straight-use-package 'async);Simple library for asynchronous processing in Emacs

;;;;pkgs

;; exwm
(straight-use-package 'bind-key)
(straight-use-package 'exwm)
(straight-use-package 'desktop-environment)
(straight-use-package 'all-the-icons)
(straight-use-package 'all-the-icons-dired)
(straight-use-package 'doom-themes)
(straight-use-package 'doom-modeline)

;; toolbox
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'aggressive-indent)
(straight-use-package 'paredit)
(straight-use-package 'crux)
(straight-use-package 'keychain-environment)
(straight-use-package 'browse-kill-ring)

;; lang
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
(bind-key* "C-S-k" 'crux-kill-whole-line)
(bind-key* "C-o" 'crux-smart-open-line)
(bind-key* "C-c a" 'abook)
(bind-key* "C-c m" 'gnus)
(bind-key* "C-c b" 'eww)
(bind-key* "C-c i" 'freenode)
(bind-key* "C-c p" 'projectile-command-map)
(bind-key* "C-c g" 'magit-status)
(bind-key* "C-c f" 'flycheck-mode)
(bind-key* "C-c t r" 'region-to-termbin)
(bind-key* "C-c t b" 'buffer-to-termbin)
(bind-key* "C-c #" 'my-su-edit)
(bind-key* "C-c $" 'my-switch-to-home)
(bind-key* "C-c I" (lambda () (interactive) (find-file user-init-file)))
(bind-key* "C-c C-l" 'crux-duplicate-current-line-or-region)
(bind-key* "C-c C-;" 'crux-duplicate-and-comment-current-line-or-region)
(bind-key* "C-;" 'comment-line)
(bind-key* "<f5>" 'compile)
(bind-key* "C-t" 'eshell)
(bind-key* "<C-tab>" 'spacemacs/alternate-buffer)
(bind-key* "C-`" 'spacemacs/alternate-window)
(bind-key* "C--" 'bury-buffer)
(bind-key "M-/" 'hippie-expand)

;; window-related binds, doubly reinforced
(exwm-input-set-key (kbd "C-t") 'eshell)
(bind-key* "C-t" 'eshell)
(exwm-input-set-key (kbd "<f9>") 'exwm-input-toggle-keyboard)
(bind-key* "<f9>" 'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "<C-tab>") 'spacemacs/alternate-buffer)
(bind-key* "<C-tab>" 'spacemacs/alternate-buffer)
(exwm-input-set-key (kbd "<C-return>") 'spacemacs/alternate-window)
(bind-key* "<C-return>" 'spacemacs/alternate-window)
(exwm-input-set-key (kbd "C-1") 'delete-other-windows)
(bind-key* "C-1" 'delete-other-windows)
(exwm-input-set-key (kbd "C-2") 'split-window-below)
(bind-key* "C-2" 'split-window-below)
(exwm-input-set-key (kbd "C-3") 'split-window-right)
(bind-key* "C-3" 'split-window-right)
(exwm-input-set-key (kbd "C-0") 'delete-window)
(bind-key* "C-0" 'delete-window)
(exwm-input-set-key (kbd "C--") 'bury-buffer)
(bind-key* "C--" 'bury-buffer)

(exwm-input-set-key (kbd "<C-up>") 'enlarge-window)
(bind-key* "<C-up>" 'enlarge-window)
(exwm-input-set-key (kbd "<C-down>") 'shrink-window)
(bind-key* "<C-down>" 'shrink-window)
(exwm-input-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
(bind-key* "<C-right>" 'enlarge-window-horizontally)
(exwm-input-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(bind-key* "<C-left>" 'shrink-window-horizontally)
(exwm-input-set-key (kbd "<menu>") 'ido-switch-buffer)
(bind-key* "<menu>" 'ido-switch-buffer)

(exwm-input-set-key (kbd "s-^") (lambda ()(interactive) (call-interactively 'open-yt-dl)))
(bind-key* "s-^" 'open-yt-dl)

(bind-key* (kbd "<C-kp-add>") (lambda () (interactive) (text-scale-adjust 1)))
(bind-key* (kbd "<C-kp-subtract>") (lambda () (interactive) (text-scale-adjust -1)))
(exwm-input-set-key (kbd "<s-kp-add>") 'desktop-environment-volume-increment)
(exwm-input-set-key (kbd "<s-kp-subtract>") 'desktop-environment-volume-decrement)
(exwm-input-set-key (kbd "<S-s-kp-add>") 'desktop-environment-brightness-increment)
(exwm-input-set-key (kbd "<S-s-kp-subtract>") 'desktop-environment-brightness-decrement)
(exwm-input-set-key (kbd "<S-s-kp-subtract>") 'desktop-environment-brightness-decrement)

;;;;theme

(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(when (not (file-exists-p (concat (getenv "HOME") "/.local/share/fonts/all-the-icons.ttf")))
  (all-the-icons-install-fonts t))

(add-hook 'after-init-hook (lambda ()
                             ;; theme
                             (blink-cursor-mode 1)
                             (display-time-mode 1)
                             (load-theme 'doom-outrun-electric t)
                             (setq doom-modeline-icon t)
                             (doom-modeline-mode 1)
                             (exwm-transparency)
                             (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(defun exwm-transparency ()
  (set-frame-parameter (selected-frame) 'alpha '(95)))
(add-hook 'exwm-workspace-switch-hook 'exwm-transparency)

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

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              tab-width 8
              fill-column 80)

;; old girl
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

(recentf-mode 1)

;;;;functions

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
   "eval $(keychain --eval --agents gpg,ssh 77CF5C5C65A8F9F44940A72CDD4795B51117D906 id_rsa); emacsclient -e '(keychain-refresh-environment)'"))

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

(defun eshell/k (&optional x y)
  "simple kiss pkg manager front-end"
  (with-temp-buffer
    (cd "/su::")
    (async-shell-command (concat "kiss " x " " y))))

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
   gnus-keep-backlog '0)

  (setq gnus-no-groups-message "")
  (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
    (with-timeout
        (3 (message "Gnus timed out."))
      ad-do-it))

  (gnus-demon-add-handler 'gnus-demon-scan-news 1 t)
  (gnus-demon-add-scanmail))

;; abook

(defvar my-abook "~/contacts.el")

(when (file-exists-p my-abook)
  (progn
    (load-file my-abook)

    ;; e.g. dummy address book
    ;; (setq my-contact-list '((name . foo@bar.email)
    ;;                         (nick . nick@nick.com)
    ;;                         (john . john@doe.com)))

    (setq-local my-contact-keys (cl-loop for (key . value) in my-contact-list
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
  (interactive)

  (require 'f)

  ;; put flag
  (f-write-text "dotfiles" 'utf-8 "~/.emacs.d/.dotfiles")

  ;; root dotfiles
  (with-temp-buffer
    (suroot)
    (shell-command "ln -sf /usr/bin/gpg2 /usr/local/bin/gpg"))

  ;; put straight pkg versions under vc
  (start-process-shell-command
   "ln" nil
   "DIR=~/.emacs.d/straight/versions ; \[ -d \"$DIR\" ] && rm -rf $DIR && ln -sf ~/repos/dotfiles/versions $DIR")

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
pinentry-program /home/adam/repos/dotfiles/pinentry-emacs")
  (f-write-text dotfiles-gnupg-gpg-agent-conf 'utf-8 "~/.gnupg/gpg-agent.conf")

  ;; xresources
  (setq dotfiles-xresources "Xft.dpi: 96
Xft.autohint: 0
Xft.antialias: 1
Xft.hinting: true
Xft.hintstyle: hintslight
Xft.rgba: rgb
Xft.lcdfilter: lcddefault")
  (f-write-text dotfiles-xresources 'utf-8 "~/.Xresources"))

;;;;the anti-desktop

(require 'exwm-config)

(defun exwm-config-default ()
  (toggle-frame-fullscreen)
  (setq exwm-input-global-keys
        `(([?\s-w] . exwm-workspace-switch)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "<s-f%d>" (1+ i))) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))))

  (setq exwm-input-simulation-keys
        '(
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ([?\C-s] . [?\C-f])))

  (setq exwm-workspace-number 1))

(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(exwm-config-default)
(add-hook 'exwm-init-hook #'exwm-config--fix/ido-buffer-window-other-frame)

(desktop-environment-mode 1)
(setq desktop-environment-brightness-set-command "lux %s"
      desktop-environment-brightness-normal-increment "-a 5%"
      desktop-environment-brightness-normal-decrement "-s 5%"
      desktop-environment-brightness-get-command "lux -G")

(defun external-browser (url)
  (start-process-shell-command "chromium" nil (concat "chromium " url)))

(setq browse-url-browser-function 'eww-browse-url
      shr-external-browser 'external-browser
      eww-search-prefix "https://www.google.com/search?hl=en&q=")

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "W") 'shr-copy-url))

(defvar yt-dl-player "mpv"
  "Video player used by `eww-open-yt-dl'")

(defun open-yt-dl ()
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
