;;; -*- lexical-binding: t; -*-

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
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

(setq password-cache-expiry nil
      epa-pinentry-mode 'loopback)

(fset 'yes-or-no-p 'y-or-n-p)

(winner-mode 1)

(require 'eww)

(defun my-external-browser (url)
  (start-process-shell-command "chromium" nil (concat "chromium " url)))

(setq browse-url-browser-function 'eww-browse-url
      shr-external-browser 'my-external-browser
      eww-search-prefix "https://www.google.com/search?hl=en&q=")

(defvar yt-dl-player "mpv"
  "Video player used by `eww-open-yt-dl'")

(emms-standard)
(emms-default-players)
(when (file-directory-p "~/Downloads")
  (setq emms-source-file-default-directory "~/Downloads"))
