;;; -*- lexical-binding: t; -*-

(setq gnutls-verify-error t
      gnutls-min-prime-bits 2048

      epg-gpg-program "gpg2"
      password-cache-expiry nil

      mouse-yank-at-point t
      save-interprogram-paste-before-kill t

      apropos-do-all t
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      tab-always-indent 'complete

      tramp-default-method "ssh"
      tramp-copy-size-limit nil

      vc-follow-symlinks t)

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

(with-eval-after-load 'async
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(defun dired-xdg-open-file ()
  "from https://www.emacswiki.org/emacs/OperatingOnFilesInDired"
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c !") 'dired-xdg-open-file))

(setq browse-url-browser-function 'eww-browse-url
      shr-external-browser 'browse-url-chromium ;; inside `eww', press the "&" key to launch page in external browser...
      eww-search-prefix "https://www.google.com/search?hl=en&q=")

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "^") 'eww-open-yt-dl) ;; inside `eww' press ^ to open the url with youtube-dl
  (define-key eww-mode-map (kbd "W") 'shr-copy-url)) ;; "w" by default copies current page URL, while "W" now will copy url at point.

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

(delete-selection-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(winner-mode 1)

(defadvice load-theme (before disable-themes-first activate)
  "Disable old themes before loading new themes..."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(xterm-mouse-mode 1)
