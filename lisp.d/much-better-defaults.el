;; Emacs probably has the worst default settings of any program in the history of computing.

;; Allow me...

;; beef up security
(setq gnutls-verify-error t
      gnutls-min-prime-bits 2048)

(setq mouse-yank-at-point t                 ;; paste from primary at cursor point the way vim does...
      save-interprogram-paste-before-kill t ;; save X11 clipboard just incase before overwriting it inside Emacs

      ;; good stuff from technomancy's `better-defaults'
      apropos-do-all t
      require-final-newline t
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      tab-always-indent 'complete

      ;; with these settings tramp will be more reliable.
      tramp-default-method "ssh"
      tramp-copy-size-limit nil

      ;; follow symlinks to keep version control features
      vc-follow-symlinks t)

;; do not require us to type "yes" or "no" ............
(fset 'yes-or-no-p 'y-or-n-p)

;; spaces.......................
(setq-default indent-tabs-mode nil
              tab-width 8
              fill-column 80)

;; IDO is :
;; 1. built-in.
;; 2. faster (smaller) to load.
;; 3. amazingly powerful (learn it!)
;; 4. less invasive (does not take up 40% of your screen!)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess)

;; highlight matching parens
(show-paren-mode 1)

;; better dired
(require 'dired-x)
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

;; much better dired
(with-eval-after-load 'async
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

;; backspace over blocks of highlighted text like you would expect to be able to do...
(delete-selection-mode 1)

;; utf-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; C-c <left> and C-c <right> will undo/redo window layouts
(winner-mode 1)

(defadvice load-theme (before disable-themes-first activate)
  "Disable old themes before loading new themes..."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
