;; disable emacs-nox menu bar
(menu-bar-mode -1)

;; minimalist modeline
(setq-default mode-line-format
              '((:eval (format-mode-line "%* %b %l:%c"))))

;; disable color completely
(setq-default default-frame-alist '((tty-color-mode . never)))

;; highlight functions using [only] bold.
(custom-set-faces '(font-lock-function-name-face ((t (:weight bold)))))

;; true spartans should also uncomment the next line:
;; (global-font-lock-mode -1)
