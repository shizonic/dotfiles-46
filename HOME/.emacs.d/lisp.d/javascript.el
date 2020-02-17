;;; -*- lexical-binding: t; -*-

(use-package simple-httpd)

;; js-mode in Emacs 27 includes full support for syntax highlighting and indenting of JSX syntax.
;; So install js2-mode as a minor mode just for JavaScript linting...

(use-package js2-mode
  ;; https://github.com/mooz/js2-mode
  :init (add-hook 'js-mode-hook 'js2-minor-mode))

(use-package js2-refactor
  ;; https://github.com/magnars/js2-refactor.el
  :init (add-hook 'js2-minor-mode-hook 'js2-refactor-mode)
  :config
  (setq js2-skip-preprocessor-directives t)
  (js2r-add-keybindings-with-prefix "M-m"))

(use-package skewer-mode
  ;; https://github.com/skeeto/skewer-mode
  :init
  (add-hook 'js-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode)

  (add-hook 'skewer-mode-hook
            #'(lambda ()
                (interactive)
                (setq-local browse-url-browser-function 'browse-url-firefox))))

(when (executable-find "javascript-typescript-stdio")
  (add-hook 'js-mode-hook 'eglot-ensure))
