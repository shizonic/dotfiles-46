;;; -*- lexical-binding: t; -*-
;; (use-package js2-mode :disabled
;;   ;; https://github.com/mooz/js2-mode
;;   :init (add-hook 'js-mode-hook 'js2-minor-mode))

;; (use-package js2-refactor :disabled
;;   ;; https://github.com/magnars/js2-refactor.el
;;   :init (add-hook 'js2-minor-mode-hook 'js2-refactor-mode)
;;   :config
;;   (setq js2-skip-preprocessor-directives t)
;;   (js2r-add-keybindings-with-prefix "M-m"))

;; (use-package skewer-mode :disabled
;;   ;; https://github.com/skeeto/skewer-mode
;;   :init
;;   (add-hook 'js-mode-hook 'skewer-mode)
;;   (add-hook 'css-mode-hook 'skewer-css-mode)
;;   (add-hook 'html-mode-hook 'skewer-html-mode)

;;   (add-hook 'skewer-mode-hook
;;             #'(lambda ()
;;                 (interactive)
;;                 (setq-local browse-url-browser-function 'browse-url-firefox))))



;; js-mode in Emacs 27 includes full support for syntax highlighting and indenting of JSX syntax.
;; So install js2-mode as a minor mode just for JavaScript linting...

(when (executable-find "javascript-typescript-stdio")
  (add-hook 'js-mode-hook #'lsp))

(when (executable-find "nodenv")
  (add-hook 'after-init-hook #'shim-init-node)
  (add-hook 'js-mode-hook #'shim-mode))
