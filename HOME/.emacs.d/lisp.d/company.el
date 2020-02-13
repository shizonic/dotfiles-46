;;; -*- lexical-binding: t; -*-

(use-package company
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-show-numbers t
        company-idle-delay 0.0
        company-selection-wrap-around t
        company-tooltip-align-annotations t))
