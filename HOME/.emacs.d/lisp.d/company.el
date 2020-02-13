;;; -*- lexical-binding: t; -*-

(use-package company
  :init (global-company-mode 1)
  :config
  (setq company-minimum-prefix-length 1
        company-show-numbers t
        company-idle-delay 0.0
        company-selection-wrap-around t
        company-tooltip-align-annotations t))
