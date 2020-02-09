;;; -*- lexical-binding: t; -*-

(use-package company
  :diminish
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-show-numbers t
        company-idle-delay 0.0
        company-selection-wrap-around t
        company-tooltip-align-annotations t))
