;;; -*- lexical-binding: t; -*-

(setq-default indent-tabs-mode nil
              tab-width 8
              fill-column 80)

(use-package aggressive-indent
  :init
  (aggressive-indent-global-mode 1))

(use-package editorconfig
  :init
  (editorconfig-mode 1))
