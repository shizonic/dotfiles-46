;;; -*- lexical-binding: t; -*-

(use-package diminish)

(use-package spaceline :demand
  :config
  (require 'spaceline-config)
  (setq spaceline-version-control-p nil)
  (spaceline-spacemacs-theme))
