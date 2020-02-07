;;; -*- lexical-binding: t; -*-

(use-package magit
  :init
  (global-set-key (kbd "C-c g") 'magit-status)
  (setq magit-diff-refine-hunk t)
  (setq magit-repository-directories '(("~/repos" . 1))))
