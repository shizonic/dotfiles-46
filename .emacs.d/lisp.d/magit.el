;;; -*- lexical-binding: t; -*-

(use-package magit
  :demand
  :init
  (global-set-key (kbd "C-c g") 'magit-status)
  (setq magit-diff-refine-hunk t)
  (setq magit-repository-directories '(("~/kiss" . 1)
                                       ("~/repos" . 1))))

(use-package forge
  :demand
  :after magit)
