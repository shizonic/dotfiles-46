;;; -*- lexical-binding: t; -*-

;; Hopefully this binding gets overwritten by a mode with a smarter jump-to-def
;; This is just here to cover all the bases.

(use-package dumb-jump
  :init
  (global-set-key (kbd "M-.") 'dumb-jump-go))
