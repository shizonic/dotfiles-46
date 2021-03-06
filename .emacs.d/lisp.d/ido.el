;;; -*- lexical-binding: t; -*-

(require 'ido)

(ido-mode 1)

(ido-everywhere 1)

(setq ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess)

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config
  (setq flx-ido-threshold 10000))

(use-package ido-completing-read+ :init (ido-ubiquitous-mode +1))

(use-package amx :init (amx-mode 1))

(setq magit-completing-read-function 'magit-ido-completing-read)
(setq gnus-completing-read-function 'gnus-ido-completing-read)
