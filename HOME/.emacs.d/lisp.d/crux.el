;;; -*- lexical-binding: t; -*-

(use-package crux
  :init
  (bind-key* "C-a" 'crux-move-beginning-of-line)
  (bind-key* "C-o" 'crux-smart-open-line)
  (bind-key* "C-c C-l" 'crux-duplicate-current-line-or-region)
  (bind-key* "C-c -" 'crux-kill-whole-line)
  (bind-key* "C-c ;" 'crux-duplicate-and-comment-current-line-or-region))
