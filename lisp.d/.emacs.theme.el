;;; -*- lexical-binding: t; -*-

;; misc
(global-prettify-symbols-mode 1)
(menu-bar-mode -1)

;; theme

;; disable old theme before enabling a new theme
(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; modeline

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(setq-default mode-line-format
              '((:eval (simple-mode-line-render
                        ;; left
                        (format-mode-line "%* %b %l:%c %m")
                        ;; right
                        (format-mode-line (concat
                                           (format-time-string " %I:%M%p")))))))
