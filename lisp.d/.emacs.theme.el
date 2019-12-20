;;; -*- lexical-binding: t; -*-

;; misc
(global-prettify-symbols-mode 1)
(fringe-mode 1)
(menu-bar-mode 1)
(tool-bar-mode 1)
(scroll-bar-mode 1)
(set-window-scroll-bars (minibuffer-window) nil nil)

;; theme

;; disable old theme before enabling a new theme
(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(load-theme 'nofrils-sepia t)

;; font

(setq my-font "Liberation Mono")
(setq my-font-size '15)

(my-set-font my-font my-font-size)

;; redshift

(setq redshift '3000)
(my-set-redshift redshift)

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
