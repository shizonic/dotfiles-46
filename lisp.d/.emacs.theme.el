;;; -*- lexical-binding: t; -*-

;; misc

(fringe-mode -1)
(blink-cursor-mode 1)

;; font

(setq my-font "Liberation Mono")
(setq my-font-size '10)

(my-set-font my-font my-font-size)

;; redshift

(setq redshift '3000)
(my-set-redshift redshift)

;; theme

;; (defadvice load-theme (before disable-themes-first activate)
;;   (dolist (i custom-enabled-themes)
;;     (disable-theme i)))
;; (load-theme 'nofrils-acme t)

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
