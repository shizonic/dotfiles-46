;;; -*- lexical-binding: t; -*-

;; misc
(fringe-mode -1)
(blink-cursor-mode -1)

;; font

(setq my-font "Liberation Mono")
(setq my-font-size '18)

(defun my-set-font (my-font my-font-size)
  (set-face-attribute 'default nil :font (concat my-font "-" (number-to-string my-font-size))))

(defun my-font-resizer (x)
  (if (> x 0)
      (setq-local n '1)
    (setq-local n -1))
  (when (< my-font-size 11)
      (setq my-font-size 11))
  (when (> my-font-size 24)
      (setq my-font-size 24))
  (setq my-font-size (+ n my-font-size))
  (my-set-font my-font my-font-size)
  (message (concat "Font Size: " (number-to-string my-font-size))))

(my-set-font my-font my-font-size)

;; redshift

(setq redshift '3000)

(defun my-set-redshift (redshift)
  (start-process-shell-command "redshift" nil (concat "redshift -x && redshift -O " (number-to-string redshift))))

(defun my-redshift-setter (x)
  (if (> x 0)
      (setq-local n '250)
    (setq-local n '-250))
  (setq redshift (+ n redshift))
  (if (< redshift 1000)
      (setq redshift '1000))
  (if (> redshift 25000)
      (setq redshift '25000))
  (my-set-redshift redshift))

;; theme

(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(use-package nofrils-acme-theme :disabled
  :config
  (load-theme 'nofrils-acme t))

(use-package cyberpunk-theme
  :config
  (load-theme 'cyberpunk t))

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
