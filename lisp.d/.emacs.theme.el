;;; -*- lexical-binding: t; -*-
(setq my-theme "light")

;; font

(setq my-font "Noto Sans Mono")
(setq my-font-size '10)

(defun my-set-font (my-font my-font-size)
  (set-face-attribute 'default nil :font (concat my-font "-" (number-to-string my-font-size))))

(defun my-font-resizer (x)
  (if (> x 0)
      (setq-local n '1)
    (setq-local n -1))
  (when (< my-font-size 10)
      (setq my-font-size 10))
  (when (> my-font-size 25)
      (setq my-font-size 25))
  (setq my-font-size (+ n my-font-size))
  (my-set-font my-font my-font-size))

(my-set-font my-font my-font-size)

;; redshift

(setq redshift '1000)
(start-process-shell-command "redshift" nil (concat "redshift -x && redshift -O " (number-to-string redshift)))

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

(use-package nofrils-acme-theme
  :if (string= my-theme "light")
  :config
  (load-theme 'nofrils-sepia t))

(use-package sexy-monochrome-theme
  :if (string= my-theme "dark")
  :config
  (load-theme 'sexy-monochrome t))

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

;; misc

(blink-cursor-mode -1)
(fringe-mode -1)
