;; the best theme is actually the default one

(setq display-time-default-load-average nil
      display-time-day-and-date t)
(display-time-mode 1)

(blink-cursor-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)

(setq my-font "Noto Sans Mono")
(setq my-font-size '10)

(defun my-font-resizer (x)
  (when (> x 0)
    (progn
      (setq my-font-size (+ 1 my-font-size))
      (set-face-attribute 'default nil
                          :font (concat my-font "-" (number-to-string my-font-size)))))
  (when (< x 0)
    (progn
      (setq my-font-size (+ -1 my-font-size))
      (set-face-attribute 'default nil
                          :font (concat my-font "-" (number-to-string my-font-size)))))
  (when (eq x 0)
    (progn
      (set-face-attribute 'default nil
                          :font (concat my-font "-" (number-to-string my-font-size)))))
  (message (concat my-font "-" (number-to-string my-font-size))))

(my-font-resizer 0)

(bind-key* "<C-kp-add>" #'(lambda ()
                            (interactive)
                            (my-font-resizer 1)))

(bind-key* "<C-kp-subtract>" #'(lambda ()
                                 (interactive)
                                 (my-font-resizer -1)))
