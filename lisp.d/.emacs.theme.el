(setq my-font "Noto Sans Mono")
(setq my-font-size '10)
(setq display-time-default-load-average nil)
(display-time-mode 1)
(blink-cursor-mode 1)

(defun my-set-font (my-font my-font-size)
  (set-face-attribute 'default nil :font (concat my-font "-" (number-to-string my-font-size))))

(defun my-font-resizer (x)
  (if (> x 0)
      (setq-local n '1)
    (setq-local n -1))
  (setq my-font-size (+ n my-font-size))
  (my-set-font my-font my-font-size))

(my-set-font my-font my-font-size)
(global-set-key (kbd "<C-kp-add>") (lambda()(interactive)(my-font-resizer 1)))
(global-set-key (kbd "<C-kp-subtract>") (lambda()(interactive)(my-font-resizer -1)))

(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(use-package all-the-icons :defer t
  :init (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
          (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired :defer t
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package doom-modeline
      :config
      (doom-modeline-mode 1))

(use-package plan9-theme
  :config
  (load-theme 'plan9 t))
