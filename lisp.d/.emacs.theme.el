;;; -*- lexical-binding: t; -*-

(setq my-font "Noto Sans Mono")
(setq my-font-size '10)
(setq display-time-default-load-average nil)
(display-time-mode 1)
(blink-cursor-mode 1)

(setq redshift '1000)

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

(global-set-key (kbd "M-+") (lambda()(interactive)(my-redshift-setter 1)))
(global-set-key (kbd "M--") (lambda()(interactive)(my-redshift-setter -1)))
(global-set-key (kbd "<M-kp-multiply>") (lambda()(interactive)
                                          (start-process-shell-command "redshift" nil "redshift -x")))

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

(use-package plan9-theme :disabled
  :config
  (load-theme 'plan9 t))
