;;; -*- lexical-binding: t; -*-

;; disable the old theme before loading a new theme
(defadvice load-theme (before disable-themes-first activate)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(and window-system
     (progn
       ;; gui

       (use-package doom-themes :disabled
         :init
         (load-theme 'doom-outrun-electric t))

       (use-package doom-modeline
         :init
         (setq doom-modeline-icon t)
         (doom-modeline-mode 1))

       (use-package all-the-icons
         :config
         (when (not (file-exists-p (concat (getenv "HOME") "/.local/share/fonts/all-the-icons.ttf")))
           (all-the-icons-install-fonts t)))

       (use-package all-the-icons-dired
         :init
         (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

       (setq display-time-default-load-average nil
             display-time-day-and-date t)
       (display-time-mode 1)

       (blink-cursor-mode 1)
       (tool-bar-mode -1)
       (menu-bar-mode -1)
       (scroll-bar-mode -1)
       (fringe-mode -1)

       (setq my-font "Liberation Mono")
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

       (my-font-resizer 0))

     (with-eval-after-load 'exwm

       (defun exwm-transparency ()
         (set-frame-parameter (selected-frame) 'alpha '(80)))
       (add-hook 'exwm-workspace-switch-hook 'exwm-transparency)
       (exwm-transparency)

       (exwm-input-set-key (kbd "<C-kp-add>") #'(lambda ()
                                                  (interactive)
                                                  (my-font-resizer 1)))

       (exwm-input-set-key (kbd "<C-kp-subtract>") #'(lambda ()
                                                       (interactive)
                                                       (my-font-resizer -1)))
       (exwm-input-set-key (kbd "C-=") #'(lambda ()
                                           (interactive)
                                           (my-font-resizer 1)))

       (exwm-input-set-key (kbd "C--") #'(lambda ()
                                           (interactive)
                                           (my-font-resizer -1)))))

(or window-system
    ;; nox
    (progn
      (menu-bar-mode -1)
      (setq-default default-frame-alist '((tty-color-mode . never)))
      (custom-set-faces '(font-lock-function-name-face ((t (:weight bold)))))))
