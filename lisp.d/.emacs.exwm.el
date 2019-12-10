;;; -*- lexical-binding: t; -*-

(use-package exwm
  :init (use-package xelb)
  :config
  (with-eval-after-load 'exwm-config-default
    ;; Set the initial workspace number.
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 1))
    ;; Make class name the buffer name
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (exwm-workspace-rename-buffer exwm-class-name))))
  (exwm-enable)
  (exwm-config-ido))

(use-package desktop-environment
  :config
  (desktop-environment-mode 1)
  (setq desktop-environment-brightness-set-command "lux.sh %s"
        desktop-environment-brightness-normal-increment "-a 5%"
        desktop-environment-brightness-normal-decrement "-s 5%"
        desktop-environment-brightness-get-command "lux.sh -G"))
