;;; -*- lexical-binding: t; -*-

(use-package exwm
  :init (use-package xelb)
  :config
  (require 'exwm-config)
  (defun exwm-config-default ()
    ;; Set the initial workspace number.
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 1))
    ;; Make class name the buffer name
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (exwm-workspace-rename-buffer exwm-class-name)))
    ;; Line-editing shortcuts
    (unless (get 'exwm-input-simulation-keys 'saved-value)
      (setq exwm-input-simulation-keys
            '(
              ([?\C-b] . [left])
              ([?\M-b] . [C-left])
              ([?\C-f] . [right])
              ([?\M-f] . [C-right])
              ([?\C-p] . [up])
              ([?\C-n] . [down])
              ([?\C-a] . [home])
              ([?\C-e] . [end])
              ([?\M-v] . [prior])
              ([?\C-v] . [next])
              ([?\C-d] . [delete])
              ([?\C-k] . [S-end delete])
              ([?\C-w] . [?\C-x])
              ([?\M-w] . [?\C-c])
              ([?\C-y] . [?\C-v])
              ([?\C-s] . [?\C-f]))))
    (exwm-enable)
    (exwm-config-ido))
  (exwm-config-default))

(use-package desktop-environment
  :config
  (desktop-environment-mode 1)
  (setq desktop-environment-brightness-set-command "lux.sh %s"
        desktop-environment-brightness-normal-increment "-a 5%"
        desktop-environment-brightness-normal-decrement "-s 5%"
        desktop-environment-brightness-get-command "lux.sh -G")

  (exwm-input-set-key
   (kbd "<s-kp-multiply>") 'desktop-environment-toggle-mute)
  (exwm-input-set-key
   (kbd "<s-kp-add>") 'desktop-environment-volume-increment)
  (exwm-input-set-key
   (kbd "<s-kp-subtract>") 'desktop-environment-volume-decrement))
