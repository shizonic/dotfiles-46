;;; -*- lexical-binding: t; -*-

(require 'exwm-config)
(defun exwm-config-default ()
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
            ([?\C-s] . [?\C-f])))))
;; Set the initial workspace number.
(unless (get 'exwm-workspace-number 'saved-value)
  (setq exwm-workspace-number 1))
;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))
(exwm-config-default)
(exwm-enable)
(exwm-config-ido)

(desktop-environment-mode 1)
(setq desktop-environment-brightness-set-command "lux.sh %s"
       desktop-environment-brightness-normal-increment "-a 5%"
       desktop-environment-brightness-normal-decrement "-s 5%"
       desktop-environment-brightness-get-command "lux.sh -G")
