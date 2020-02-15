(use-package exwm :demand
  :config
  (require 'exwm-config)

  (defun exwm-config-default ()
    "Overrides the default exwm-config-default function with my preferences..."
    (setq exwm-input-global-keys
          `(([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))

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
            ([?\C-s] . [?\C-f])))

    (setq exwm-workspace-number 1))

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (exwm-config-default)

  ;; of course we are using ido because it is the best, so we need this.
  (add-hook 'exwm-init-hook #'exwm-config--fix/ido-buffer-window-other-frame)

  ;; Exwm xrandr - auto move display to external monitor on plug
  (defvar external "VGA-1")
  (defvar internal "LVDS-1")

  (defun set-wallpaper ()
    (shell-command "feh --no-fehbg --bg-max ~/.wallpaper"))

  (defun switch-to-external-monitor ()
    (start-process
     "xrandr" nil
     "xrandr" "--output" internal "--off" "--output" external "--auto")
    (set-wallpaper))

  (defun  switch-to-internal-monitor ()
    (start-process
     "xrandr" nil
     "xrandr" "--output" external "--off" "--output" internal "--auto")
    (set-wallpaper))

  (defun xrandr ()
    (if (string-match (concat external " connected")
                      (shell-command-to-string "xrandr"))
        (switch-to-external-monitor)
      (switch-to-internal-monitor)))

  (require 'exwm-randr)
  (add-hook 'exwm-randr-screen-change-hook 'xrandr)
  (exwm-randr-enable)

  ;; last thing
  (exwm-enable))

(use-package desktop-environment :demand :after exwm
  :config
  (desktop-environment-mode 1)
  ;; This will use alsautils to adjust volume on keyboard volume keys and
  ;; lux to adjust brightness with keyboard brightness keys
  (setq desktop-environment-brightness-set-command "lux %s"
        desktop-environment-brightness-normal-increment "-a 5%"
        desktop-environment-brightness-normal-decrement "-s 5%"
        desktop-environment-brightness-get-command "lux -G")

  (exwm-input-set-key (kbd "<XF86AudioMute>") 'desktop-environment-toggle-mute)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'desktop-environment-volume-decrement)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'desktop-environment-volume-increment)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>")'desktop-environment-brightness-decrement)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>")'desktop-environment-brightness-increment)

  (exwm-input-set-key (kbd "<s-kp-add>") 'desktop-environment-volume-increment)
  (exwm-input-set-key (kbd "<s-kp-subtract>") 'desktop-environment-volume-decrement)

  (exwm-input-set-key (kbd "<f9>") 'exwm-input-toggle-keyboard)

  (exwm-input-set-key (kbd "<XF86PowerOff>") #'(lambda ()
                                                 (interactive)
                                                 (with-temp-buffer
                                                   (cd "/su::")
                                                   (shell-command "echo mem > /sys/power/state")))))
