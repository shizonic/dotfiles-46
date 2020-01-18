;;;;startx.el -- kind of bonkers, but "it works" ...

(defun sx ()
  "I don't use this currently, keeping this here.
A simple elisp replacement for startx/xinit scripts.
Requires subr-x and async."
  (interactive)

  (setenv "DISPLAY" ":0")
  (start-process "Xorg" nil "Xorg" "-nolisten" "tcp" "-nolisten" "local" ":0" "vt1" "v" "-arinterval" "30" "-ardelay" "175")

  (async-start
   (lambda ()
     (while (not (string-match-p "XFree86_has_VT"
                                 (shell-command-to-string "xprop -root")))
       (sleep-for 0.5)))
   (lambda (result)

     ;; touchpad
     (start-process-shell-command
      "xinput" nil "touchpad=\"$(xinput list | awk '/TouchPad/ { print $7 }')\" ; xinput set-prop \"${touchpad#id=}\" 'libinput Tapping Enabled' 1 ; xinput set-prop \"${touchpad#id=}\" 'libinput Accel Speed' 0.4")

     ;; Xresources
     (start-process "xsetroot" nil "xsetroot" "-cursor_name" "left_ptr")
     (start-process "xrdb" nil "xrdb" (concat (getenv "HOME") "/.Xresources"))

     ;; Exwm xrandr - auto move display to external monitor on plug
     (with-eval-after-load 'exwm
       (defvar external "VGA-1")
       (defvar internal "LVDS-1")

       (defun switch-to-external-monitor ()
         (start-process
          "xrandr" nil
          "xrandr" "--output" internal "--off" "--output" external "--auto"))

       (defun  switch-to-internal-monitor ()
         (start-process
          "xrandr" nil
          "xrandr" "--output" external "--off" "--output" internal "--auto"))

       (defun xrandr ()
         ;; HACK when external is already plugged
         (when (and (string-match (concat external " connected")
                                  (shell-command-to-string "xrandr"))
                    (< (string-to-number (emacs-uptime "%s")) 10))
           (switch-to-internal-monitor))

         (if (string-match (concat external " connected")
                           (shell-command-to-string "xrandr"))
             (switch-to-external-monitor)
           (switch-to-internal-monitor)))

       (require 'exwm-randr)
       (add-hook 'exwm-randr-screen-change-hook 'xrandr)
       (exwm-randr-enable)

       (exwm-enable))

     ;; Exwm
     (start-process
      "emacsclient" nil "emacsclient" "-c" "-e" "(eshell)" "-e" "(set-frame-font \"unifont-12\" nil t)"))))
