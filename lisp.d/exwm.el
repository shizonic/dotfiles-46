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

(add-hook 'exwm-init-hook #'exwm-config--fix/ido-buffer-window-other-frame)

(desktop-environment-mode 1)

(setq desktop-environment-brightness-set-command "lux %s"
      desktop-environment-brightness-normal-increment "-a 5%"
      desktop-environment-brightness-normal-decrement "-s 5%"
      desktop-environment-brightness-get-command "lux -G")

;; tip, install perlfile-mimeinfo, which xdg-open falls back to using when not in a full desktop-environment!
(defun dired-xdg-open-file ()
  "from https://www.emacswiki.org/emacs/OperatingOnFilesInDired"
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

;; hit C-c ! to use xdg-open when opening files from dired
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c !") 'dired-xdg-open-file))

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "^") 'eww-open-yt-dl)
  (define-key eww-mode-map (kbd "W") 'shr-copy-url))

(defvar yt-dl-player "mpv"
  "Video player used by `eww-open-yt-dl'")

(defun external-browser (url)
  (start-process-shell-command my-external-browser nil (concat my-external-browser " " url)))

(setq browse-url-browser-function 'eww-browse-url
      shr-external-browser 'external-browser
      my-external-browser "surf"
      eww-search-prefix "https://www.google.com/search?hl=en&q=")

(defun open-yt-dl ()
  "Browse youtube videos using the Emacs `eww' browser and \"youtube-dl.\"
Specify the video player to use by setting the value of `yt-dl-player'"
  (interactive)
  (when (executable-find "youtube-dl")
    (progn
      (if (string-match  "*eww*" (format "%s"(current-buffer)))
          (eww-copy-page-url)
        (with-temp-buffer (yank)))
      (start-process-shell-command "youtube-dl" nil
                                   (concat "youtube-dl -o - " (nth 0 kill-ring) " - | " yt-dl-player " -")))))


;; I dislike multi-monitor setups...
;; However sometimes I plug my laptop into a larger external monitor...
;; So I want it to autodetect and switch to the external display...
;; This is how I accomplish this using with exwm:

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
  (if (string-match (concat external " connected")
                    (shell-command-to-string "xrandr"))
      (switch-to-external-monitor)
    (switch-to-internal-monitor)))

(require 'exwm-randr)

(add-hook 'exwm-randr-screen-change-hook 'xrandr)

(exwm-randr-enable)

;;EOF
(exwm-enable) ;; leave me here
