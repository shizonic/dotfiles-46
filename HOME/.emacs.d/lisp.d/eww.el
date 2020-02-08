;;; -*- lexical-binding: t; -*-

(defvar yt-dl-player "mpv"
  "Video player used by `eww-open-yt-dl'")

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

(setq browse-url-browser-function 'eww-browse-url)

(if (executable-find "chromium")
    (setq browse-url-secondary-browser-function 'browse-url-chromium)
  (setq browse-url-secondary-browser-function 'browse-url-firefox))

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "^") 'eww-open-yt-dl) ;; inside `eww' press ^ to open the url with youtube-dl
  (define-key eww-mode-map (kbd "W") 'shr-copy-url));; "w" by default copies current page URL, while "W" now will copy url at point.
