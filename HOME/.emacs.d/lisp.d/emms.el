;;; -*- lexical-binding: t; -*-

(use-package emms
  :init
  (emms-standard)
  (emms-default-players)
  (when (file-directory-p "~/Downloads")
    (setq emms-source-file-default-directory "~/Downloads"))

  (emms-mode-line-disable))

(defun my-emms ()
  "EMMS has a lot of features, but these are the only ones I use..."
  (interactive)
  (let ((choices '("emms-playlist-mode-go" "emms-play-file" "emms-streams")))
    (message "%s" (setq-local choice (ido-completing-read "EMMS:" choices)))
    (call-interactively (intern choice))))
