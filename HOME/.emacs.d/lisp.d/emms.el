;;; -*- lexical-binding: t; -*-

(use-package emms
  :init
  (emms-standard)
  (emms-default-players)
  (when (file-directory-p "~/Downloads")
    (setq emms-source-file-default-directory "~/Downloads"))

  (emms-mode-line-disable)

  (add-to-list 'emms-streams-built-in-list
               '(*track*
                 (type . streamlist)
                 (name . "http://streaming.radio.co/s75cae90f2/listen.m3u")
                 (metadata "Crusade Channel"
                           "http://streaming.radio.co/s75cae90f2/listen.m3u"
                           1 streamlist))))

(defun radio ()
  "EMMS has a lot of features, but I only use it for radio..."
  (interactive)
  (let ((choices '("emms-playlist-mode-go" "emms-streams" "emms-streams-install")))
    (message "%s" (setq-local choice (ido-completing-read "EMMS:" choices)))
    (call-interactively (intern choice))))
