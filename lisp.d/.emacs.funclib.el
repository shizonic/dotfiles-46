;;; -*- lexical-binding: t; -*-

;; Elisp Function Library
;; handy functions I have adopted or made up to do my bidding

(require 'cl-lib)

(defun read-lines (FILEPATH)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents FILEPATH)
    (split-string (buffer-string) "\n" t)))

(defun spacemacs/alternate-buffer (&optional window)
  "switch buffer back-and-forth"
  (interactive)
  (let ((current-buffer (window-buffer window)))
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name   (car (last (split-string parent "/" t)))))
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))


(defvar yt-dl-player "mpv"
  "Video player used by `eww-open-yt-dl'")

(defun eww-open-yt-dl ()
  "Browse youtube videos using the Emacs `eww' browser and \"youtube-dl.\"
Specify the video player to use by setting the value of `yt-dl-player'"
  (interactive)
  (if (executable-find "youtube-dl")
      (progn
        (eww-copy-page-url)
        (start-process-shell-command "youtube-dl" nil
                                     (concat "youtube-dl -o - " (nth 0 kill-ring) " - | " yt-dl-player " -")))
    (progn
      (setq xbuff (generate-new-buffer "*youtube-dl not found*"))
      (with-output-to-temp-buffer xbuff
        (print "Ensure youtube-dl is installed on the system and try again...")))))

(with-eval-after-load 'eww (define-key eww-mode-map (kbd "^") 'eww-open-yt-dl))

;; abook

(setq my-contacts-file "~/contacts.el")
(when (file-exists-p my-contacts-file)
  (progn
    (load-file my-contacts-file)

    ;; e.g. dummy address book (key . value) list
    ;; (setq my-contact-list '((name . foo@bar.email)
    ;;                         (nick . nick@nick.com)
    ;;                         (john . john@doe.com)))

    (setq my-contact-keys (cl-loop for (key . value) in my-contact-list
                                   collect key))

    (defun abook ()
      "Insert an email address from `my-contact-list' to the current buffer."
      (interactive)
      (let ((item my-contact-keys))
        (fset 'my-read 'completing-read)

        ;; interactive menu + convert chosen item (key) from string to data
        (setq interactive-chosen-key (intern (my-read "Contact Name:" item)))
        ;; match key to list and get associated email (value), convert back to string
        (setq email (format "%s" (cdr (assq interactive-chosen-key my-contact-list))))

        ;; output email address to buffer
        (princ email (current-buffer))))))


;; transpose frame

(defun transpose-frame-get-arrangement (&optional frame subtree)
  (let ((tree (or subtree
                  (car (window-tree frame)))))
    (if (windowp tree)
        (list (window-buffer tree)
              (window-start tree)
              (window-point tree)
              (window-hscroll tree)
              (window-margins tree)
              (window-fringes tree)
              (window-dedicated-p tree)
              (window-redisplay-end-trigger tree)
              tree
              (eq tree (frame-selected-window frame)))
      (let* ((vertical (car tree))
             (edges (cadr tree))
             (length (float (if vertical
                                (- (nth 3 edges) (cadr edges))
                              (- (nth 2 edges) (car edges))))))
        (cons vertical
              (mapcar (lambda (subtree)
                        (cons (transpose-frame-get-arrangement frame subtree)
                              (/ (let ((edges (if (windowp subtree)
                                                  (window-edges subtree)
                                                (cadr subtree))))
                                   (if vertical
                                       (- (nth 3 edges) (cadr edges))
                                     (- (nth 2 edges) (car edges))))
                                 length)))
                      (cddr tree)))))))

(defun transpose-frame-set-arrangement (config &optional window-or-frame &rest how)
  (let ((window (if (windowp window-or-frame)
                    window-or-frame
                  (frame-selected-window window-or-frame))))
    (unless (windowp window-or-frame)
      (delete-other-windows window))
    (if (bufferp (car config))
        (let ((buffer (pop config)))
          (set-window-buffer window buffer)
          (set-window-start window (pop config))
          (set-window-point window (pop config))
          (set-window-hscroll window (pop config))
          (set-window-margins window (caar config) (cdr (pop config)))
          (apply 'set-window-fringes window (pop config))
          (set-window-dedicated-p window (pop config))
          (set-window-redisplay-end-trigger window (pop config))
          (let ((orig-window (pop config))
                (ol-func (lambda (ol)
                           (if (eq (overlay-get ol 'window) orig-window)
                               (overlay-put ol 'window window))))
                (ol-lists (with-current-buffer buffer
                            (overlay-lists))))
            (mapc ol-func (car ol-lists))
            (mapc ol-func (cdr ol-lists)))
          (if (car config) (select-window window)))
      (let* ((horizontal (if (memq 'transpose how)
                             (pop config)
                           (not (pop config))))
             (edges (window-edges window))
             (length (if horizontal
                         (- (nth 2 edges) (car edges))
                       (- (nth 3 edges) (cadr edges)))))
        (if (memq (if horizontal 'flop 'flip) how)
            (setq config (reverse config)))
        (while (cdr config)
          (setq window (prog1
                           (split-window window (round (* length (cdar config)))
                                         horizontal)
                         (apply 'transpose-frame-set-arrangement
                                (caar config) window how))
                config (cdr config)))
        (apply 'transpose-frame-set-arrangement
               (caar config) window how)))))

(defun transpose-frame (&optional frame)
  "Transpose windows arrangement at FRAME.
Omitting FRAME means currently selected frame."
  (interactive)
  (transpose-frame-set-arrangement (transpose-frame-get-arrangement frame) frame
                                   'transpose)
  (if (interactive-p) (recenter)))

(defun flip-frame (&optional frame)
  "Flip windows arrangement vertically at FRAME.
Omitting FRAME means currently selected frame."
  (interactive)
  (transpose-frame-set-arrangement (transpose-frame-get-arrangement frame) frame
                                   'flip))

(defun flop-frame (&optional frame)
  "Flop windows arrangement horizontally at FRAME.
Omitting FRAME means currently selected frame."
  (interactive)
  (transpose-frame-set-arrangement (transpose-frame-get-arrangement frame) frame
                                   'flop))

(defun rotate-frame (&optional frame)
  "Rotate windows arrangement 180 degrees at FRAME.
Omitting FRAME means currently selected frame."
  (interactive)
  (transpose-frame-set-arrangement (transpose-frame-get-arrangement frame) frame
                                   'flip 'flop))

(defun rotate-frame-clockwise (&optional frame)
  "Rotate windows arrangement 90 degrees clockwise at FRAME.
Omitting FRAME means currently selected frame."
  (interactive)
  (transpose-frame-set-arrangement (transpose-frame-get-arrangement frame) frame
                                   'transpose 'flop)
  (if (interactive-p) (recenter)))

(defun rotate-frame-anticlockwise (&optional frame)
  "Rotate windows arrangement 90 degrees anti-clockwise at FRAME.
Omitting FRAME means currently selected frame."
  (interactive)
  (transpose-frame-set-arrangement (transpose-frame-get-arrangement frame) frame
                                   'transpose 'flip)
  (if (interactive-p) (recenter)))
