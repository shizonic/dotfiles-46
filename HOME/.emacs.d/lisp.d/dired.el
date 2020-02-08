;;; -*- lexical-binding: t; -*-

(require 'dired-x)
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(defun dired-xdg-open-file ()
  "from https://www.emacswiki.org/emacs/OperatingOnFilesInDired"
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(with-eval-after-load 'async
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c !") 'dired-xdg-open-file))
