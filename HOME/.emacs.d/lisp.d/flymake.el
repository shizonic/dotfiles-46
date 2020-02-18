;;; -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook #'(lambda ()
                              (flymake-mode 1)))
