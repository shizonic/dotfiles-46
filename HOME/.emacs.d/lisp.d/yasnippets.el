;;; -*- lexical-binding: t; -*-

(use-package yasnippet :demand :after company
  :preface (defvar tmp/company-point nil)
  :config
  (yas-global-mode 1)

  ;; Use TAB to expand snippets. The code snippet below also avoids clashing with company-mode.
  ;; https://github.com/ianpan870102/yay-evil-emacs/blob/master/config.org
  (advice-add 'company-complete-common
              :before
              #'(lambda ()
                  (setq tmp/company-point (point))))
  (advice-add 'company-complete-common
              :after
              #'(lambda ()
                  (when (equal tmp/company-point (point))
                    (yas-expand))))


  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or
         (not company-mode/enable-yas)
         (and (listp backend)
              (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package yasnippet-snippets)
