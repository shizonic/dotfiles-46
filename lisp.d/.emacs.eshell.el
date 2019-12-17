;;; -*- lexical-binding: t; -*-

;; environment variables

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" (getenv "EDITOR"))

(setenv "CC" "x86_64-pc-linux-musl-gcc -static")
(setenv "MAKEFLAGS" "-j5")
(setenv "CFLAGS" "-O3 -pipe")
(setenv "CXXFLAGS" "-O3 -pipe")

;; PATH

(setq my-path-inherited (getenv "PATH"))

(setq my-path-insert (concat
                      "/home/" user-login-name "/bin:"
                      "/home/" user-login-name "/.local/bin:"
                      "/sucks/coreutils/bin:"
                      "/sucks/misc/bin:"
                      "/rocks/bin:"))

(setq my-path-append ":/foo/bar")

(setenv "PATH"
        (string-join
         (setq my-path
               (delete-dups (split-string-by-delim
                             (setenv "PATH" (concat
                                             my-path-insert
                                             my-path-inherited
                                             my-path-append)) ":")))":"))

(setq my-path (concat "PATH=" (getenv "PATH")))

(setq my-sync-root-path t) ;; keep root's PATH in sync with user/Emacs

(defun root-path ()
  (interactive)
  (when (bound-and-true-p my-sync-root-path)
      (f-write-text my-path 'utf-8 "/su:root@kiss:/root/.profile")))

;; eshell alias / functions

(defun eshell/troot ()
 (cd (concat "/su:root@"system-name":")))

(defun eshell/emacs (file)
  (find-file file))

;; MISC

;; make scripts executeable automatically
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; a shell-mode hook
(defun my-shell-mode-hook()
  (setq-local compile-command
              '((lambda()
                  (save-buffer)
                  (async-shell-command (buffer-file-name))))))
(add-hook 'sh-mode-hook 'my-shell-mode-hook)
