;; A working gpg configuration in Emacs is not trivial...
;; Documenting my gpg setup here.
;; Basically requires gpg2, Funtoo "keychain", a custom "pinentry-emacs" script, some helper functions, some emacs settings, and a ~/.gnupg/gpg-agent.conf file

;; this is a helpful variable for people to inspect while debugging [inevitable] gpg issues...
(setq-default epg-gpg-program "gpg") ;; must be actually gpg2 or a symlink to gpg2

;; i need these for it all to work
(setq password-cache-expiry nil
      epa-pinentry-mode 'loopback)

;; This works together with the helper script located in bin/pinentry-emacs
(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

;; We want to refresh the environment of Emacs to inform it of keychain actions...
;; `keychain-environemnt' does the job and is used in the `unlock' and `lock' functions below...
(straight-use-package 'keychain-environment)

;; The following two functions require the funtoo keychain script be on PATH. Most distros have this script installable as a package.
;; It will unlock or lock your gpg/ssh keyring basically. Don't forget to update to public gpg hash to use your own.
(defun unlock ()
  "Requires the keychain script be on PATH"
  (interactive)
  (async-shell-command
   "eval $(keychain --eval --agents gpg,ssh 77CF5C5C65A8F9F44940A72CDD4795B51117D906 id_rsa); emacsclient -e '(keychain-refresh-environment)'"))

(defun lock ()
  "Requires the keychain script be on PATH."
  (interactive)
  (async-shell-command "keychain --agents ssh,gpg -k all"))

;; Here's what my ~/.gnupg/gpg-agent.conf looks like
;; Note the bin/pinentry-emacs script is required for this all to work.
;; And this version of the pinentry-emacs script is different than the mainstream one...
(setq dotfiles-gnupg-gpg-agent-conf (concat  "default-cache-ttl 84000
max-cache-ttl 84000
allow-emacs-pinentry
allow-loopback-pinentry
pinentry-program /home/"user-login-name"/bin/pinentry-emacs"))

(defun gpg-dotfile-install ()
  "Create ~/.gnupg/gpg-agent.conf"
  (f-write-text dotfiles-gnupg-gpg-agent-conf 'utf-8 "~/.gnupg/gpg-agent.conf"))
