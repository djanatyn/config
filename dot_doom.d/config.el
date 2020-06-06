(after! org
  (setq org-agenda-files
        '("~/repos/org")))

(after! term
  (setq multi-term-program "/bin/bash"))

(setq doom-line-numbers-style 'relative)

;; theming
(require 'leuven-theme)
(setq doom-theme 'leuven)

;; use terminus font
(setq doom-font "xft:-xos4-Terminess Powerline-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")

;; transparent background
(doom/set-frame-opacity 90)

;; symon
(require 'symon)
(symon-mode)

;; ormolu
(set-formatter! 'ormolu "ormolu" :modes '(haskell-mode))

;; dante
(setq dante-methods '(stack snack nix))

;; set ssh to gpg agent
(defun fix-ssh ()
  (interactive)
  (setenv "SSH_AUTH_SOCK" (s-chomp (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket"))))
(fix-ssh)

;; elfeed
(setq rmh-elfeed-org-files (list "~/.feeds.org"))
