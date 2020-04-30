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

;; fix ssh inside tmux
(defun fix-ssh ()
  (interactive)
  (let ((cmd "tmux show-environment | grep -oP '(?<=^SSH_AUTH_SOCK=).+$'")
        (strip (lambda (t) replace-regexp-in-string "\n\\'" "" t)))
    (setenv "SSH_AUTH_SOCK" (apply strip (shell-command-to-string cmd)))))

;; elfeed
(setq rmh-elfeed-org-files (list "~/.feeds.org"))
