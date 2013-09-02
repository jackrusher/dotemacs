;; -*- emacs-lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PATHS

;; place backup files here, rather than sprinkling them everywhere.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Under OSX, one must run:
;;
;; $ defaults write $HOME/.MacOSX/environment PATH "$PATH"
;;
;; ... whenever the path changes to ensure the path is availabel to
;; programs launched via Spotlight, the Dock, Finder, &c.
