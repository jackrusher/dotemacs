;; -*- emacs-lisp -*-

;; turn off splash screen messages
(setq inhibit-startup-echo-area-message t
      inhibit-startup-screen t)

;; no window chrome!
(custom-set-variables
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

;; I have loads of RAM, don't start GC until I've used a decent chunk
;; of it.
(setq gc-cons-threshold 20000000)

;;; utf-8 all the time
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)

;; I'll be sending files from the command line
(server-start)

;;; local lisp packages for this configuration live here
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; Like /etc/rc.d, all startup filenames begin with a number and get
;;; loaded in numerical order.
(mapc #'load-file (directory-files (concat user-emacs-directory "modules") t "[0-9]*.el$"))

;;; PER-USER CUSTOMIZATIONS

;; Here's a hook to load "~/.emacs.d/lisp/<username>-local.el" if such
;; a file exists on the load path.  Place personal extensions to this
;; configuration in that file to avoid merge hassles later.
(let ((per-user-file (concat (user-login-name) "-local.el")))
  (when (locate-library per-user-file) (load per-user-file)))

;; TODO bring in latex customizations from old .emacs

;; TODO bring in org-mode customizations from old .emacs, and add some

;; Mac OS X capture mode scripting for grabbing bits and pieces from
;; outside emacs (via Automator).

;;(setq org-default-notes-file (concat org-directory "/notes.org"))
;;(define-key global-map "<s-E>" 'org-capture)

;; TODO bring in more web development stuff
