;; -*- emacs-lisp -*-

;; turn off splash screen messages
(setq inhibit-startup-echo-area-message t
      inhibit-startup-screen t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/"))
(when (version< emacs-version "27.0") (package-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (typescript-mode dired-details applescript-mode company-pollen pollen-mode edn gnuplot which-key org-babel-eval-in-repl htmlize ox-reveal ttl-mode sparql-mode yaml-mode visual-regexp undo-tree tuareg smex smartparens slime-company skewer-mode robe restclient rainbow-mode processing-mode pretty-symbols powerline parenface-plus paredit pallet motion-mode markdown-mode magit json-mode js2-refactor ido-ubiquitous haml-mode go-mode geiser fringe-helper flymake-ruby flymake-coffee find-file-in-project expand-region elisp-slime-nav diminish company-quickhelp company-inf-ruby company-ghc color-theme coffee-mode cider-eval-sexp-fu cider bundler aggressive-indent ag ack-and-a-half ace-jump-mode)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#2f2f2f"))))
 '(company-scrollbar-fg ((t (:background "#222222"))))
 '(company-tooltip ((t (:inherit default :background "#1b1b1b"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-common-selection ((t (:inherit font-lock-keyword-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-keyword-face)))))
(put 'narrow-to-region 'disabled nil)
