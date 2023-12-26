;; -*- emacs-lisp -*- -*- lexical-binding: t; -*-

;;; utf-8 all the time
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)

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

(put 'narrow-to-region 'disabled nil)

;; I'll be sending files from the command line
(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode which-key web-mode visual-regexp uuid use-package undo-fu typo tuareg swift-mode smex smartparens slime-company simple-httpd rustic robe restclient rainbow-mode pretty-symbols pkg-info paren-face paredit ox-reveal magit lsp-mode julia-mode json-mode indium ido-grid-mode ido-completing-read+ htmlize highlight haskell-mode haml-mode geiser-racket geiser-kawa geiser-chez fringe-helper flymake-ruby flymake-cursor find-file-in-project expand-region elisp-slime-nav doom-themes doom-modeline diminish company-pollen company-inf-ruby company-go cider-eval-sexp-fu cider cargo bundler atomic-chrome aggressive-indent ag ace-jump-mode ac-geiser)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
