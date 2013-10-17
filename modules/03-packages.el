;; -*- emacs-lisp -*-

(require 'package)

;; more (and more up-to-date) packages than plain ELPA
(mapc (apply-partially #'add-to-list 'package-archives)
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(let ((needed-packages
       (delq nil (mapcar (lambda (p) (when (not (package-installed-p p)) p))
                         '(ac-js2 ac-nrepl ac-slime ace-jump-mode ack-and-a-half auto-complete caml clojure-mode clojurescript-mode coffee-mode color-theme color-theme-sanityinc-tomorrow css-mode dash diminish elisp-slime-nav expand-region find-file-in-project flymake-cursor flymake-easy flymake-ruby fringe-helper geiser git-commit-mode git-rebase-mode go-mode haml-mode haskell-mode highlight ido-ubiquitous inf-ruby js2-mode js2-refactor json-mode magit markdown-mode motion-mode multiple-cursors nrepl nrepl-eval-sexp-fu nrepl-ritz parenface-plus pkg-info popup powerline pretty-symbols-mode processing-mode rainbow-mode restclient robe s simple-httpd skewer-mode slime slime-repl slime-ritz smartparens smex tuareg undo-tree visual-regexp yaml-mode yasnippet)))))
  (when needed-packages
    (message "Refreshing the package database...")
    (package-refresh-contents)
    (message "Installing packages...")
    (mapc 'package-install needed-packages)
    (message "Package installation complete.")))

(defun currently-installed-packages-list ()
  (sort (copy-sequence (mapcar 'car package-alist)) 'string<))

;;(currently-installed-packages-list)

