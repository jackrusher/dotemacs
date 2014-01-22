;; -*- emacs-lisp -*-

(require 'package)

;; more (and more up-to-date) packages than plain ELPA
(mapc (apply-partially #'add-to-list 'package-archives)
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(let ((needed-packages
       (delq nil (mapcar (lambda (p) (when (not (package-installed-p p)) p))
                         '(ac-geiser ac-js2 ac-nrepl ac-slime ace-jump-mode ack-and-a-half auto-complete caml cider clojure-mode clojurescript-mode coffee-mode color-theme dash diminish elisp-slime-nav epl expand-region find-file-in-project flymake-coffee flymake-cursor flymake-easy flymake-ruby fringe-helper geiser git-commit-mode git-rebase-mode go-mode haml-mode haskell-mode highlight ido-ubiquitous inf-ruby js2-mode js2-refactor json-mode magit markdown-mode motion-mode multiple-cursors nrepl-eval-sexp-fu paredit parenface-plus pkg-info popup powerline pretty-symbols processing-mode rainbow-mode restclient robe s simple-httpd skewer-mode slime slime-repl slime-ritz smartparens smex tuareg undo-tree visual-regexp yaml-mode yasnippet)))))
  (when needed-packages
    (message "Refreshing the package database...")
    (package-refresh-contents)
    (message "Installing packages...")
    (mapc 'package-install needed-packages)
    (message "Package installation complete.")))

(defun currently-installed-packages-list ()
  (sort (copy-sequence (mapcar 'car package-alist)) 'string<))

;;(currently-installed-packages-list)



