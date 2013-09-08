;; -*- emacs-lisp -*-

(require 'package)

;; more (and more up-to-date) packages than plain ELPA
(mapc (apply-partially #'add-to-list 'package-archives)
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(let ((needed-packages
       (delq nil (mapcar (lambda (p) (when (not (package-installed-p p)) p))
                         '(ac-js2 ac-nrepl ac-slime ace-jump-mode ack-and-a-half auto-complete caml cl-lib clojure-mode clojurescript-mode coffee-mode color-theme css-mode dash diminish elisp-slime-nav expand-region find-file-in-project flymake-cursor flymake-easy flymake-ruby fringe-helper geiser git-commit-mode git-rebase-mode go-mode haml-mode haskell-mode highlight ido-ubiquitous inf-ruby js2-mode js2-refactor json-mode magit markdown-mode motion-mode multiple-cursors nrepl nrepl-eval-sexp-fu nrepl-ritz paredit pkg-info popup powerline pretty-symbols-mode processing-mode robe s simple-httpd skewer-mode slime slime-repl slime-ritz smex tuareg undo-tree visual-regexp yaml-mode yasnippet)))))
  (when needed-packages
    (message "Refreshing the package database...")
    (package-refresh-contents)
    (message "Installing packages...")
    (mapc 'package-install needed-packages)
    (message "Package installation complete.")))

;; return a formatted list of packages to insert into the above code
;; whenever I change this configuration.
(defun currently-installed-packages-list ()
  (sort (copy-sequence (mapcar 'car package-alist)) 'string<))

;;(currently-installed-packages-list)

