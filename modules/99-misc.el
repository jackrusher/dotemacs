;; -*- emacs-lisp -*-

;;; DATA MUNGING

(require 's)

(defun reformat-field (str)
  "Trims leading/trailing whitespace from `str`, then converts it
to a number if the string looks like a number."
  (let ((s (s-trim str)))
    (if (and (string-match "[\$\.,0-9]+" s) (= (match-end 0) (length s)))
        (string-to-number (replace-regexp-in-string "[\$,]" "" s))
      s)))

(defun tsv-to-sexp (tsv)
  "Parses the string `tsv` as a tab-separated-value file,
returning a sexp containing the values with strings converted to
numbers where appropriate."
  (-map (lambda (s) (-map 'reformat-field (s-split "\t" s))) (s-lines tsv)))

;;; MISC HELPER FUNCTIONS

;;; some of the keycodes produced by OSX are inscrutable
(defun insert-key-sequence (key-repr)
  "Reads a literal key sequence from the user and inserts a
representation of it at point, suitable for `global-set-key' or
`local-set-key'."
  (interactive "KKey sequence? ")
  (prin1 key-repr (current-buffer))
  (insert " "))

;;; emacs doesn't include a function to rename a buffer and file at
;;; the same time by default, so here's one.
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
