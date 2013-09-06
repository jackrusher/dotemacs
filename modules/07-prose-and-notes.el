;; -*- emacs-lisp -*-

(require 'markdown-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\(md\\|markdown\\)$" . markdown-mode) auto-mode-alist)

;; GitHub-flavoured markdown for what look like GH README files
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; I prefer my navigation keys be left alone, please.
(eval-after-load 'markdown-mode
  (dolist (binding (list (kbd "M-<up>") (kbd "M-<down>") (kbd "M-<left>") (kbd "M-<right>")))
    (define-key markdown-mode-map binding nil)))

;; I like to date my diary entries in this format
(defun insert-current-date ()
  "Insert a YYYY-MM-DD representation of the current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%m" (current-time))))

;;; I wrote a little minor mode for word count because it helps me to
;;; hit targets while writing.
(define-minor-mode wordcount-mode
  "Toggle word-count mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the word count for the current buffer
is displayed in the mode-line."
  :init-value nil
  :lighter (:eval (format " [%d words]" (count-words (point-min) (point-max)))))

(add-hook 'markdown-mode-hook (lambda () (wordcount-mode)))

