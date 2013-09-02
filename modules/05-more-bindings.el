;; -*- emacs-lisp -*-

;; I will not type 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off safety mode
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ido-mode is a work of beauty and magic
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; smex is "smart M-x"
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq smex-key-advice-ignore-menu-bar t)

;; nicer buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; chorded backward kill magnifiers
(global-set-key (kbd "C-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-paragraph)

;; prefer regexp in my backward search, inputrc-compatible binding
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; moving between windows, normalized with iTerm2 and (mod'd) tmux
(global-set-key [M-s-left]  'windmove-left)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-up]    'windmove-up)
(global-set-key [M-s-down]  'windmove-down)

;; enhanced completion library, same as inputrc binding
(global-set-key (kbd "M-/") 'hippie-expand)

;; expand-region is super handy! I like having expand- and contract-
;; side by side within easy reach.
(require 'expand-region)
(global-set-key (kbd "s-1") 'er/expand-region)
(global-set-key (kbd "s-2") 'er/contract-region)

(require 'ace-jump-mode)
(global-set-key (kbd "C-SPC") 'ace-jump-mode)

;; ;; Keep region when undoing in region (from Magnar)
;; (defadvice undo-tree-undo (around keep-region activate)
;;   (if (use-region-p)
;;       (let ((m (set-marker (make-marker) (mark)))
;;             (p (set-marker (make-marker) (point))))
;;         ad-do-it
;;         (goto-char p)
;;         (set-mark m)
;;         (set-marker p nil)
;;         (set-marker m nil))
;;     ad-do-it))
