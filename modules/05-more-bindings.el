;; -*- emacs-lisp -*-

;; I will not type 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off safety mode
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ido-mode is a work of beauty and magic
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
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

;; chorded backward kill magnifiers
(global-set-key [C-M-kp-delete] 'kill-paragraph)

;; prefer regexp in my backward search, inputrc-compatible binding
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; moving between windows, normalized with iTerm2 and (mod'd) tmux
(global-set-key [M-s-left]  'windmove-left)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-up]    'windmove-up)
(global-set-key [M-s-down]  'windmove-down)

;; enhanced completion library, same as inputrc binding
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "M-s-o") 'occur)

;; expand-region is super handy! I like having expand- and contract-
;; side by side within easy reach.
(require 'expand-region)
(global-set-key (kbd "s-1") 'er/expand-region)
(global-set-key (kbd "s-2") 'er/contract-region)

;; I can't get today's kids interested in set-mark, so I've repurposed
;; C-SPC for ace-jump-mode.
(require 'ace-jump-mode)
(global-set-key (kbd "C-SPC") 'ace-jump-mode)

;; Enable which-key for function discovery
(which-key-mode)

;; Multiple cursors magic
(global-set-key (kbd "C-,") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; a suggested binding from Sublime/Atom/&c
(global-set-key (kbd "s-d") 'mc/mark-all-like-this-dwim)
