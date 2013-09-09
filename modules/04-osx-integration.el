;; -*- emacs-lisp -*-

;; use OS X's Spotlight for M-x locate
(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

;; alt-click for mouse-2, command-click for mouse-3
;; ...is this broken?
(setq mac-emulate-three-button-mouse t)

;; shift-select and delete-selection are standard is OS X inputs
(setq shift-select-mode t) 
(delete-selection-mode t)

;;;; Normalize key bindings with Mac OS X system ones

;; command + up/down/left/right = file start/end, line start/end
(global-set-key (kbd "<s-up>")    'beginning-of-buffer)
(global-set-key (kbd "<s-down>")  'end-of-buffer)
(global-set-key (kbd "<s-left>")  'move-beginning-of-line)
(global-set-key (kbd "<s-right>") 'move-end-of-line)

(define-key global-map (kbd "s-+") 'text-scale-increase)
(define-key global-map (kbd "s--") 'text-scale-decrease)

;; fn+option+delete = kill word to the right in OS X inputs
(define-key global-map (kbd "<M-kp-delete>") 'paredit-forward-kill-word)

;; ESC is normally used to begin certain control sequences in emacs,
;; but universally used for 'quit' by everything else. This normalizes
;; with everything else.
(defun jackrusher-escape-quit ()
  "Quit the current action, a la C-g, and unhighlight any
lingering search matches."
  (interactive)
  (keyboard-escape-quit)
  (lazy-highlight-cleanup))
(global-set-key (kbd "<escape>") 'jackrusher-escape-quit)

;; undo-tree-mode aliased to command+z/shift+command+z
(require 'undo-tree)
(global-undo-tree-mode 1)
(global-set-key (kbd "s-z") 'undo)           
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;; the fantastic undo-tree-visualize on C-s-z
(global-set-key [C-s-268632090] 'undo-tree-visualize)

;; command-f, the default OSX search keybinding, but with regexp
(global-set-key (kbd "s-f") 'isearch-forward-regexp)

;; command-r, forward-replace
(require 'visual-regexp)
(global-set-key (kbd "s-r") 'vr/replace)
(global-set-key [8388690] 'vr/query-replace) ; s-R

;; make M-up and M-down the same as C-up and C-down because the former
;; is how it's bound in OSX
(global-set-key (kbd "<M-up>") 'backward-paragraph)
(global-set-key (kbd "<M-down>") 'forward-paragraph)

;; option-delete = backword-kill-word in OS X
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)

;; I like a single frame, so I'd rather have this kill the buffer
(global-set-key (kbd "s-w") 'kill-this-buffer)

;; I never want to see the OS file finder
(require 'find-file-in-project)
(global-set-key (kbd "s-o") 'find-file-in-project)

;; don't use the right alt/option key as M-, so it can still be used
;; to type accented characters. Fück yeah.
(setq ns-right-alternate-modifier nil)
