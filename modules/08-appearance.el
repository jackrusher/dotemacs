;; -*- emacs-lisp -*-

(setq
 ;; display line & column numbers in mode-line
 line-number-mode t
 column-number-mode t

 ;; speed up screen re-paint on local sessions
 redisplay-dont-pause t 

 ;; general look and feel things
 font-lock-maximum-decoration t
 color-theme-is-global t
 visible-bell t
 truncate-partial-width-windows nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; put the current filename and path in the frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; font and spacing
(set-default-font "Menlo-13")
(setq-default line-spacing 3)

;; unblinking bar-style cursor
(blink-cursor-mode 0)
(setq default-cursor-type 'bar)

;; fancy lambda, &c
(global-prettify-symbols-mode 1)
(mapc (lambda (m) (add-hook m (lambda () (push '("fn" . ?ƒ) prettify-symbols-alist))))
      '(clojure-mode-hook clojurescript-mode-hook))

;; dim the parentheses
(require 'parenface-plus)
(set-face-foreground 'paren-face "#666")

(eval-after-load 'undo-tree
  '(diminish 'undo-tree-mode))

;; locally hacked version of noctilux, turning into something else
(add-to-list 'load-path (concat user-emacs-directory "eigengrau/"))
(require 'eigengrau-theme)

;; powerline gives a much aesthetically improved mode line
(require 'powerline)
(powerline-center-theme)

;; TODO customize ac-complete for color theme
;(set-face-background 'ac-candidate-face "#366060")
;(set-face-foreground 'ac-selection-face "#1f1f1f")
;(set-face-background 'ac-selection-face "#8cd0d3")
;(set-face-foreground 'ac-selection-face "#1f1f1f")
