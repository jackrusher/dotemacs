;; -*- emacs-lisp -*-

;; enable sRGB colors in the Cocoa version of emacs
(setq ns-use-srgb-colorspace t)

(setq
 ;; display line & column numbers in mode-line
 line-number-mode t
 column-number-mode t

 ;; speed up screen re-paint on local sessions
 redisplay-dont-pause t 

 ;; general look and feel things
 font-lock-maximum-decoration t
 color-theme-is-global t
 visible-bell nil
 truncate-partial-width-windows nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; put the current filename and path in the frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; font and spacing
(set-frame-font "Menlo-14")
(setq-default line-spacing 4)

;; color emoji support
(if (fboundp 'set-fontset-font)
	(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; unblinking bar-style cursor
(blink-cursor-mode 0)
(setq default-cursor-type 'bar)

;; fancy lambda, &c
(global-prettify-symbols-mode 1)
(mapc (lambda (m) (add-hook m (lambda () (push '("fn" . ?ƒ) prettify-symbols-alist))))
      '(clojure-mode-hook clojurescript-mode-hook))

;; dim the parentheses
(require 'paren-face)

(require 'doom-themes)
(require 'doom-modeline)
(load-theme 'doom-eigengrau t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Global settings (defaults)
(setq doom-themes-enable-bold t     ; if nil, bold is universally disabled
      doom-themes-enable-italic t)  ; if nil, italics is universally disabled

(doom-modeline-mode t)

;; customize company-mode's popup
;; (let ((bg (face-attribute 'default :background)))
;;     (custom-set-faces
;;      `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;      `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;      `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;      `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
;;      `(company-tooltip-common-selection ((t (:inherit font-lock-keyword-face))))
;;      `(company-tooltip-selection ((t (:inherit font-lock-keyword-face))))))

