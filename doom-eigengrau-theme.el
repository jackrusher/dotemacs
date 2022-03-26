;;; doom-eigengrau.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2022 Jack Rusher
;;
;; Author: Jack Rusher <https://github.com/jackrusher>
;; Created: March 19, 2022
;; Version: 0.0.1
;; Keywords: custom themes, faces
;; Homepage: https://github.com/hlissner/emacs-doom-themes
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; Somewhat inspired by Noctilux and LightTable.
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-eigengrau-theme nil
  "Options for the `doom-eigengrau' theme."
  :group 'doom-themes)

(defcustom doom-eigengrau-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-eigengrau-theme
  :type 'boolean)

(defcustom doom-eigengrau-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-eigengrau-theme
  :type 'boolean)

(defcustom doom-eigengrau-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-eigengrau-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-eigengrau
  "A dark theme somewhat inspired by Noctilux and Light Table."

  ;; name        default   256           16
  ((bg         '("#16161D" "black"       "black"  )) ; the hex code for actual eigengrau
   (fg         '("#bbc2cf" "#bfbfbf"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#21242b" "black"       "black"        ))
   (fg-alt     '("#5B6268" "#2d2d2d"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#1B2229" "black"       "black"        ))
   (base1      '("#1c1f24" "#1e1e1e"     "brightblack"  ))
   (base2      '("#202328" "#2e2e2e"     "brightblack"  ))
   (base3      '("#23272e" "#262626"     "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f"     "brightblack"  ))
   (base5      '("#5B6268" "#525252"     "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b"     "brightblack"  ))
   (base7      '("#9ca0a4" "#979797"     "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf"     "white"        ))

   (grey       base4)
   (red        '("#ff463b" "#ff463b" "red"          ))
   (orange     '("#DADA86" "#DADA86" "brightred"    )) ; actually yellow
   (green      '("#86DA87" "#86DA87" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#86DAB1" "#86DAB1" "yellow"       )) ; not yellow at all :)
   (blue       '("#86AEDA" "#86AEDA" "brightblue"   ))
   (dark-blue  '("#597492" "#597492" "blue"         )) ; darker "#43576d" ?
   (magenta    '("#DB85D9" "#DB85D9" "brightmagenta"))
   (violet     '("#B286DB" "#B286DB" "magenta"      ))
   (cyan       '("#86D9DB" "#86D9DB" "brightcyan"   ))
   (dark-cyan  '("#64a2a4" "#64a2a4" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-eigengrau-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-eigengrau-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      teal) 
   (keywords       green)
   (methods        cyan)
   (operators      green)
   (type           yellow)
   (strings        blue)
   (variables      teal)
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-eigengrau-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-eigengrau-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-eigengrau-padded-modeline
      (if (integerp doom-eigengrau-padded-modeline) doom-eigengrau-padded-modeline 4))))

  ;;;; Base theme face overrides
  (((font-lock-variable-name-face &override) :slant 'normal)
   ((font-lock-doc-face &override) :slant 'italic)
   
   ((eval-sexp-fu-flash &override)  :background "#0A414C" :foreground "#84FBFF")
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-eigengrau-brighter-comments (doom-lighten bg 0.05)))

   ;; calm the parens a bit
   ((show-paren-match &override) :foreground bg :background comments :weight 'ultra-bold)
   ((show-paren-mismatch &override) :foreground nil :background "red")

   ((paren-face &override) :foreground comments)
   ((paren-face-match &override) :foreground "#666" :background bg :weight 'ultra-bold)

   ;; subtle mode-line
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-eigengrau-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-eigengrau-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; doom-eigengrau-theme.el ends here

;; ((clojure-keyword &override) :foreground yellow) ;; Carper's extended clojure syntax table
;; ((clojure-braces &override) :foreground violet)
;; ((clojure-brackets &override) :foreground yellow)
;; ((clojure-special &override) :foreground blue)
;; ((clojure-java-call &override) :foreground violet)
