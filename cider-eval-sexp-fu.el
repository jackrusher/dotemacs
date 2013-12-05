;;; cider-eval-sexp-fu.el --- Tiny functionality enhancements for evaluating sexps.

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Modified to add cider support by Sam Aaron <samaaron@gmail.com>

;; Keywords: lisp, highlight, convenience
;; Package-Requires: ((highlight "0.0.0") (smartparens "0.0.0") (thingatpt "0.0.0")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Tiny functionality enhancements for evaluating sexps.
;; This package provides:
;; - Flashing the sexps during the evaluation.
;; - `eval-last-sexp' variants (inner-list/inner-sexp).

;;; Installation:
;;
;; Put the highlight.el to your load-path.
;; Then require this package.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `cider-eval-sexp-fu-flash-mode'
;;    Toggle EvalSexpFuFlash mode on or off. If this mode is on, some `eval-last-sexp'-ish commands will highlight the sexps during evaluation.
;;  `turn-on-cider-eval-sexp-fu-flash-mode'
;;    Unequivocally turn on EvalSexpFuFlash mode
;;  `cider-eval-sexp-fu-eval-sexp-inner-list'
;;    Evaluate the list _currently_ pointed at as sexp; print value in minibuffer.
;;  `cider-eval-sexp-fu-eval-sexp-inner-sexp'
;;    Evaluate the sexp _currently_ pointed; print value in minibuffer.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `cider-eval-sexp-fu-flash-face'
;;    *Face to use for showing the sexps' overlay during the evaluation.
;;    default = (quote cider-eval-sexp-fu-flash)
;;  `cider-eval-sexp-fu-flash-error-face'
;;    *Face to use for showing the sexps' overlay if the evaluation signaled any error. The error highlighting is take into account only if non-nil value.
;;    default = (quote cider-eval-sexp-fu-flash-error)
;;  `cider-eval-sexp-fu-flash-duration'
;;    *For time duration, highlight the evaluating sexps.
;;    default = 0.15
;;  `cider-eval-sexp-fu-flash-error-duration'
;;    *For time duration, highlight the evaluating sexps signaled errors.
;;    default = 0.3
;;  `cider-eval-sexp-fu-flash-function'
;;    *Function to be used to create all of the actual flashing implementations.
;;    default = (quote cider-eval-sexp-fu-flash-default)
;;  `cider-eval-sexp-fu-flash-doit-function'
;;    *Function to use for flashing the sexps.
;;    default = (quote cider-eval-sexp-fu-flash-doit-simple)

;;; Note:
;;
;; For CIDER user, this package registers the setup clauses which set up the
;; flashers and the several interactive commands at `eval-after-load' the
;; 'cider phase. The interactive commands bellow will be defined,
;; `cider-eval-sexp-fu-cider-eval-expression-inner-list',
;; `cider-eval-sexp-fu-cider-eval-expression-inner-sexp'
;; and the pprint variants respectively.

;;; Code:

(eval-when-compile (require 'cl))
(require 'highlight)
(require 'smartparens)
(require 'thingatpt)

(defgroup cider-eval-sexp-fu nil
  "Tiny functionality enhancements for evaluating sexps."
  :prefix "cider-eval-sexp-fu-"
  :group 'cider-eval-sexp-fu)

;;; Flashing the sexps during the evaluation for just an eye candy.
(defface cider-eval-sexp-fu-flash
  '((((class color)) (:background "blue" :foreground "white" :bold t))
    (t (:inverse-video t)))
  "Face for highlighting sexps during evaluation."
  :group 'cider-eval-sexp-fu)
(defface cider-eval-sexp-fu-flash-error
  '((((class color)) (:foreground "red" :bold t))
    (t (:inverse-video t)))
  "Face for highlighting sexps signaled errors during evaluation."
  :group 'cider-eval-sexp-fu)

(defcustom cider-eval-sexp-fu-flash-face 'cider-eval-sexp-fu-flash
  "*Face to use for showing the sexps' overlay during the evaluation."
  :type 'face
  :group 'cider-eval-sexp-fu)
(defcustom cider-eval-sexp-fu-flash-error-face 'cider-eval-sexp-fu-flash-error
  "*Face to use for showing the sexps' overlay if the evaluation signaled any error. The error highlighting is take into account only if non-nil value."
  :type 'face
  :group 'cider-eval-sexp-fu)
(defcustom cider-eval-sexp-fu-flash-duration 0.15
  "*For time duration, highlight the evaluating sexps."
  :type 'number
  :group 'cider-eval-sexp-fu)
(defcustom cider-eval-sexp-fu-flash-error-duration 0.3
  "*For time duration, highlight the evaluating sexps signaled errors."
  :type 'number
  :group 'cider-eval-sexp-fu)
(defcustom cider-eval-sexp-fu-flash-function 'cider-eval-sexp-fu-flash-default
  "*Function to be used to create all of the actual flashing implementations."
  :type 'function
  :group 'cider-eval-sexp-fu)


(defun nesf-live-lisp-top-level-p ()
  "Returns true if point is not within a given form i.e. it's in
  toplevel 'whitespace'. Only works for lisp modes."
  (= 0 (car (syntax-ppss))))

(defun nesf-live-whitespace-at-point-p ()
  "Returns true if the char at point is whitespace"
  (string-match "[ \n\t]" (buffer-substring (point) (+ 1 (point)))))

(defun nesf-hl-highlight-bounds (bounds face buf)
  (with-current-buffer buf
    (hlt-highlight-region (car bounds) (cdr bounds) face)))
(defun nesf-hl-unhighlight-bounds (bounds buf)
  (with-current-buffer buf
    (hlt-unhighlight-region (car bounds) (cdr bounds))))
(defun nesf-flash-error-bounds (bounds buf face)
  (when face
    (let ((flash-error
           (lambda (bounds buf face)
             (nesf-hl-highlight-bounds bounds face buf)
             (run-at-time cider-eval-sexp-fu-flash-error-duration nil
                          'nesf-hl-unhighlight-bounds
                          bounds buf))))
      (run-with-idle-timer (max cider-eval-sexp-fu-flash-error-duration
                                cider-eval-sexp-fu-flash-duration)
                           nil flash-error
                           bounds buf face))))
(defun* cider-eval-sexp-fu-flash (bounds &optional (face cider-eval-sexp-fu-flash-face) (eface cider-eval-sexp-fu-flash-error-face))
  "BOUNS is either the cell or the function returns, such that (BEGIN . END).
Reurn the 4 values; bounds, highlighting, un-highlighting and error flashing procedure. This function is convenient to use with `define-cider-eval-sexp-fu-flash-command'."
  (flet ((bounds () (if (functionp bounds) (funcall bounds) bounds)))
    (let ((b (bounds)) (buf (current-buffer)))
      (when b
        (funcall cider-eval-sexp-fu-flash-function b face eface buf)))))
(defun cider-eval-sexp-fu-flash-default (bounds face eface buf)
  "Create all of the actual flashing implementations. See also `cider-eval-sexp-fu-flash'."
  (lexical-let ((bounds bounds) (face face) (eface eface) (buf buf))
    (values bounds
            (apply-partially 'nesf-hl-highlight-bounds bounds face buf)
            (apply-partially 'nesf-hl-unhighlight-bounds bounds buf)
            (apply-partially 'nesf-flash-error-bounds bounds buf eface))))

(defcustom cider-eval-sexp-fu-flash-doit-function 'cider-eval-sexp-fu-flash-doit-simple
  "*Function to use for flashing the sexps.

Please see the actual implementations:
- `cider-eval-sexp-fu-flash-doit-simple'
- `cider-eval-sexp-fu-flash-doit-hold-on-error'"
  :type 'function
  :group 'cider-eval-sexp-fu)
(defun cider-eval-sexp-fu-flash-doit (do-it-thunk hi unhi)
  (funcall cider-eval-sexp-fu-flash-doit-function do-it-thunk hi unhi))
(defun cider-eval-sexp-fu-flash-doit-simple (do-it-thunk hi unhi)
  (funcall hi)
  (run-at-time cider-eval-sexp-fu-flash-duration nil unhi)
  (funcall do-it-thunk))
(defun cider-eval-sexp-fu-flash-doit-hold-on-error (do-it-thunk hi unhi)
  (funcall hi)
  (unwind-protect
       (funcall do-it-thunk)
    (run-at-time cider-eval-sexp-fu-flash-duration nil unhi)))

(defmacro nesf-konstantly (v)
  `(lambda (&rest _it) ,v))
(defmacro nesf-unwind-protect-with-tracking (normallyp body unwind)
  (declare (indent 2))
  `(let (,normallyp)
     (unwind-protect
          (prog1 ,body
            (setq ,normallyp t))
       ,unwind)))
(defun nesf-flash-doit (do-it-thunk hi unhi eflash)
  (nesf-unwind-protect-with-tracking ret
      (cider-eval-sexp-fu-flash-doit do-it-thunk hi unhi)
    (unless ret
      (funcall eflash))))

;; Entry point.
(defmacro define-cider-eval-sexp-fu-flash-command (command form)
  "Install the flasher implemented as the COMMAND's around advice.

FORM is expected to return 4 values;
- A bounds (BEGIN . END) to be highlighted or nil.
- An actual highlighting procedure takes 0 arguments.
- An actual un-highliting procedure takes 0 arguments.
- An actual flashing error procedure takes 0 arguments.
See also `cider-eval-sexp-fu-flash'."
  (declare (indent 1))
  `(defadvice ,command (around cider-eval-sexp-fu-flash-region activate)
     (if cider-eval-sexp-fu-flash-mode
         (multiple-value-bind (bounds hi unhi eflash) ,form
           (if bounds
               (nesf-flash-doit (nesf-konstantly ad-do-it) hi unhi eflash)
             ad-do-it))
       ad-do-it)))
(define-minor-mode cider-eval-sexp-fu-flash-mode
    "Toggle EvalSexpFuFlash mode on or off. If this mode is on, some `eval-last-sexp'-ish commands will highlight the sexps during evaluation."
  :init-value t :global t)
(defun turn-on-cider-eval-sexp-fu-flash-mode ()
  "Unequivocally turn on EvalSexpFuFlash mode
 (see also `cider-eval-sexp-fu-flash-mode')."
  (interactive)
  (cider-eval-sexp-fu-flash-mode 1))

;;; eval-inner- stuff.
(defun nesf-funcall-and-eval-last-sexp (before eval-last-sexp)
  "Call 0 arg procedure BEFORE then call interactive command EVAL-LAST-SEXP."
  (save-excursion
    (funcall before)
    (call-interactively eval-last-sexp)))

(require 'rx)
(defun nesf-forward-inner-sexp0 ()
  (flet ((poss ()
           (let
               ((prev (save-excursion (backward-sexp) (forward-sexp) (point)))
                (next (save-excursion (forward-sexp) (backward-sexp) (point))))
             (list prev (line-number-at-pos prev)
                   next (line-number-at-pos next)
                   (point) (line-number-at-pos)))))
    (cond ((looking-at (rx (or (syntax symbol) (syntax word)
                               (syntax open-parenthesis))))
           (forward-sexp))
          (t (destructuring-bind (pp pl np nl cp cl) (poss)
               (cond ((and (<=  pp cp) (<= cp np))
                      (cond ((= pl cl) (backward-sexp))
                            ((= nl cl))
                            ((< (- cl pl) (- nl cl)) (backward-sexp))
                            ((< (- nl cl) (- cl pl)))
                            (t (backward-sexp)))
                      (forward-sexp))
                     (t (backward-sexp) (forward-sexp))))))))
(defun nesf-forward-inner-sexp ()
  (condition-case nil
      (nesf-forward-inner-sexp0)
    (scan-error nil)))
(defun nesf-backward-up-inner-list0 (steps)
  (unless steps (setq steps 1))
  (when (looking-at (rx (syntax open-parenthesis))) (decf steps))
  (dotimes (_ steps) (backward-up-list)))
(defun nesf-backward-up-inner-list (steps)
  (condition-case nil
      (nesf-backward-up-inner-list0 steps)
    (scan-error nil)))
(defun nesf-end-of-backward-up-inner-list (steps)
  (nesf-backward-up-inner-list steps)
  (nesf-forward-inner-sexp))

(defun cider-eval-sexp-fu-eval-sexp-inner-list (&optional arg)
  "Evaluate the list _currently_ pointed at as sexp; print value in minibuffer.

Interactivelly with numeric prefix argument, call to `backward-up-list' happens several times. This function is an \"Evaluate this N lists, please.\" thing."
  (interactive "P")
  (nesf-funcall-and-eval-last-sexp (apply-partially
                                   'nesf-end-of-backward-up-inner-list arg)
                                  'nesf-eval-last-sexp))
(defun cider-eval-sexp-fu-eval-sexp-inner-sexp ()
  "Evaluate the sexp _currently_ pointed; print value in minibuffer."
  (interactive)
  (nesf-funcall-and-eval-last-sexp 'nesf-forward-inner-sexp 'nesf-eval-last-sexp))

(defmacro define-nesf-eval-last-sexp-1 (command-name eval-last-sexp)
  "Define an interactive command COMMAND-NAME kind of EVAL-LAST-SEXP
such that ignores any prefix arguments."
  `(defun ,command-name ()
     (interactive)
     (let (current-prefix-arg)
       (call-interactively ',eval-last-sexp))))
(define-nesf-eval-last-sexp-1 nesf-eval-last-sexp eval-last-sexp)

;; Piece of code which defines the above inner-{sexp,list} functions.
;; This makes it possible to batch install the
;; cider-eval-sexp-fu-eval-sexp-inner-{sexp,list} with below form.
;; * (define-cider-eval-sexp-fu-eval-sexp cider-eval-sexp-fu-eval-sexp eval-last-sexp)
;; Used by making the `cider-eval-last-sexp' variant functions.
(defmacro define-nesf-eval-sexp* (eval-last-sexp inner-sexp inner-list)
  "Based on EVAL-LAST-SEXP, define INNER-SEXP and INNER-LIST interactive commands."
  (declare (indent 1))
  `(progn
     (defun ,inner-sexp ()
       (interactive)
       (nesf-funcall-and-eval-last-sexp 'nesf-forward-inner-sexp
                                       ',eval-last-sexp))
     (defun ,inner-list (&optional arg)
       (interactive "P")
       (nesf-funcall-and-eval-last-sexp (apply-partially
                                        'nesf-end-of-backward-up-inner-list arg)
                                       ',eval-last-sexp))))
(defmacro define-cider-eval-sexp-fu-eval-sexp (command-name-prefix eval-last-sexp)
  "Define -inner-sexp and -inner-list interactive commands prefixed by COMMAND-NAME-PREFIX based on EVAL-LAST-SEXP. Actual work is done by `define-nesf-eval-sexp*'."
  (let ((nesf-eval-last-sexp-1
         (intern (format "nesf-%s-1" (symbol-name eval-last-sexp)))))
    `(progn
       (define-nesf-eval-last-sexp-1 ,nesf-eval-last-sexp-1 ,eval-last-sexp)
       (define-nesf-eval-sexp* ,nesf-eval-last-sexp-1
         ,@(mapcar (lambda (post)
                     (intern (concat (symbol-name command-name-prefix) post)))
                   '("-inner-sexp" "-inner-list"))))))

(defun nesf-end-of-buffer-p ()
  "Predicate fn to determine whether point is at the end of the
   buffer"
  (<= (buffer-size) (point)))

(defun nesf-sp-forward-down ()
  "Doesn't freeze Emacs if attempted to be called at end of
   buffer. Otherwise similar to sp-down-sexp."
  (interactive)
  (if (save-excursion
          (forward-comment (buffer-size))
          (not (nesf-end-of-buffer-p)))
      (sp-down-sexp)
    (error "unexpected end of buffer")))

;; bug#13952
(if (version<= "24.3.1" emacs-version)
    (defmacro with-nesf-end-of-sexp (&rest body)
      (declare (indent 0))
      `(progn ,@body))
  (defmacro with-nesf-end-of-sexp (&rest body)
    (declare (indent 0))
    `(save-excursion
       (backward-char)
       ,@body)))

;;; initialize.
(defun nesf-initialize ()
  (define-cider-eval-sexp-fu-flash-command eval-last-sexp
    (cider-eval-sexp-fu-flash (with-nesf-end-of-sexp
                               (bounds-of-thing-at-point 'sexp))))
  (define-cider-eval-sexp-fu-flash-command eval-defun
    (cider-eval-sexp-fu-flash (save-excursion
                                (end-of-defun)
                                (beginning-of-defun)
                                (bounds-of-thing-at-point 'sexp))))
  (eval-after-load 'eev
    '(progn
       ;; `eek-eval-last-sexp' is defined in eev.el.
       (define-cider-eval-sexp-fu-flash-command eek-eval-last-sexp
         (cider-eval-sexp-fu-flash (cons (save-excursion (eek-backward-sexp))
                                         (point)))))))

(defun nesf-initialize-cider ()
  (define-cider-eval-sexp-fu-flash-command cider-eval-last-sexp
    (cider-eval-sexp-fu-flash (when (not (bolp ))
                                (with-nesf-end-of-sexp
                                  (save-excursion
                                    (skip-chars-backward " \t")
                                    (bounds-of-thing-at-point 'sexp))))))
  (define-cider-eval-sexp-fu-flash-command cider-pprint-eval-last-sexp
    (cider-eval-sexp-fu-flash (when (not (bolp))
                                (with-nesf-end-of-sexp
                                  (save-excursion
                                    (skip-chars-backward " \t")
                                    (bounds-of-thing-at-point 'sexp))))))
  (define-cider-eval-sexp-fu-flash-command cider-eval-defun-at-point
    (cider-eval-sexp-fu-flash  (with-nesf-end-of-sexp
                                 (when (not (and (nesf-live-lisp-top-level-p)
                                                 (save-excursion
                                                   (ignore-errors (forward-char))
                                                   (nesf-live-lisp-top-level-p))
                                                 (nesf-live-whitespace-at-point-p)
                                                 (not (save-excursion (sp-up-sexp)))))
                                   (save-excursion
                                     (save-match-data
                                       (while (sp-up-sexp))
                                       (if (nesf-live-whitespace-at-point-p)
                                           (let ((end (point)))
                                             (backward-sexp)
                                             (cons (point) end))
                                         (bounds-of-thing-at-point 'sexp))))))))

  (progn
    ;; Defines:
    ;; `cider-eval-sexp-fu-cider-eval-expression-inner-list',
    ;; `cider-eval-sexp-fu-cider-eval-expression-inner-sexp'
    ;; and the pprint variants respectively.
    (define-cider-eval-sexp-fu-eval-sexp cider-eval-sexp-fu-cider-eval-expression
      cider-eval-last-sexp)
    (define-cider-eval-sexp-fu-eval-sexp cider-eval-sexp-fu-cider-pprint-eval-expression
      cider-pprint-eval-last-sexp)))

(eval-when (load eval)
  (nesf-initialize)
  (eval-after-load 'cider
    '(nesf-initialize-cider)))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "nesf-forward-inner-sexp0")
      (expect ?p
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "s+exp")
          (goto-char (point-min))
          (nesf-forward-inner-sexp0)
          (char-before)))
      (expect ?p
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "s+exp")
          (goto-char (1+ (point-min)))
          (nesf-forward-inner-sexp0)
          (char-before)))
      (expect ?\)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "s(exp)")
          (goto-char (1+ (point-min)))
          (nesf-forward-inner-sexp0)
          (char-before)))
      (desc "nesf-forward-inner-sexp0 same line, but far near the next")
      ;; Always previous, is this OK?
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0   sexp1")
          (goto-char (+ (point-min) 7))
          (nesf-forward-inner-sexp0)
          (char-before)))
      (desc "nesf-forward-inner-sexp0 across lines")
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (nesf-forward-inner-sexp0)
          (char-before)))
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (forward-line)
          (nesf-forward-inner-sexp0)
          (char-before)))
      (expect ?1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (forward-line 3)
          (nesf-forward-inner-sexp0)
          (char-before)))
      (expect ?1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (forward-line 3)
          (nesf-forward-inner-sexp0)
          (char-before)))
      (expect ?1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (forward-line 4)
          (nesf-forward-inner-sexp0)
          (char-before)))
      (desc "nesf-forward-inner-sexp0 across lines (equal delta)")
      ;; Always previous lines', is this OK?
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n\n\nsexp1")
          (goto-char (point-min))
          (forward-line 2)
          (nesf-forward-inner-sexp0)
          (char-before)))
      (desc "nesf-forward-inner-sexp0 no more")
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "sexp0\n\n")
          (goto-char (point-max))
          (nesf-forward-inner-sexp0)
          (char-before)))
      (desc "nesf-forward-inner-sexp0 no less")
      (expect ?0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\n\nsexp0")
          (goto-char (point-min))
          (nesf-forward-inner-sexp0)
          (char-before)))
      (desc "nesf-forward-inner-sexp0 no any")
      (expect 5
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\n\n\n\n")
          (goto-char (point-min))
          (nesf-forward-inner-sexp0)
          (point)))
      )))

(provide 'cider-eval-sexp-fu)
;;; cider-eval-sexp-fu.el ends here
