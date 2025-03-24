;; -*- emacs-lisp -*- -*- lexical-binding: t; -*-

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org"          . "http://orgmode.org/elpa/"))) ;; no https :(

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(eval-and-compile
 (require 'use-package))

;;; Packages we require
(use-package ace-jump-mode)
(use-package ag)
(use-package aggressive-indent)
(use-package all-the-icons)
(use-package bundler)
(use-package caml)

;; clojure stuff
(use-package clojure-mode)
(use-package cider :pin melpa-stable)
(use-package cider-eval-sexp-fu)

;; in-buffer completion framework
 (use-package company
    :ensure t
    :commands (global-company-mode)
    :init
    (global-company-mode)
    :custom
    (company-tooltip-align-annotations 't)
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.1))

;;(use-package company-ghc)
(use-package company-inf-ruby)
(use-package company-go)

(use-package dash)
(use-package diminish)
(use-package doom-themes)
(use-package doom-modeline)
(use-package elisp-slime-nav)
(use-package expand-region)
(use-package find-file-in-project)
(use-package flymake-cursor)
(use-package flymake-easy)
(use-package flymake-ruby)
(use-package fringe-helper)
(use-package geiser)
(use-package go-mode)
(use-package haml-mode)
(use-package haskell-mode)
(use-package highlight)
(use-package ido-completing-read+)
(use-package ido-grid-mode)
(use-package inf-ruby)
(use-package js2-mode)
(use-package js2-refactor)
(use-package json-mode)
(use-package magit
  :custom ((magit-diff-refine-hunk 'all)
           (magit-process-finish-apply-ansi-colors t)))
(use-package markdown-mode)
(use-package multiple-cursors)
(use-package org :pin org)
(use-package paredit)
(use-package paren-face)
(use-package pkg-info)
(use-package pretty-symbols)
(use-package rainbow-mode)
(use-package restclient)
(use-package robe)
(use-package s)
(use-package simple-httpd)
(use-package slime-company)
(use-package smartparens)
(use-package smex)
(use-package tuareg)
(use-package typo)
(use-package undo-fu)
(use-package uuid)
(use-package visual-regexp)
(use-package web-mode)
(use-package which-key)
(use-package yaml-mode)
(use-package yasnippet)

;;'(ido-grid-mode lsp-mode yaml-mode which-key web-mode visual-regexp uuid use-package undo-fu typo tuareg swift-mode smex smartparens slime-company simple-httpd rustic robe restclient rainbow-mode pretty-symbols pkg-info paren-face paredit ox-reveal magit julia-mode json-mode indium ido-completing-read+ htmlize highlight haskell-mode haml-mode geiser-racket geiser-kawa geiser-chez fringe-helper flymake-ruby flymake-cursor find-file-in-project expand-region elisp-slime-nav doom-themes doom-modeline diminish company-pollen company-inf-ruby company-go cider-eval-sexp-fu cider cargo bundler atomic-chrome aggressive-indent ag ace-jump-mode ac-geiser)
