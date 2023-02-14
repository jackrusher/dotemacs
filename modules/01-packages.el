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

;;;; Install straight
;; (defvar bootstrap-version)

;; (let ((install-url "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")
;;       (bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer (url-retrieve-synchronously install-url 'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; ;;;; Tell it to hook into use-package
;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

;; ;;;; Create a version file if it does not yet exist
;; (when (not (file-exists-p (expand-file-name "straight/versions/default.el" straight-base-dir)))
;;   (straight-freeze-versions))

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

;; completion plugins
(use-package company)
;;(use-package company-ghc)
(use-package company-inf-ruby)
(use-package company-go)

(use-package dash)
(use-package diminish)
(use-package docker-tramp)
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
(use-package git-commit)
(use-package go-mode)
(use-package haml-mode)
(use-package haskell-mode)
(use-package highlight)
(use-package ido-completing-read+)
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
;;(use-package typopunct)
(use-package undo-fu)
(use-package uuid)
(use-package visual-regexp)
(use-package web-mode)
(use-package which-key)
(use-package yaml-mode)
(use-package yasnippet)
