;; FIXME might want to `add-to-list` instead
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("marmalade"    . "https://marmalade-repo.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org"          . "http://orgmode.org/elpa/"))) ;; no https :(


;;;; We need the `use-package` package before we use `use-package` to use all our packages
(package-initialize)

(unless (file-exists-p (expand-file-name "archives/melpa" package-user-dir))
  (package-refresh-contents))

(package-install 'use-package) ;; (if this fails, e.g. because it can't find such-and-such version, try manually executing `package-refresh-contents`)


;;;; Packages we require
;;;; - Listed alphabetically
;;;; - Line breaks
;;;;   - every 10 packages, or
;;;;   - if the package requires special config (e.g. pinning to a particular archive)
(use-package ace-jump-mode     :ensure t)
(use-package ack-and-a-half    :ensure t)
(use-package ag                :ensure t)
(use-package aggressive-indent :ensure t)
(use-package bundler           :ensure t)
(use-package caml              :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable)
(use-package cider-eval-sexp-fu :ensure t)

(use-package clojure-mode         :ensure t)
(use-package coffee-mode          :ensure t)
(use-package color-theme          :ensure t)
(use-package company              :ensure t)
(use-package company-ghc          :ensure t)
(use-package company-inf-ruby     :ensure t)
(use-package company-go           :ensure t)
(use-package dash                 :ensure t)
(use-package diminish             :ensure t)
(use-package docker-tramp         :ensure t)

(use-package elisp-slime-nav      :ensure t)
(use-package epl                  :ensure t)
(use-package expand-region        :ensure t)
(use-package find-file-in-project :ensure t)
(use-package flymake-coffee       :ensure t)
(use-package flymake-cursor       :ensure t)
(use-package flymake-easy         :ensure t)
(use-package flymake-ruby         :ensure t)
(use-package fringe-helper        :ensure t)
(use-package geiser               :ensure t)

(use-package git-commit           :ensure t)
(use-package go-mode              :ensure t)
(use-package haml-mode            :ensure t)
(use-package haskell-mode         :ensure t)
(use-package highlight            :ensure t)
(use-package ido-completing-read+ :ensure t)
(use-package inf-ruby             :ensure t)
(use-package js2-mode             :ensure t)
(use-package js2-refactor         :ensure t)
(use-package json-mode            :ensure t)

(use-package magit                :ensure t)
(use-package markdown-mode        :ensure t)
(use-package motion-mode          :ensure t)
(use-package multiple-cursors     :ensure t)

(use-package org
  :ensure t
  :pin org)

(use-package pallet          :ensure t)
(use-package paredit         :ensure t)
(use-package parenface-plus  :ensure t)
(use-package pkg-info        :ensure t)
(use-package powerline       :ensure t)
(use-package pretty-symbols  :ensure t)
(use-package processing-mode :ensure t)
(use-package rainbow-mode    :ensure t)
(use-package restclient      :ensure t)
(use-package robe            :ensure t)

(use-package s               :ensure t)
(use-package simple-httpd    :ensure t)
(use-package skewer-mode     :ensure t)
(use-package slime-company   :ensure t)
(use-package smartparens     :ensure t)
(use-package smex            :ensure t)
(use-package tuareg          :ensure t)
(use-package typopunct       :ensure t)
(use-package undo-tree       :ensure t)

(use-package uuid            :ensure t)
(use-package visual-regexp   :ensure t)
(use-package web-mode        :ensure t)
(use-package which-key       :ensure t)
(use-package yaml-mode       :ensure t)
(use-package yasnippet       :ensure t)
