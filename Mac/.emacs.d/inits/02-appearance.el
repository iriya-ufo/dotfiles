;;; appearance.el --- appearance configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------
;; brief   : theme and size
;; note    : -
;; ------------------------------------------------------------------
(use-package color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)

;; frame configuration
(tool-bar-mode -1)         ; disable tool bar
(menu-bar-mode -1)         ; disable menu bar
(set-scroll-bar-mode nil)  ; disable scroll bar
(set-frame-parameter nil 'fullscreen 'maximized)  ; start full screen

;; emacs startup buffer
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; translucent
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 86)))

;; ------------------------------------------------------------------
;; brief   : フォント設定
;; note    : 事前にフォントのインストールが必要
;;         : $ brew tap homebrew/cask-fonts
;;         : $ brew install --cask font-ricty-diminished
;; ------------------------------------------------------------------
(set-face-attribute 'default nil
                    :family "Ricty Diminished Discord"
                    :height 140)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (cons "Ricty Diminished Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0212
                  (cons "Ricty Diminished Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'katakana-jisx0201
                  (cons "Ricty Diminished Discord" "iso10646-1"))

;; ------------------------------------------------------------------
;; brief   : popwin
;; note    : https://github.com/emacsorphanage/popwin
;; ------------------------------------------------------------------
(use-package popwin)
(require 'popwin)
(popwin-mode 1)
;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)
;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)

;; ------------------------------------------------------------------
;; brief   : シンボルのハイライト
;; note    : -
;; ------------------------------------------------------------------
(use-package highlight-symbol)
(require 'highlight-symbol)
;; 使いたい色を設定
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

;; ------------------------------------------------------------------
;; brief   : ネストに応じて括弧の色を変える
;; note    : -
;; ------------------------------------------------------------------
(use-package rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; ------------------------------------------------------------------
;; brief   : 空白などの可視化
;; note    : -
;; ------------------------------------------------------------------
(require 'whitespace)
(setq whitespace-style '(face
                         trailing
                         tabs
                         spaces
                         empty
                         space-mark
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

(setq whitespace-space-regexp "\\(\u3000+\\)")  ; show full-width space
(setq whitespace-action '(auto-cleanup))        ; auto cleanup before save file
(global-whitespace-mode 1)                      ; enable whitespace-mode
