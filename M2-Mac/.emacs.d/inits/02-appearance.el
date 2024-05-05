;;; appearance.el --- appearance configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------
;; brief   : theme and size
;; note    : -
;; ------------------------------------------------------------------
(use-package modus-themes)
(load-theme 'modus-vivendi-tinted t)

;; frame configuration
(tool-bar-mode -1)         ; disable tool bar
(menu-bar-mode -1)         ; disable menu bar
(set-scroll-bar-mode nil)  ; disable scroll bar
(setq default-frame-alist
      '(
        (width . 140)
        (height . 80)
        ))

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
;;         : $ brew install font-cica
;; ------------------------------------------------------------------
(set-face-attribute 'default nil
                    :family "Cica"
                    :height 160)

;; ------------------------------------------------------------------
;; brief   : Neotree
;; note    : https://github.com/jaypei/emacs-neotree
;; ------------------------------------------------------------------
(use-package neotree
  :init
  (setq-default neo-keymap-style 'concise)
  (setq-default neo-show-hidden-files t)
  :config
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (setq neo-theme 'arrow)
  (bind-key "C-c C-o" 'neotree-toggle)
  (bind-key "RET" 'neotree-enter-hide neotree-mode-map)
  (bind-key "a" 'neotree-hidden-file-toggle neotree-mode-map)
  (bind-key "<left>" 'neotree-select-up-node neotree-mode-map)
  (bind-key "<right>" 'neotree-change-root neotree-mode-map))

;; Change neotree's font size
;; Tips from https://github.com/jaypei/emacs-neotree/issues/218
(defun neotree-text-scale ()
  "Text scale for neotree."
  (interactive)
  (text-scale-adjust 0)
  (text-scale-decrease 0.5)
  (message nil))
(add-hook 'neo-after-create-hook
          (lambda (_)
            (call-interactively 'neotree-text-scale)))

;; Hide neotree window after open file
;; Tips from https://github.com/jaypei/emacs-neotree/issues/77
(defun neo-open-file-hide (full-path &optional arg)
  "Open file and hiding neotree.
The description of FULL-PATH & ARG is in `neotree-enter'."
  (neo-global--select-mru-window arg)
  (find-file full-path)
  (neotree-hide))

(defun neotree-enter-hide (&optional arg)
  "Neo-open-file-hide if file, Neo-open-dir if dir.
The description of ARG is in `neo-buffer--execute'."
  (interactive "P")
  (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))

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
;; brief   : Highlight TODO and similar keywords in comments and strings
;; note    : https://github.com/tarsius/hl-todo
;; ------------------------------------------------------------------
(use-package hl-todo)
(require 'hl-todo)
(setq hl-todo-highlight-punctuation ":")
(setq hl-todo-keyword-faces
      '(("TODO"       . "#7FFFD4")
        ("FIXME"      . "#F0E68C")
        ("DEPRECATED" . "#FF1493")
        ("DEBUG"      . "#EE82EE")))
(global-hl-todo-mode 1)

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
