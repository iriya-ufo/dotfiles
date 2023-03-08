;;; development-tools.el --- development configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------
;; brief   : Language Server Protocol Support for Emacs
;; note    : https://github.com/emacs-lsp/lsp-mode
;; ------------------------------------------------------------------
(use-package lsp-mode)
(use-package company-lsp)
(use-package lsp-ui)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-doc-max-width 150)
(setq lsp-ui-doc-max-height 30)
(setq lsp-ui-peek-enable t)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; ------------------------------------------------------------------
;; brief   : quickrun
;; note    : https://github.com/emacsorphanage/quickrun
;; ------------------------------------------------------------------
(use-package quickrun)
(require 'quickrun)

;; ------------------------------------------------------------------
;; brief   : http - Yet another HTTP client
;; note    : https://github.com/emacs-pe/http.el
;; ------------------------------------------------------------------
(use-package http)
(require 'http)

;; ------------------------------------------------------------------
;; brief   : flycheck configuration
;; note    : -
;; ------------------------------------------------------------------
(use-package flycheck)
(use-package flycheck-color-mode-line)
(use-package flycheck-pos-tip)

(require 'flycheck)
(global-flycheck-mode t)
(require 'flycheck-color-mode-line)

;; ------------------------------------------------------------------
;; brief   : yasnippet
;; note    : Yasnippet official snippet collections
;;         : https://github.com/AndreaCrotti/yasnippet-snippets/tree/master/snippets
;; ------------------------------------------------------------------
(use-package yasnippet)
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; ------------------------------------------------------------------
;; brief   : ParEdit
;; note    : Lisp 系言語の入力支援
;; ------------------------------------------------------------------
(use-package paredit)
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
