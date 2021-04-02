;;; development-tools.el --- development configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
