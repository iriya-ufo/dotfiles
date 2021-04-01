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
