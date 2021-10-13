;;; go.el --- initializes go modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package go-mode)
(add-hook 'go-mode-hook #'lsp)
