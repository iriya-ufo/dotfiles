;;; go.el --- initializes go modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package go-mode)
(add-hook 'go-mode-hook #'lsp)

;; ------------------------------------------------------------------
;; brief   : goimports
;; note    : https://pkg.go.dev/golang.org/x/tools/cmd/goimports
;;         : $ go install golang.org/x/tools/cmd/goimports@latest
;; ------------------------------------------------------------------
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
