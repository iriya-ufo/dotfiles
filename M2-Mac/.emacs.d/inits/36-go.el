;;; go.el --- initializes go modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package go-mode)
(add-hook 'go-mode-hook #'lsp)

;; ------------------------------------------------------------------
;; brief   : Adding useful tips by go-mode-hook
;; note    : You should install goimports for formatting
;;         : $ go install golang.org/x/tools/cmd/goimports@latest
;; ------------------------------------------------------------------
(defun go-mode-omnibus ()
  ;; Go code formatting by goimports
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  )
(add-hook 'go-mode-hook 'go-mode-omnibus)
