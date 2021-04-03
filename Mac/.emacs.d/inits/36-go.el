;;; go.el --- initializes go modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package go-mode)
(use-package company-go)
(use-package go-eldoc)
(use-package go-autocomplete)

(when (and (locate-library "exec-path-from-shell") (locate-library "go-mode"))
  (require 'exec-path-from-shell)
  (let ((envs '("PATH" "GOPATH")))
    (exec-path-from-shell-copy-envs envs))
  (require 'go-autocomplete)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq indent-tabs-mode t)
              (go-eldoc-setup)
              (setq gofmt-command "goimports")
              (add-hook 'before-save-hook 'gofmt-before-save)
              ))
  )
