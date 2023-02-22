;;; typescript.el --- initializes typescript modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------
;; brief   : TypeScript mode with language server
;; note    : https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
;;         : $ npm i -g typescript-language-server
;;         : Install this language server with M-x lsp-install-server RET ts-ls RET.
;; ------------------------------------------------------------------
(use-package typescript-mode)
(use-package tide)
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\.ts$" . typescript-mode))
(add-hook 'typescript-mode-hook
          #'(lambda ()
             (interactive)
             (tide-setup)
             (flycheck-mode +1)
             (tide-hl-identifier-mode +1)
             (company-mode +1)
             (eldoc-mode +1)
             (setq typescript-indent-level 2)
             ))
(add-hook 'typescript-mode-hook #'lsp)
