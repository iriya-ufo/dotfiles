;;; rust.el --- initializes rust modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------
;; brief   : rustic-mode is an extension of rust-mode that provides better support for the Rust language
;; note    : https://github.com/brotzeit/rustic
;;         : $ brew install rust-analyzer 
;; usage   : https://iriya-ufo.net/blog/2023/02/18/rust-setup-on-emacs/
;; ------------------------------------------------------------------
(use-package rustic)
(setq-default rustic-format-trigger 'on-save)
(add-hook 'rustic-mode-hook #'lsp)
