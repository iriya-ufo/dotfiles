;;; magit.el --- A Git Porcelain inside Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; magit を利用するので Emacs 標準のバージョン管理ツールを使用しない
(setq vc-handled-backends nil)
(eval-after-load "vc"
  '(remove-hook 'find-file-hooks 'vc-find-file-hook))

(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)
