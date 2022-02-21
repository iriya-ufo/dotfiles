;;; magit.el --- A Git Porcelain inside Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)
