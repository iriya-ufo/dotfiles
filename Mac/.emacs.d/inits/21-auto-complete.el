;;; auto-complete.el --- auto complete configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package auto-complete)
(require 'auto-complete)
(require 'auto-complete-config)

(setq ac-dwim t)        ; useful configuration
(setq ac-auto-start 4)  ; start trigger: input 4 characters
(setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat")

(ac-set-trigger-key "TAB")
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(global-auto-complete-mode t)
