;;; helm.el --- helm configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package helm)
(use-package helm-ag)
(use-package projectile)
(use-package helm-projectile)

(require 'helm-config)
(helm-mode 1)
(helm-projectile-on)

;; global-map keybind is defiend at general-keybinds.el
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
