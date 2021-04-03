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
