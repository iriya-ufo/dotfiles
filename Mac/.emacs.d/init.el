;;; init.el --- Emacs main initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------
;; brief   : straight.el: next-generation package manager
;; note    : https://github.com/raxod502/straight.el
;; ------------------------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Users of Emacs versions >= 27 will want to add:
(setq package-enable-at-startup nil)

;; Integration with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; ------------------------------------------------------------------
;; brief   : init-loader configuration
;; note    : -
;; ------------------------------------------------------------------
(use-package init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")
