;;; cmigemo.el --- japanese incremental search -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------
;; brief   : 日本語のインクリメンタル検索
;; note    : cmigemo の事前インストールが必要
;;         : $ brew install cmigemo
;; ------------------------------------------------------------------
(use-package migemo)
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)
