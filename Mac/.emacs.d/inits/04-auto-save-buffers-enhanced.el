;;; auto-save-buffers-enhanced.el --- auto save configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; バッファの内容を自動保存
(use-package auto-save-buffers-enhanced)
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 2)
(auto-save-buffers-enhanced t)
