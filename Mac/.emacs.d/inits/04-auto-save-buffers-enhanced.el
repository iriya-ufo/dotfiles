;; -----------------------------------------------------------------------------------------------
;; brief   : auto save configuration
;; note    : https://github.com/kentaro/auto-save-buffers-enhanced
;; -----------------------------------------------------------------------------------------------
;; バッファの内容を自動保存
(use-package auto-save-buffers-enhanced)
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 0.5)
(auto-save-buffers-enhanced t)
