;; -----------------------------------------------------------------------------------------------
;; brief   : auto complete configuration
;; note    : https://github.com/auto-complete/auto-complete
;; -----------------------------------------------------------------------------------------------
(use-package auto-complete)
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start 4)  ; 4文字以上の際に補完を開始する
