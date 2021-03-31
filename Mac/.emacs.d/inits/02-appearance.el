;; -----------------------------------------------------------------------------------------------
;; brief   : appearance configuration
;; note    : -
;; -----------------------------------------------------------------------------------------------
(use-package color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)

;; frame configuration
(tool-bar-mode -1)         ; disable tool bar
(menu-bar-mode -1)         ; disable menu bar
(set-scroll-bar-mode nil)  ; disable scroll bar
(set-frame-parameter nil 'fullscreen 'maximized)  ; start full screen

;; emacs startup buffer
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; 半透明化
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 86)))
