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

;; -----------------------------------------------------------------------------------------------
;; brief   : popwin
;; note    : https://github.com/emacsorphanage/popwin
;; -----------------------------------------------------------------------------------------------
(use-package popwin)
(require 'popwin)
(popwin-mode 1)
;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)
;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)

;; -----------------------------------------------------------------------------------------------
;; brief   : highlight-symbol
;; note    : -
;; -----------------------------------------------------------------------------------------------
(use-package highlight-symbol)
(require 'highlight-symbol)
;; 使いたい色を設定
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))
;; キーバインドの設定
(global-set-key (kbd "C-o") 'highlight-symbol-at-point)
(global-set-key (kbd "M-C-o") 'highlight-symbol-remove-all)

;; -----------------------------------------------------------------------------------------------
;; brief   : ネストに応じて括弧の色を変える
;; note    : -
;; -----------------------------------------------------------------------------------------------
(use-package rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
