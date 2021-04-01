;; -----------------------------------------------------------------------------------------------
;; brief   : C and C++
;; note    : -
;; -----------------------------------------------------------------------------------------------
(use-package cc-mode)
(require 'cc-mode)

;; Kernighan & Ritchie style
(setq c-default-style "k&r")

;; Indents 4 and tabs expand to spaces
(add-hook 'c-mode-common-hook
         '(lambda ()
             (progn
               (c-toggle-hungry-state 1)
               (setq c-basic-offset 4 indent-tabs-mode nil))))

;; .hpp and .h are c++ extensions
(setq auto-mode-alist
      (append
       '(("\\.hpp$" . c++-mode)
         ("\\.h$"   . c++-mode)
         ) auto-mode-alist))

;; c-mode color setting
(setq c-font-lock-keywords-3
      (append '(("(\\|)" . paren-face))
             '(("{\\|}" . brace-face))
             '(("\\[\\|\\]" . bracket-face))
             c-font-lock-keywords-3))
