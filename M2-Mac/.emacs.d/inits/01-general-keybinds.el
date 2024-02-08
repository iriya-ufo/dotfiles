;;; general-keybinds.el --- general keybinds configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------
;; brief   : general keybinds
;; note    : -
;; ------------------------------------------------------------------
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\M-?" 'info-lookup-symbol)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
(global-set-key "\C-cc" 'compile)  ; C-c c でコンパイル

;; ------------------------------------------------------------------
;; brief   : frame control
;; note    : -
;; ------------------------------------------------------------------
(global-unset-key (kbd "C-t"))
(global-set-key (kbd "C-t v")   'split-window-below)
(global-set-key (kbd "C-t s")   'split-window-right)
(global-set-key (kbd "C-t C-t") 'other-window)
(global-set-key (kbd "C-t C-h") 'windmove-left)
(global-set-key (kbd "C-t C-j") 'windmove-down)
(global-set-key (kbd "C-t C-k") 'windmove-up)
(global-set-key (kbd "C-t C-l") 'windmove-right)
(global-set-key (kbd "C-t C-d") 'delete-window)

;; ------------------------------------------------------------------
;; brief   : multiple cursors keybind
;; note    : -
;; ------------------------------------------------------------------
(use-package multiple-cursors)
(use-package expand-region)
(use-package smartrep)
(require 'multiple-cursors)
(require 'expand-region)
(require 'smartrep)
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)
(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
(smartrep-define-key
    global-map "C-." '(("C-n" . 'mc/mark-next-like-this)
                      ("C-p" . 'mc/mark-previous-like-this)
                      ("*"   . 'mc/mark-all-like-this)))

;; ------------------------------------------------------------------
;; brief   : highlight-symbol keybind
;; note    : -
;; ------------------------------------------------------------------
(global-set-key (kbd "C-o") 'highlight-symbol-at-point)
(global-set-key (kbd "M-C-o") 'highlight-symbol-remove-all)

;; ------------------------------------------------------------------
;; brief   : hl-todo keybind
;; note    : https://github.com/tarsius/hl-todo
;; ------------------------------------------------------------------
(global-set-key (kbd "C-c C-p") 'hl-todo-previous)
(global-set-key (kbd "C-c C-n") 'hl-todo-next)

;; ------------------------------------------------------------------
;; brief   : lsp-mode keybinds
;; note    : -
;; ------------------------------------------------------------------
(defun lsp-mode-init ()
  (lsp)
  (global-set-key (kbd "M-*") 'xref-pop-marker-stack)
  (global-set-key (kbd "M-.") 'xref-find-definitions)
  (global-set-key (kbd "M-/") 'xref-find-references))
