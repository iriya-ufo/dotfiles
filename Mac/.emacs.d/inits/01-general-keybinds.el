;; -----------------------------------------------------------------------------------------------
;; brief   : keybinds configuration
;; note    : -
;; -----------------------------------------------------------------------------------------------
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\M-?" 'info-lookup-symbol)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

;; -----------------------------------------------------------------------------------------------
;; brief   : multiple cursors
;; note    : -
;; -----------------------------------------------------------------------------------------------
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
