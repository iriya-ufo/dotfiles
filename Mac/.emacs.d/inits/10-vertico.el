;;; vertico.el --- vertico configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------
;; brief   : Vertico provides a performant and minimalistic vertical completion UI based on the default completion system.
;; note    : https://github.com/minad/vertico
;; usage   : https://blog.tomoya.dev/posts/a-new-wave-has-arrived-at-emacs/
;; ------------------------------------------------------------------
(use-package vertico)                   ; ミニバッファ補完UI
(use-package savehist)                  ; 補完候補の履歴: ~/.emacs.d に history ファイルが作成される
(use-package orderless)                 ; 順不同のスペース区切り補完スタイルの提供
(use-package marginalia)                ; 補完候補にファイル属性などの追加情報を提供
(use-package embark-consult)            ; embark と consult の連携
(use-package embark)                    ; キーボード操作のコンテキストメニューの提供
(use-package consult)                   ; 補完候補リストの作成と便利な補完コマンドを提供

;; 補完スタイルに orderless を利用する
(with-eval-after-load 'orderless
  (setq completion-styles '(orderless)))

;; 補完候補を最大20行まで表示する
(setq vertico-count 20)

;; vertico-mode と marginalia-mode を有効化する
(defun after-init-hook ()
  (vertico-mode)
  (marginalia-mode)
  ;; savehist-mode を使って Vertico の順番を永続化する
  (savehist-mode))
(add-hook 'after-init-hook #'after-init-hook)

;; embark-consult を読み込む
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))

;; C-u を付けるとカーソル位置の文字列を使う my-consult-line コマンドを定義する
(defun my-consult-line (&optional at-point)
  "Consult-line uses things-at-point if set C-u prefix."
  (interactive "P")
  (if at-point
      (consult-line (thing-at-point 'symbol))
    (consult-line)))
;; C-s [isearch-forward] を my-consult-line コマンドに割り当てる
(global-set-key (kbd "C-s") 'my-consult-line)

;; C-s/C-r で行を移動できるようにする
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-r") 'vertico-previous)
  (define-key vertico-map (kbd "C-s") 'vertico-next))

;; C-l で一つ上の階層に移動する
(define-key vertico-map (kbd "C-l") #'vertico-directory-up)

;; goto-line [M-g g] を consult-goto-line に変更
(global-set-key [remap goto-line] 'consult-goto-line)
