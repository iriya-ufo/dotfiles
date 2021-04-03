;;; general-conf.el --- general configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; バックアップファイルを作らない
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; 履歴の設定
(setq history-length t)         ; set mini-buffer history length to infinity
(setq undo-no-redo t)           ; set undo no redo
(setq undo-limit 10000)         ; set undo limit
(setq undo-strong-limit 50000)  ; set undo limit

;; バッファの同一ファイル名を区別する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; 選択範囲で isearch
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

(global-auto-revert-mode 1)               ; バッファの自動読み込み
(cd "~/")                                 ; ホームディレクトリより開始
(put 'narrow-to-region 'disabled nil)     ; ナローイングの有効化
(global-font-lock-mode t)                 ; 文字の色付け
(setq-default show-trailing-whitespace t) ; 行末の空白を表示
(setq-default tab-width 2)                ; タブ文字の幅を設定
(setq-default indent-tabs-mode nil)       ; インデント文字をタブではなく空白に設定
(setq require-final-newline t)            ; 行末に改行文字を追加する
(display-time)                            ; 時計を表示
(global-linum-mode t)                     ; 全体の行番号を表示
(line-number-mode t)                      ; カーソルのある行番号を表示
(column-number-mode t)                    ; カーソルのある桁番号を表示
(auto-compression-mode t)                 ; 日本語infoの文字化け防止
(setq frame-title-format                  ; フレームのタイトル指定
      (concat "%b - emacs@" system-name))
(show-paren-mode 1)                       ; 対応する括弧を光らせる
(electric-pair-mode t)                    ; 括弧の自動入力
(setq-default truncate-lines t)           ; 行を折り返さない
(setq scroll-step 1)                      ; 一行ずつスクロール
(mouse-avoidance-mode 'exile)             ; カーソルが近付いたとき右上隅に移動,その後復帰
