
;; 設定ファイルのディレクトリを load-path に追加
(let ((default-directory (expand-file-name "~/.emacs.d")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; キーバインド
(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(define-key global-map "\M-?" 'help-for-help)        ; ヘルプ
(define-key global-map "\C-z" 'undo)                 ; undo
(define-key global-map "\C-ci" 'indent-region)       ; インデント
(define-key global-map "\C-c\C-i" 'dabbrev-expand)   ; 補完
(define-key global-map "\C-c;" 'comment-region)      ; コメントアウト
(define-key global-map "\C-c:" 'uncomment-region)    ; コメント解除
(define-key global-map "\C-o" 'toggle-input-method)  ; 日本語入力切替
(define-key global-map "\C-\\" nil) ; \C-\の日本語入力の設定を無効にする
(define-key global-map "\C-c " 'other-frame)         ; フレーム移動

;; shell を正しく表示させる
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; 色を付ける
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq transient-mark-mode t)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;;====================================
;; 括弧の色を薄くする
;;====================================
;; 小括弧 () の色を定義
(defvar paren-face 'paren-face)
(make-face 'paren-face)
(set-face-foreground 'paren-face "#88aaff")

;; 中括弧 {} の色を定義
(defvar brace-face 'brace-face)
(make-face 'brace-face)
(set-face-foreground 'brace-face "#ffaa88")

;; 大括弧 [] の色を定義
(defvar bracket-face 'bracket-face)
(make-face 'bracket-face)
(set-face-foreground 'bracket-face "#72ae91")

;;====================================
;; auto-save
;;====================================
;; ~/.emacs.d に auto-save-buffers.el を置く
(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers) 
;; バックアップファイル(file~)を作らない
(setq make-backup-files nil)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;====================================
;; ruby-mode
;;====================================
(require 'ruby-electric)
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
	     (inf-ruby-keys)))