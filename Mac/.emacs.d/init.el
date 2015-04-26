;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 初期設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;================================================================
;; cask
;;================================================================
(when (require 'cask nil t)
  (cask-initialize))
;;
;;================================================================
;; add-to-load-path
;;================================================================
;; 引数を load-path へ追加する
(defun add-to-load-path (&rest paths)
  (mapc '(lambda (path)
	   (add-to-list 'load-path path))
	(mapcar 'expand-file-name paths)))
;; 設定ファイルのディレクトリを load-path に追加
(let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))
;; exec-path が GUI で正しく引き継がれない問題を解決
(let ((path-str
       (replace-regexp-in-string
	"\n+$" "" (shell-command-to-string "echo $PATH"))))
  (setenv "PATH" path-str)
  (setq exec-path (nconc (split-string path-str ":") exec-path)))
;;
;;================================================================
;; Language
;;================================================================
(add-hook 'set-language-environment-hook
	  (lambda ()
	    (when (equal "ja_JP.UTF-8" (getenv "LANG"))
	      (setq default-process-coding-system '(utf-8 . utf-8))
	      (setq default-file-name-coding-system 'utf-8))
	    (when (equal "Japanese" current-language-environment)
	      (setq default-buffer-file-coding-system 'utf-8)
	      (set-terminal-coding-system 'utf-8)
	      (set-keyboard-coding-system 'utf-8)
	      (set-buffer-file-coding-system 'utf-8))))
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
;; IME 設定
;; インラインパッチを当てないと使用できない
;(setq default-input-method "MacOSX")
;(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")
;(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `cursor-color "blue")
;(mac-set-input-method-parameter "com.google.inputmethod.Japanese.Roman" `cursor-color "green")
;;
;;================================================================
;; キーバインド
;;================================================================
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\M-?" 'info-lookup-symbol)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
;;
;;================================================================
;; テーマ・位置・ウィンドウ・フォント
;;================================================================
;; カスタムテーマは ~/.emacs.d/themes 配下に置く
(setq custom-theme-directory "~/.emacs.d/themes/")
;; テーマを読み込む
(load-theme 'molokai t)
;(load-theme 'deeper-blue t)

;; 位置調整
(setq initial-frame-alist
      (append (list
               '(width . 175)                  ; フレームの幅
               '(height . 46)                  ; フレームの高さ
               '(top . 0)                      ; Y 表示位置
               '(left . 0)                     ; X 表示位置
               )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; 半透明
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 90)))

;; ウィンドウ分割を賢くする
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;; フォント設定
(create-fontset-from-ascii-font "Monaco-12:weight=normal:slant=normal" nil "monaco")
(set-fontset-font "fontset-monaco"
                  'unicode
                  (font-spec :family "Hiragino Marugo ProN" :size 12)
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-monaco"))
;;
;;================================================================
;; popwin
;;================================================================
(require 'popwin)
(popwin-mode 1)
;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)
;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)
;;
;;================================================================
;; auto-save
;;================================================================
(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers)
;; バックアップファイル(file~)を作らない
(setq make-backup-files nil)
;;
;;================================================================
;; auto-complete
;;================================================================
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
;; 特定のモードで auto-complete を有効にする
(add-to-list 'ac-modes 'haml-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'ruby-mode)
;; 4文字以上の際に補完を開始する
(setq ac-auto-start 4)
;;
;;================================================================
;; multiple cursors
;;================================================================
(require 'expand-region)
(require 'multiple-cursors)
(require 'smartrep)
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)
(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
(smartrep-define-key
    global-map "C-." '(("C-n" . 'mc/mark-next-like-this)
		       ("C-p" . 'mc/mark-previous-like-this)
		       ("*"   . 'mc/mark-all-like-this)))
;;
;;================================================================
;; Helm (anything.el)
;;================================================================
(require 'helm-config)
(helm-mode 1)
;; Disable helm in some functions
(add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
;; Disable auto completion
(setq helm-ff-auto-update-initial-value nil)
;; Expand helm buffer length
(setq helm-buffer-max-length 50)
;; Key define
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "M-g .") 'helm-ag)
(global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))
;; Execute command only if CANDIDATE exists
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
    ad-do-it))
;; Transform the pattern to reflect my intention
(defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
  "Transform the pattern to reflect my intention"
  (let* ((pattern (ad-get-arg 0))
         (input-pattern (file-name-nondirectory pattern))
         (dirname (file-name-directory pattern)))
    (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
    (setq ad-return-value
          (concat dirname
                  (if (string-match "^\\^" input-pattern)
                      ;; '^' is a pattern for basename
                      ;; and not required because the directory name is prepended
                      (substring input-pattern 1)
                    (concat ".*" input-pattern))))))
;; Escape '.' to match '.' instead of an arbitrary character
(defun helm-buffers-list-pattern-transformer (pattern)
  (if (equal pattern "")
      pattern
    (setq pattern (replace-regexp-in-string "\\." "\\\\." pattern))
    (let ((first-char (substring pattern 0 1)))
      (cond ((equal first-char "*")
             (concat " " pattern))
            ((equal first-char "=")
             (concat "*" (substring pattern 1)))
            (t
             pattern)))))
(add-to-list 'helm-source-buffers-list
             '(pattern-transformer helm-buffers-list-pattern-transformer))
;; 選択範囲を isearch
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
;;
;;================================================================
;; cmigemo
;;================================================================
;; 日本語のインクリメンタル検索
;; インストール
;; $ brew install cmigemo
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)
;;
;;================================================================
;; highlight-symbol
;;================================================================
(require 'highlight-symbol)
;; 使いたい色を設定、repeat してくれる
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))
;; キーバインドの設定
(global-set-key (kbd "C-o") 'highlight-symbol-at-point)
(global-set-key (kbd "M-C-o") 'highlight-symbol-remove-all)
;;
;;================================================================
;; 雑多な設定
;;================================================================
(global-auto-revert-mode 1)               ; バッファの自動再読み込み
(require 'uniquify)                       ; バッファの同一ファイル名を区別する
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq initial-scratch-message             ; Scratch バッファの表示
"
This is Scratch Buffer.
")
(cd "~/")                                 ; ホームディレクトリより開始
(put 'narrow-to-region 'disabled nil)     ; ナローイングの有効化
(global-font-lock-mode t) 	              ; 文字の色つけ
(setq-default show-trailing-whitespace t) ; 行末の空白を表示
(setq-default tab-width 2)                ; タブ文字の幅を設定
(setq-default indent-tabs-mode nil)       ; インデント文字をタブではなく空白に設定
(setq require-final-newline t)            ; 行末に改行文字を追加する
(display-time)            	              ; 時計を表示
(global-linum-mode t)                     ; 全体の行番号を表示
(line-number-mode t) 	                    ; カーソルのある行番号を表示
(column-number-mode t)	 	                ; カーソルのある桁番号を表示
(auto-compression-mode t) 	              ; 日本語infoの文字化け防止
(setq frame-title-format  	              ; フレームのタイトル指定
      (concat "%b - emacs@" system-name))
(show-paren-mode 1) 	  	                ; 対応する括弧を光らせる
(electric-pair-mode t)                    ; 括弧の自動入力
(global-set-key "\C-cc" 'compile)         ; C-c cでコンパイル
(transient-mark-mode t)		                ; リージョンの色つけ
(setq-default truncate-lines t)           ; 行を折り返さない
(set-scroll-bar-mode 'right)	            ; スクロールバーを右にセット
(mouse-avoidance-mode 'exile)             ; カーソルが近付いたとき右上隅に移動,その後復帰
(setq inhibit-startup-message t)          ; 起動時にロゴ非表示
(menu-bar-mode 0)                         ; メニューバーを消す
(tool-bar-mode 0)                         ; ツールバーを消す
