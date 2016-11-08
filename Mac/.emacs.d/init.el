;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 初期設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;================================================================
;; cask
;;================================================================

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
;; 読み込まれないパスを設定
(add-to-list 'exec-path "/usr/local/bin")
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
;(load-theme 'molokai t)
(load-theme 'deeper-blue t)

;; 位置調整
(setq initial-frame-alist
      (append (list
               '(width . 234)                  ; フレームの幅
               '(height . 65)                  ; フレームの高さ
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
(create-fontset-from-ascii-font "Monaco-10:weight=normal:slant=normal" nil "monaco")
(set-fontset-font "fontset-monaco"
                  'unicode
                  (font-spec :family "Hiragino Marugo ProN" :size 10)
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
;; rainbow delimiters
;;================================================================
;; ネストの応じて括弧の色を変える
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; プログラミング関連設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;================================================================
;; Flymake -- syntax checker for Emacs
;;================================================================
(require 'flymake)
(require 'flymake-easy)
;; flymake-ruby
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
;; flymake-yaml
(add-hook 'yaml-mode-hook 'flymake-yaml-load)
;; flymake-haml
(require 'flymake-haml)
(add-hook 'haml-mode-hook 'flymake-haml-load)
;; flymake-coffee
(when (require 'flymake-coffee nil t)
  (add-hook 'coffee-mode-hook 'flymake-coffee-load))
;;
;;================================================================
;; Mark Down Mode
;;================================================================
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
;;
;;================================================================
;; C and C++
;;================================================================
(require 'cc-mode)
(setq c-default-style "k&r")          ; Kernighan & Ritchie スタイルにする
;; BackSpace キーを「賢く」し，インデント幅は4桁，タブはスペースに展開
(add-hook 'c-mode-common-hook
     	  '(lambda ()
             (progn
               (c-toggle-hungry-state 1)
               (setq c-basic-offset 4 indent-tabs-mode nil))))
;; .hpp と .h を C++ の拡張子とする (.h の方は余計かも)
(setq auto-mode-alist
      (append
       '(("\\.hpp$" . c++-mode)
         ("\\.h$"   . c++-mode)
         ) auto-mode-alist))
;; c-mode の色設定
(setq c-font-lock-keywords-3
      (append '(("(\\|)" . paren-face))
	      '(("{\\|}" . brace-face))
	      '(("\\[\\|\\]" . bracket-face))
	      c-font-lock-keywords-3))
;;
;;================================================================
;; Python
;;================================================================
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
;;
;;================================================================
;; ParEdit
;;================================================================
;; Lisp 系言語の入力支援
(require 'paredit)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
;;
;;================================================================
;; Scheme
;;================================================================
(setq scheme-program-name "/usr/local/bin/gosh -i")
(require 'cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cS" 'scheme-other-window)

(require 'scheme-complete)
(eval-after-load 'scheme
  ;; キーバインド
  '(progn
     ;; scheme-smart-complete: M-TAB
     (define-key scheme-mode-map "\e\t" 'scheme-smart-complete)
     ;; scheme-complete-or-indent: TAB
     (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (setq default-scheme-implementation 'gosh)
	    (setq *current-scheme-implementation* 'gosh)
	    ;; eldoc-mode
	    (set (make-local-variable 'eldoc-documentation-function)
		 'scheme-get-current-symbol-info)
	    (eldoc-mode t)))

;; [カーソルの移動]
;;
;; M-C Space 	カーソルの次のＳ式をマーク
;; M-C-a 	カーソルを含むトップレベルのＳ式の先頭へ移動
;; M-C-e 	カーソルを含むトップレベルのＳ式の末尾へ移動
;; M-C-f 	次のＳ式へ移動
;; M-C-b 	前のＳ式へ移動
;; M-C-t 	カーソルの前後のＳ式を交換
;; M-C-d 	1レベル内側のＳ式へ移動
;; M-C-u 	1レベル外側のＳ式へ移動

;; [ファイルのロードおよびＳ式の評価]
;;
;; C-cS とタイプして,Gaucheのインタープリターを起動
;; hoge.scm のバッファへ戻って,C-cC-lとするとミニバッファに
;; Load Scheme file: というプロンプトがあらわれて
;; インタープリタにロードするファイルを尋ねられる
;; そのままリターンキーを押すとインタープリタ側のバッファに #t が出る
;; これで,インタープリタにプログラムが読みこまれたことになる
;; gosh> のあとに先程読み込んだプログラムを実行して(リターンキーを押して)動作確認
;; 1つ1つのＳ式の評価は,C-xC-eで行う

;; 以下インデントの定義
(put 'and-let* 'scheme-indent-function 1)
(put 'begin0 'scheme-indent-function 0)
(put 'call-with-client-socket 'scheme-indent-function 1)
(put 'call-with-input-conversion 'scheme-indent-function 1)
(put 'call-with-input-file 'scheme-indent-function 1)
(put 'call-with-input-process 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-iterator 'scheme-indent-function 1)
(put 'call-with-output-conversion 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-string 'scheme-indent-function 0)
(put 'call-with-temporary-file 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'if-match 'scheme-indent-function 2)
(put 'let*-values 'scheme-indent-function 1)
(put 'let-args 'scheme-indent-function 2)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'let-match 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-syntax 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)
(put 'let1 'scheme-indent-function 2)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'multiple-value-bind 'scheme-indent-function 2)
(put 'match 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'parse-options 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 2)
(put 'rxmatch-case 'scheme-indent-function 1)
(put 'rxmatch-cond 'scheme-indent-function 0)
(put 'rxmatch-if  'scheme-indent-function 2)
(put 'rxmatch-let 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-builder 'scheme-indent-function 1)
(put 'with-error-handler 'scheme-indent-function 0)
(put 'with-error-to-port 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-from-string 'scheme-indent-function 1)
(put 'with-iterator 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)
(put 'with-locking-mutex 'scheme-indent-function 1)
(put 'guard 'scheme-indent-function 1)
;;
;;================================================================
;; Gauche info
;;================================================================
(eval-after-load "info-look"
  '(progn
     (info-lookup-add-help
      :topic 'symbol
      :mode 'scheme-mode
      :regexp "[^()`',\"\t\n]+"
      :ignore-case t
      :doc-spec '(("(gauche-refj.info)Index - 手続きと構文索引" nil
                   "^ -+ [^:]+: *" "[\n ]")
                  ("(gauche-refj.info)Index - モジュール索引" nil
                   "^ -+ [^:]+: *" "[\n ]")
                  ("(gauche-refj.info)Index - クラス索引" nil
                   "^ -+ [^:]+: *" "[\n ]")
                  ("(gauche-refj.info)Index - 変数索引" nil
                   "^ -+ [^:]+: *" "[\n ]")
                  ("(slib.info)Index" (lambda (item) (concat item " <slib>"))
                   "^ -+ [^:]+: *" "[\n ]")
                  ("(r5rs)Index" (lambda (item) (concat item " <r5rs>"))
                   "^[ \t]+-+ [^:]+:[ \t]*" "[\n ]"))
      :parse-rule "[^()`',\" \t\n]+"
      :other-modes nil)

     (info-lookup-add-help
      :mode 'inferior-scheme-mode
      :other-modes '(scheme-mode))
     ))
;;
;;================================================================
;; Clojure
;;================================================================
(require 'clojure-mode)
;;
;;================================================================
;; ruby-mode
;;================================================================
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;; 対応関係のハイライト
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
;; 改行時にオートインデント
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (define-key ruby-mode-map "\C-m" 'reindent-then-newline-and-indent)))
;; Setting rbenv PATH
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))
;; magic comment をオフにする
(setq ruby-insert-encoding-magic-comment nil)
;;
;;================================================================
;; Ruby on Rails
;;================================================================
;; rhtml-mode
(when (require 'rhtml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))
  (add-hook 'rhtml-mode-hook
	    (lambda () (rinari-launch))))
;; yaml-mode
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))
;; JavaScript
(when (require 'js3-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode)))
;; coffee-mode
(when (require 'coffee-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
  (add-hook 'coffee-mode-hook
	    '(lambda () (setq tab-width 2))))
;; haml-mode
(require 'haml-mode)
;; slim-mode
(require 'slim-mode)
;; scss-mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(defun scss-custom ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   (set (make-local-variable 'scss-compile-at-save) nil)
   )
  )
(add-hook 'scss-mode-hook
  '(lambda() (scss-custom)))
;;
;;================================================================
;; web-mode
;;================================================================
(require 'web-mode)
;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset    2)
  (setq web-mode-code-indent-offset   2))
(add-hook 'web-mode-hook 'web-mode-hook)
;; 色の設定
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-comment-face ((t (:foreground "#D9333F"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
 '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face ((t (:foreground "#E6B422" :weight bold))))
 '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (package-build shut-up epl git commander f dash s))))
