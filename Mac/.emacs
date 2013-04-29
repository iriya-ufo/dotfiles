;;--------------------------------------------------
;; File name    :   ~/.emacs
;;              :   Emacs の基本的な設定
;;--------------------------------------------------
;;
;;===================================
;; add-to-load-path
;;===================================
;; 引数を load-path へ追加する
(defun add-to-load-path (&rest paths)
   (mapc '(lambda (path)
            (add-to-list 'load-path path))
         (mapcar 'expand-file-name paths)))
;; 設定ファイルのディレクトリを load-path に追加
(let ((default-directory (expand-file-name "~/.emacs.d")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))
;;
;;===================================
;; Language
;;===================================
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
(setq default-input-method "MacOSX")
;;
;;===================================
;; Wheel mouse
;;===================================
;;(global-set-key [mouse-4] 'scroll-down)
;;(global-set-key [mouse-5] 'scroll-up)
(progn
  (defun scroll-up-half ()
    "Scroll up half a page."
    (interactive)
    (scroll-up (/ (window-height) 2))
    )
  (defun scroll-down-half ()
    "Scroll down half a page."
    (interactive)
    (scroll-down (/ (window-height) 2))
    )
  (global-set-key [(mouse-5)] 'scroll-up-half)
  (global-set-key [(mouse-4)] 'scroll-down-half)
  )
;;
;;====================================
;; keyboard
;;====================================
;; キーバインド
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\M-?" 'info-lookup-symbol)
;; 改行キーでオートインデントさせる
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
;; C-x p で逆向きへのウィンドウ移動
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))
;;
;;====================================
;; フレームサイズ,位置,色,フォントなど
;;====================================
;; カラーテーマを使う
(require 'color-theme)
(color-theme-initialize)
;; テーマ"molokai"を使う
(color-theme-molokai)

(setq initial-frame-alist
      (append (list
;	       '(foreground-color . "#333333") ; 文字色
;	       '(background-color . "#ffffff") ; 背景色
;	       '(border-color . "black")
;	       '(mouse-color . "black")
;	       '(cursor-color . "#191970")
	       '(width . 79)                   ; フレームの幅
	       '(height . 50)                  ; フレームの高さ
	       '(top . 0)                      ; Y 表示位置
	       '(left . 400)                   ; X 表示位置
	       )
	      initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; 使用可能フォント一覧の見方 (ターミナル上で)
;; fc-list

;; Emacs23 用フォント設定
;(cond (window-system
;       (set-default-font "VL Gothic-10")
;       (set-fontset-font (frame-parameter nil 'font)
;			 'japanese-jisx0208
;			;'("VL PGothic" . "unicode-bmp"))
;			 '("IPA モナー P明朝" . "unicode-bmp"))
;       ))
;;
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

;; lisp-mode の色設定に追加
(setq lisp-font-lock-keywords-2
      (append '(("(\\|)" . paren-face))
	      lisp-font-lock-keywords-2))

;; scheme-mode の色設定に追加
(add-hook 'scheme-mode-hook
	  '(lambda ()
	     (add-to-list 'scheme-font-lock-keywords-2 '("(\\|)" . paren-face))
	     (add-to-list 'scheme-font-lock-keywords-2 '("\\[\\|\\]" . bracket-face))
	     (add-to-list 'scheme-font-lock-keywords-2 '("#\\\\\\(\\w+\\|.\\)" . font-lock-string-face)) ; 文字 #\x
	     (add-to-list 'scheme-font-lock-keywords-2 '("#\\[.*?\\]" . font-lock-string-face)) ; 文字集合 #[...]
	     (add-to-list 'scheme-font-lock-keywords-2 '("#/\\(.\\|\\\\/\\)*/" . font-lock-string-face)))) ; 正規表現 #/.../
;;
;;====================================
;; auto-save
;;====================================
;; ~/.emacs.d に auto-save-buffers.el を置く
(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers) 
;; バックアップファイル(file~)を作らない
(setq make-backup-files nil)
;;
;;====================================
;; C and C++
;;====================================
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
;;====================================
;; Python
;;====================================
;; ~/.emacs.d に python-mode.el-6.0.3 を置く
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; F1 m でヘルプ参照
;;
;;====================================
;; Scheme
;;====================================
(setq scheme-program-name "gosh")
(require 'cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cS" 'scheme-other-window)

;;------------------------------------
;; "scheme-complete"
;; Smart tab completion for Emacs
;;------------------------------------
;; ~/.emacs.d に scheme-complete.el を置く
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
;;====================================
;; Gauche info
;;====================================
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
;;====================================
;; CommonLisp
;;====================================
;; To use SBCL
;(defun sbcl-start ()
;  (interactive)
;  (shell-command "sbcl --load /home/iriya/.slime.lisp &"))
;; To use SLIM
;; SLIM: The Superior Lisp Interaction Mode for Emacs
;(setq inferior-lisp-program "/usr/local/bin/sbcl")
;(require 'slime)
;(setq slime-net-coding-system 'utf-8-unix)
;(add-hook 'lisp-mode-hook (lambda ()
;                            (slime-mode t)))
;(add-hook 'slime-mode-hook
;          (lambda ()
;            (setq lisp-indent-function 'common-lisp-indent-function)))
;(add-hook 'inferior-lisp-mode-hook
;          (lambda ()
;            (slime-mode t)))
;; Additional definitions by Pierpaolo Bernardi.
;(defun cl-indent (sym indent)
;  (put sym 'common-lisp-indent-function
;       (if (symbolp indent)
;           (get indent 'common-lisp-indent-function)
;	   indent)))

;(cl-indent 'if '1)
;(cl-indent 'generic-flet 'flet)
;(cl-indent 'generic-labels 'labels)
;(cl-indent 'with-accessors 'multiple-value-bind)
;(cl-indent 'with-added-methods '((1 4 ((&whole 1))) (2 &body)))
;(cl-indent 'with-condition-restarts '((1 4 ((&whole 1))) (2 &body)))
;(cl-indent 'with-simple-restart '((1 4 ((&whole 1))) (2 &body)))

;(setq slme-lisp-implementations
;      '((sbcl ("sbcl") :coding-system utf-8-unix)
;        (cmucl ("cmucl") :coding-system iso-latin-1-unix)))
;;
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
;;
;;====================================
;; AUCTeX
;;====================================
(require 'tex-site)
;(setq TeX-default-mode 'japanese-latex-mode)
;(setq japanese-TeX-command-default "pTeX")
;(setq japanese-LaTeX-command-default "pLaTeX")
;(setq japanese-LaTeX-default-style "jsarticle")
;(setq TeX-file-extensions '("tex" "sty" "cls" "ltx" "texi" "texinfo" "dtx"))
;(setq kinsoku-limit 10)
;(setq LaTeX-indent-level 4)
;(setq TeX-output-view-style '(("^pdf$" "." "/usr/bin/open -a preview %o")))
;(add-to-list 'TeX-output-view-style '("^dvi$" "." "dvipdfmx %d && open -a preview %s.pdf"))
;(setq LaTeX-clean-intermediate-suffixes '("\\.aux" "\\.log" "\\.out" "\\.toc" "\\.brf" "\\.nav" "\\.snm"))
;;
;;====================================
;; 雑多な設定
;;====================================
(require 'magit)                          ; Git を使う
(setq initial-scratch-message             ; Scratch バッファの表示
"
This is Scratch Buffer.
")
(cd "~/")                                 ; ホームディレクトリより開始
(put 'narrow-to-region 'disabled nil)     ; ナローイングの有効化
(global-font-lock-mode t) 	          ; 文字の色つけ
(display-time)            	          ; 時計を表示
(setq line-number-mode t) 	          ; カーソルのある行番号を表示
(column-number-mode t)	 	          ; カーソルのある桁番号を表示
(auto-compression-mode t) 	          ; 日本語infoの文字化け防止
(setq frame-title-format  	          ; フレームのタイトル指定
      (concat "%b - emacs@" system-name))
(show-paren-mode 1) 	  	          ; 対応する括弧を光らせる
(global-set-key "\C-cc" 'compile)         ; C-c cでコンパイル
(transient-mark-mode t)		          ; リージョンの色つけ
(setq-default truncate-lines t)           ; 行を折り返さない
(set-scroll-bar-mode 'right)	          ; スクロールバーを右にセット
(mouse-avoidance-mode 'exile)             ; カーソルが近付いたとき右上隅に移動,その後復帰
(setq inhibit-startup-message t)          ; 起動時にロゴ非表示
;;------------------------------------
(menu-bar-mode 0)                         ; メニューバーを消す
(tool-bar-mode 0)                         ; ツールバーを消す
;; 起動中に表示の ON OFF を切り替えるには,それぞれ
;; M-x menu-bar-mode
;; M-x tool-bar-mode
;; と打てばよい
;;------------------------------------
;;
;;====================================
;; バッファの切替えを楽にする
;;====================================
(iswitchb-mode t)
(add-hook 'iswitchb-define-mode-map-hook
          'iswitchb-my-keys)
(defun iswitchb-my-keys ()
  "Add my keybindings for iswitchb."
  (define-key iswitchb-mode-map " " 'iswitchb-next-match)
  )
;;
;;====================================
;; outline-mode, outline-minor-mode
;;====================================
;; outline-minor-mode のプレフィックス変更
(setq outline-minor-mode-prefix "\C-c\C-n")

;; To use c-outline.el in a c-mode buffer simply type `M-x c-outline RET'.
;; To turn off outline-minor-mode, do `M-x outline-minor-mode'.
(autoload 'c-outline "c-outline" nil t)
(add-hook 'c-mode-hook 'c-outline)

;; Lispファイルを開いたら自動で outline-minor-mode へ移行
(add-hook 'slime-mode-hook
 	  (outline-minor-mode t))

;; outline-minor-mode キーバインド一覧
;;------------------------------------
;; `outline-minor-mode' Minor Mode Bindings Starting With C-c C-n:
;; key binding
;; --- -------
;; C-c C-n C-a show-all
;; C-c C-n C-b outline-backward-same-level
;; C-c C-n C-c hide-entry
;; C-c C-n C-d hide-subtree
;; C-c C-n C-e show-entry
;; C-c C-n C-f outline-forward-same-level
;; C-c C-n TAB show-children
;; C-c C-n C-k show-branches
;; C-c C-n C-l hide-leaves
;; C-c C-n RET outline-insert-heading
;; C-c C-n C-n outline-next-visible-heading
;; C-c C-n C-o hide-other
;; C-c C-n C-p outline-previous-visible-heading
;; C-c C-n C-q hide-sublevels
;; C-c C-n C-s show-subtree
;; C-c C-n C-t hide-body
;; C-c C-n C-u outline-up-heading
;; C-c C-n C-v outline-move-subtree-down
;; C-c C-n C-^ outline-move-subtree-up
;; C-c C-n @ outline-mark-subtree
;; C-c C-n C-< outline-promote
;; C-c C-n C-> outline-demote
;;
;;====================================
;; sdic-mode 英和-和英辞書
;;====================================
(autoload 'sdic-describe-word "sdic" "単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point "sdic" "カーソル位置の単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)
;; 動作と見かけの調整
(setq sdic-window-height 10
      sdic-disable-select-window t)
;; 英和-和英辞書の設定
(setq sdic-eiwa-dictionary-list
      '((sdicf-client "/usr/share/dict/eijiro.sdic")))
(setq sdic-waei-dictionary-list
      '((sdicf-client "/usr/share/dict/waeijiro.sdic")))
;;
;;====================================
;; shell-mode でエスケープシーケンスを正しく表示
;;====================================
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;
;;====================================
;; 印刷設定
;;====================================
(setq ps-multibyte-buffer 'non-latin-printer)
(require 'ps-mule)
(defalias 'ps-mule-header-string-charsets 'ignore)
