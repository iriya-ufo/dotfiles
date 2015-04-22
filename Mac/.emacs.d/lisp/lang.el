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
 '(web-mode-doctype-face
   ((t (:foreground "#82AE46"))))               ; doctype
 '(web-mode-html-tag-face
   ((t (:foreground "#E6B422" :weight bold))))  ; 要素名
 '(web-mode-html-attr-name-face
   ((t (:foreground "#C97586"))))               ; 属性名など
 '(web-mode-html-attr-value-face
   ((t (:foreground "#82AE46"))))               ; 属性値
 '(web-mode-comment-face
   ((t (:foreground "#D9333F"))))               ; コメント
 '(web-mode-server-comment-face
   ((t (:foreground "#D9333F"))))               ; コメント
 '(web-mode-css-rule-face
   ((t (:foreground "#A0D8EF"))))               ; cssのタグ
 '(web-mode-css-pseudo-class-face
   ((t (:foreground "#FF7F00"))))               ; css 疑似クラス
 '(web-mode-css-at-rule-face
   ((t (:foreground "#FF7F00"))))               ; cssのタグ
)
