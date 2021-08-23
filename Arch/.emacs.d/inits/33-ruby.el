;;; ruby.el --- initializes ruby modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; install packages for ruby
(use-package ruby-mode)
(use-package rubocop)
(use-package ruby-electric)
(use-package ruby-end)

;; install packages for rails
(use-package yaml-mode)
(use-package js3-mode)
(use-package coffee-mode)
(use-package rhtml-mode)
(use-package haml-mode)
(use-package slim-mode)
(use-package scss-mode)
(use-package sass-mode)

;; ------------------------------------------------------------------
;; brief   : Ruby
;; note    : -
;; ------------------------------------------------------------------
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; auto indent with newline
(add-hook 'ruby-mode-hook
         '(lambda ()
            (define-key ruby-mode-map "\C-m" 'reindent-then-newline-and-indent)))

;; rbenv path setting
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

;; off the magic comment
(setq ruby-insert-encoding-magic-comment nil)

;; ------------------------------------------------------------------
;; brief   : Ruby on Rails
;; note    : -
;; ------------------------------------------------------------------
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
