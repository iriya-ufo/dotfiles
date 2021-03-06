;;; python.el --- initializes python modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python-mode)

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                  interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
