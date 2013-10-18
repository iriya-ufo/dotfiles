;;; coffee-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (coffee-mode) "coffee-mode" "coffee-mode.el" (21089
;;;;;;  34095 134496 808000))
;;; Generated autoloads from coffee-mode.el

(autoload 'coffee-mode "coffee-mode" "\
Major mode for editing CoffeeScript.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.iced\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("Cakefile\\'" . coffee-mode))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

;;;***

;;;### (autoloads nil nil ("coffee-mode-pkg.el") (21089 34095 214364
;;;;;;  314000))

;;;***

(provide 'coffee-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; coffee-mode-autoloads.el ends here
