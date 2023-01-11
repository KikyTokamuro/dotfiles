;;; init.el --- Emacs init
;; Copyright (C) 2021-2022 Arhangelsky Daniil (Kiky Tokamuro)
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

(package-initialize)

;; Init file for custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; Custom themes path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Load modules
(let ((my-modules-dir "~/.emacs.d/elisp/")
      (modules '(server packages settings ui
		 path spell company lsp
		 flycheck org webdev golang
		 lisp c perl python php ocaml
		 web-tools utils)))
  (dolist (module modules)
    (let ((file (concat my-modules-dir (symbol-name module) ".el")))
      (if (file-exists-p file)
	  (load-file file)))))

;;; init.el ends here
