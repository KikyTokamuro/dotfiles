;;; python.el --- Python development settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Python-mode
;;; pip install python-language-server[all]
;;; pip install pyls-black pyls-isort pyls-mypy
;;; pip install future
;;; 
(use-package python-mode
  :hook
  (python-mode . eglot-ensure))

;; python.el ends here
