;;; python.el --- Python development settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Python-mode
;;; pip install python-language-server[all]
;;; pip install pyls-black pyls-isort pyls-mypy
;;; pip install future
(use-package python-mode
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  :hook
  (python-mode . lsp))

;; python.el ends here
