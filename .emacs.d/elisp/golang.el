;;; golang.el --- Golang development settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Go-mode hooks
(defun my-go-hooks ()
  "Golang hooks."
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (eglot-ensure))

;; Go-mode
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :hook
  (go-mode . my-go-hooks))

;; golang.el ends here
