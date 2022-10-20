;;; c.el --- C/C++ development settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Ccls
(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "ccls"
	lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook
  ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp))))

;; c.el ends here
