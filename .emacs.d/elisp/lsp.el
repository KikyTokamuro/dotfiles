;;; lsp.el --- LSP settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Lsp-mode
(use-package lsp-mode
  :commands lsp
  :ensure t
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

;; Lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t)

;; lsp.el ends here
