;;; php.el --- PHP development settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; PHP
(use-package php-mode
  :ensure t
  :mode
  ("\\.php\\'" . php-mode)
  :init
  (add-to-list 'load-path "~/.emacs.d/emacs-php-doc-block")
  (require 'php-doc-block)
  :hook
  (php-mode . lsp))

;; php.el ends here
