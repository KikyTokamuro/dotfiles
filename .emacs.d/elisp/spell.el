;;; spell.el --- Spell tools
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Flyspell
(use-package flyspell
  :ensure t
  :config
  (setq ispell-program-name "aspell"))

;; spell.el ends here
