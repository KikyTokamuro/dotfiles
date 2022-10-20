;;; web-tools.el --- Various web tools (Translate, Browser, ...)
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Eww-lnum
(use-package eww-lnum
  :ensure t)

;; Eww
(use-package eww
  :bind
  (:map eww-mode-map
	("f" . eww-lnum-follow)))

;; Google translate
(use-package google-translate
  :ensure t
  :functions (google-translate--search-tkk)
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130)))

;; web-tools.el ends here
