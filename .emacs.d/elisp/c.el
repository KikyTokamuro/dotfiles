;;; c.el --- C/C++ development settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

(defun my-c/c++-mode-hook ()
  "C/C++ mode hook."
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (eglot-ensure))

(add-hook 'c-mode-hook 'my-c/c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c/c++-mode-hook)

;; c.el ends here
