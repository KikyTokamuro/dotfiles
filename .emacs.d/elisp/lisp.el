;;; lisp.el --- Lisp development settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Sly (Common Lisp)
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"))

;; Geiser
(use-package geiser-guile
  :mode
  ("\\.scm\\'" . scheme-mode)
  :config
  (setq geiser-active-implementations '(guile)))

;; Racket
(use-package racket-mode
  :ensure t
  :hook
  (racket-mode . racket-xp-mode))

;; lisp.el ends here
