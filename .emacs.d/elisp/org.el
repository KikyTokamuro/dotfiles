;;; org.el --- Org mode settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Org-mode
(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-html-validation-link nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((php        . t)
     (python     . t)
     (perl       . t)
     (C          . t)
     (awk        . t)
     (lisp       . t)
     (scheme     . t)
     (shell      . t)
     (emacs-lisp . t)
     (js         . t))))

;; org.el ends here
