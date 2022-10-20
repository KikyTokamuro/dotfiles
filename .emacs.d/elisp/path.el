;;; path.el --- Path tools
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PERL5LIB"))
  :init
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; path.el ends here
