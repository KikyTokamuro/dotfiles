;;; perl.el --- Perl development settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Perl
(use-package cperl-mode
  :ensure t
  :init (defalias 'perl-mode 'cperl-mode)
  :config
  (setq cperl-highlight-variables-indiscriminately t
	cperl-indent-level 4
        cperl-tab-always-indent nil
        cperl-continued-statement-offset 0
        cperl-indent-parens-as-block t
        cperl-close-paren-offset -4
        cperl-electric-keywords t
        cperl-label-offset 0)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("node" "/home/kiky/work/PerlNavigator/server/out/server.js" "--stdio"))
		    :major-modes '(cperl-mode perl-mode)
		    :priority 10
		    :server-id 'perl-ls))
  :hook
  (cperl-mode . lsp))

;; perl.el ends here
