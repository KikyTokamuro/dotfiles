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
  :hook
  (cperl-mode . eglot-ensure))

(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (if (executable-find "perltidy")
      (save-excursion
        (shell-command-on-region (point) (mark) "perltidy -q" nil t))
    (message "Unable to find perltidy")))

(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

(defun perltidy-buffer ()
  "Run perltidy on current buffer."
  (interactive)
  (if (executable-find "perltidy")
      (let ((where-i-was (point)))
        (shell-command-on-region (point-min) (point-max) "perltidy -q" nil t)
        (goto-char where-i-was))
    (message "Unable to find perltidy")))

;; perl.el ends here
