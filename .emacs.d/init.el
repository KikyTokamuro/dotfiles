;;; init.el --- Emacs init
;; Copyright (C) 2021-2022 Arhangelsky Daniil (Kiky Tokamuro)
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

(package-initialize)

;; Utils
(load-file "~/.emacs.d/utils.el")

;; Init file for custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; Custom themes path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Run server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Install use-package
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;;;;
;;;; Settings
;;;;

;; Set frame maximized
;;(toggle-frame-maximized)

;; Window title
(setq frame-title-format
      '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " — Emacs"))

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)

;; Show-paren-mode settings
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Electric-modes settings
(electric-pair-mode    1)
(electric-indent-mode  1)

;; Delete selection
(delete-selection-mode t)

;; Disable GUI components
(tooltip-mode       -1)
(menu-bar-mode       t)
(blink-cursor-mode nil)

;; Bell off
(setq ring-bell-function 'ignore)

;; Load average off
(setq display-time-default-load-average nil)

;; Line wrapping
(setq word-wrap         t)
(global-visual-line-mode t)

;; Indent settings
(global-set-key (kbd "RET") 'newline-and-indent)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Disable backup files
(setq make-backup-files nil)

;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; Use-package
(require 'use-package)

;; Flyspell
(use-package flyspell
  :ensure t
  :config
  (setq ispell-program-name "aspell"))

;;;;
;;;; UI and UI tools
;;;;

;; Highlight current line
(use-package hl-line
  :ensure t
  :hook ((prog-mode org-mode) . hl-line-mode))

;; Solarized theme (Colors)
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-selenized-light t)
  (set-face-attribute 'mode-line nil
		      :height 1.0
		      :overline nil
		      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
		      :height 1.0
		      :overline nil
		      :underline nil))

;; Telephone-line
(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-nil
	telephone-line-secondary-left-separator 'telephone-line-nil
	telephone-line-primary-right-separator 'telephone-line-nil
	telephone-line-secondary-right-separator 'telephone-line-nil)
  (telephone-line-mode 1))

;; Smooth-scrolling
(use-package smooth-scrolling
  :ensure t
  :init
  (smooth-scrolling-mode 1))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Linum
(use-package linum
  :config
  (setq linum-format " %d")
  :hook
  (prog-mode . linum-mode))

;; Diminish
(use-package diminish
  :ensure t)

;; Which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :diminish which-key-mode)

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 'logo
	dashboard-items '((recents . 5)))
  (dashboard-setup-startup-hook))

;; Treemacs
(use-package treemacs
  :ensure t
  :bind
  (("C-x C-n" . treemacs)))

;; Centaur-tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (setq centaur-tabs-set-bar 'over
	 centaur-tabs-set-modified-marker t
	 centaur-tabs-modifier-marker ".")
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

;; Helm
(use-package helm
  :ensure t
  :init
  (setq-default helm-M-x-fuzzy-match t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . 'helm-find-files)
   ("C-x C-b" . 'helm-buffers-list)))

;;;;
;;;; Programming
;;;;

;; Exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PERL5LIB"))
  :init
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; Org-mode
(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-html-validation-link nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python     . t)
     (perl       . t)
     (C          . t)
     (awk        . t)
     (lisp       . t)
     (scheme     . t)
     (shell      . t)
     (emacs-lisp . t)
     (js         . t))))

;; Web-beautify
;;; Install: npm -g install js-beautify
(use-package web-beautify
  :ensure t)

;; Company
(use-package company
  :ensure t
  :init
  (global-company-mode))

;; Go-mode hooks
(defun my-go-hooks ()
  "Golang hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (lsp))

;; Go-mode
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :hook
  (go-mode . my-go-hooks))

;; Sly (Common Lisp)
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"))

;; Flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

;; Lsp-mode
(use-package lsp-mode
  :commands lsp
  :ensure t
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

;; Lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t)

;; Ccls
(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "ccls"
	lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook
  ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp))))

;; Geiser
(use-package geiser-guile
  :mode
  ("\\.scm\\'" . scheme-mode)
  :config
  (setq geiser-active-implementations '(guile)))

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

;; Tuareg (Ocaml)
(use-package tuareg-mode
  :mode
  ("\\.ml[ily]?$" . tuareg-mode)
  :init
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (setq merlin-command "opam exec ocamlmerlin")))
  :hook
  (tuareg-mode . merlin-mode))

;; Utop (Ocaml)
(use-package utop
  :diminish utop-minor-mode
  :config
  (setq utop-command "opam exec -- utop -emacs")
  :hook
  (tuareg-mode . utop-minor-mode))

;; Company for Merlin
(use-package merlin-company)

;;;;
;;;; Web tools
;;;;

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

;;; init.el ends here
