;;; init.el --- Emacs init
;; Copyright (C) 2021 Arhangelsky Daniil (Kiky Tokamuro)
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

(package-initialize)

;; Utils
(load-file "~/.emacs.d/utils.el")

;; Init file for custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

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
(setq word-wrap          t)
(global-visual-line-mode t)

;; Indent settings
(global-set-key (kbd "RET") 'newline-and-indent)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Disable backup files
(setq make-backup-files nil)

;; Use-package
(require 'use-package)

;;;;
;;;; UI and UI tools
;;;;

;; Solarized theme (Colors)
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-selenized-light t)
  (set-face-attribute 'mode-line nil :height 1.0 :overline nil :underline nil))

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
  ;; :init
  ;; (global-linum-mode t)
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
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 5)))
  (dashboard-setup-startup-hook))

;; Neotree
(use-package neotree
  :ensure t
  :bind
  (("C-x C-n" . neotree-toggle))
  :init
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 25)
  (setq neo-smart-open t))

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
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PERL5LIB"))
    (exec-path-from-shell-initialize)))

;; Org-mode
(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (perl . t)
     (C . t)
     (awk . t)
     (lisp . t)
     (scheme . t)
     (shell . t)
     (emacs-lisp . t)
     (js . t))))

;; Web-beautify
;;; Install: npm -g install js-beautify
(use-package web-beautify
  :ensure t)

;; Company
(use-package company
  :ensure t
  :init
  (global-company-mode))

;; Company-go
(use-package company-go
  :ensure t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

;; Go-mode (Golang)
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq gofmt-command "goimports"
	indent-tabs-mode t)
  :hook
  (go-mode . (lambda () (add-hook 'before-save-hook 'gofmt-before-save)))
  :bind
  (:map go-mode-map
	("\C-c \C-c" . compile)
        ("\C-c \C-g" . go-goto-imports)
        ("\C-c \C-k" . godoc)
        ("M-j" . pop-tag-mark)
        ("M-k" . godef-jump)))

;; Go-guru
(use-package go-guru
  :ensure t
  :hook
  (go-mode . go-guru-hl-identifier-mode))

;; Sly (Common Lisp)
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"))

;; Tuareg
(use-package tuareg
  :mode
  ("\\.ml[ily]?$" . tuareg-mode))

;; Merlin (Ocaml)
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (setq merlin-command 'opam)))

;; Utop
(use-package utop
  :diminish utop-minor-mode
  :config
  (setq utop-command "opam config exec -- utop -emacs")
  :hook
  (tuareg-mode . utop-minor-mode))

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
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook
  ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp))))

;; Racket
;; (use-package racket-mode
;;   :mode
;;   ("\\.rkt[dl]?\\'" . racket-mode)
;;   :hook
;;   (racket-mode . racket-xp-mode))

;; Geiser
(use-package geiser-guile
  :mode
  ("\\.scm\\'" . scheme-mode)
  :config
  (setq geiser-active-implementations '(guile)))

;; Perl
(use-package cperl-mode
  :defer t
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
  (cperl-mode . lsp)
  (cperl-mode . (lambda () (add-hook 'before-save-hook 'perltidy-buffer))))

;; Raku
(use-package raku-mode
  :defer t
  :init
  (defalias 'perl6-mode #'raku-mode))

;; Elixir
(use-package elixir-mode
  :mode
  ("\\.ex\\'" . elixir-mode)
  :init
  (add-to-list 'exec-path (expand-file-name "~/elixir-ls/release"))
  :hook
  (elixir-mode . lsp)
  (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format))))

;; PHP
(use-package php-mode
  :ensure t
  :mode
  ("\\.php\\'" . php-mode)
  :hook
  (php-mode . lsp))

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

;; Elfeed -- feed reader
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'("http://nullprogram.com/feed/"
	  "https://planet.emacslife.com/atom.xml"
	  "https://lobste.rs/rss"
	  "https://hnrss.org/newest"
	  "https://www.reddit.com/r/programming/.rss"
	  "https://www.reddit.com/r/lisp/.rss"
	  "https://www.reddit.com/r/emacs/.rss"
	  "https://www.reddit.com/r/perl/.rss"
	  "https://reddit.com/r/lispmachine/.rss"
	  "https://reddit.com/r/rakulang/.rss"
	  "https://reddit.com/r/C_Programming/.rss"
	  "https://reddit.com/r/cpp/.rss"
	  "https://reddit.com/r/coding/.rss"
	  "https://reddit.com/r/openbsd/.rss"
	  "https://reddit.com/r/freebsd/.rss"
	  "https://reddit.com/r/scheme/.rss"
	  "https://reddit.com/r/PHP/.rss"
	  "https://reddit.com/r/ProgrammingLanguages/.rss")))

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
