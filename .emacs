;; Packages
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)

;; Show-paren-mode settings
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Electric-modes settings
(electric-pair-mode    1)
(electric-indent-mode -1)

;; Delete selection
(delete-selection-mode t)

;; Disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(blink-cursor-mode -1)

;; Bell off
(setq ring-bell-function 'ignore)

;; Linum plugin
(require 'linum)
(line-number-mode   t)
(global-linum-mode  t)
(column-number-mode t)
(setq linum-format " %d")

;; Load average off
(setq display-time-default-load-average nil)

;; Line wrapping
(setq word-wrap          t)
(global-visual-line-mode t)

;; Buffer Selection and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "<f2>") 'bs-show)

;; Indent settings
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Scrolling settings
(setq scroll-step               1)
(setq scroll-margin             5)
(setq scroll-conservatively 10000)

;; Clipboard settings
(setq x-select-enable-clipboard t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(package-selected-packages '(paredit company geiser jedi atom-one-dark-theme))
 '(size-indication-mode nil))

;; Font settings
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 98 :width normal)))))

;; Color theme
(load-theme 'atom-one-dark t)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Paredit
(autoload 'enable-paredit-mode 
	  "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Python
(add-hook 'python-mode-hook 'jedi:setup)

;; Racket
(require 'geiser-mode)

(defun geiser-buffer () geiser-mode)

(defun geiser-save-buffers-and-enter-module ()
  (interactive)
  (save-some-buffers t 'geiser-buffer)
  (geiser-mode-switch-to-repl-and-enter))

(define-key geiser-mode-map "\C-c\C-a"
  'geiser-save-buffers-and-enter-module)
