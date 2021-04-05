(package-initialize)

;; Init file for custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Install selected packages 
(unless (cl-every 'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages))

;; PATH settings
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)

;; Show-paren-mode settings
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Electric-modes settings
(electric-pair-mode    1)
(electric-indent-mode -1)

;; Delete selection
(delete-selection-mode t)

;; Disable GUI components
(tooltip-mode       -1)
(menu-bar-mode       t)
(blink-cursor-mode nil)

;; Bell off
(setq ring-bell-function 'ignore)

;; Linum
(require 'linum)
(global-linum-mode  t)
(setq linum-format " %d")

;; Load average off
(setq display-time-default-load-average nil)

;; Line wrapping
(setq word-wrap          t)
(global-visual-line-mode t)

;; Indent settings
(global-set-key (kbd "RET") 'newline-and-indent)

;; Buffers
(global-set-key (kbd "C-x C-b") 'bs-show)

;; Scrolling settings
;; (setq scroll-step               1)
;; (setq scroll-margin             5)
;; (setq scroll-conservatively 10000)
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Color theme
(load-theme 'nord t)

;; Golang
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (go-guru-hl-identifier-mode)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (auto-complete-mode 1))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(with-eval-after-load 'go-mode
  (require 'go-guru)
  (require 'go-autocomplete))

;; C/C++
(defun my-c-mode-hook ()
  (setq c-default-style "linux"
        c-basic-offset 4
	indent-tabs-mode nil)
  (infer-indentation-style)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  (company-mode 1)
  (flycheck-mode 1)
  (irony-mode 1))

(with-eval-after-load 'my-c-mode-hook
  (require 'company)
  (require 'company-irony-c-headers)
  (require 'flycheck-irony))

(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)

