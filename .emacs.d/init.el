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
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Color theme
(load-theme 'nord t)

;; Use-package
(require 'use-package)

;; Smooth-scrolling
(use-package smooth-scrolling
  :init
  (smooth-scrolling-mode 1))

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 10)))
  (dashboard-setup-startup-hook))

;; Helm
(use-package helm
  :init
  (setq-default helm-M-x-fuzzy-match t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . 'helm-find-files)))

;; Auto-complete
(use-package auto-complete
  :commands auto-complete-mode
  :init
  (auto-complete-mode 1))

;; Go-mode
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq gofmt-command "goimports"
	indent-tabs-mode t)
  (use-package go-autocomplete)
  (auto-complete-mode)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
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
