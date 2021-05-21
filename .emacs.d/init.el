(package-initialize)

;; Init file for custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Install selected packages 
;; (unless (cl-every 'package-installed-p package-selected-packages)
;;   (package-refresh-contents)
;;   (package-install-selected-packages))

;; Install use-package
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

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

;; Disable backup files
(setq make-backup-files nil)
 
;; Use-package
(require 'use-package)

;; Nord theme (Colors)
(use-package nord-theme
  :ensure t
  :init
  (load-theme 'nord t))

;; Smooth-scrolling
(use-package smooth-scrolling
  :ensure t
  :init
  (smooth-scrolling-mode 1))

;; Linum
(use-package linum
  :init
  (global-linum-mode t)
  :config
  (setq linum-format " %d"))

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
  (setq dashboard-items '((recents . 10)))
  (dashboard-setup-startup-hook))

;; Dired-sidebar
(use-package dired-sidebar
  :ensure t
  :bind
  (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands
  (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-width 25))

;; Helm
(use-package helm
  :ensure t
  :init
  (setq-default helm-M-x-fuzzy-match t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . 'helm-find-files)))

;; Exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
    (exec-path-from-shell-initialize)))

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

(use-package utop
  :diminish utop-minor-mode
  :config
  (setq utop-command "opam config exec -- utop -emacs")
  :hook
  (tuareg-mode . utop-minor-mode))
