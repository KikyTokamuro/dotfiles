;; Packages
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)

;; Show-paren-mode settings
(setq show-paren-style 'expression)

;; Electric-modes settings
(electric-pair-mode    1)
(electric-indent-mode -1)

;; Delete selection
(delete-selection-mode t)

;; Disable GUI components
(tooltip-mode       -1)
(menu-bar-mode       t)
(scroll-bar-mode    -1)
(blink-cursor-mode nil)

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
;; (setq-default tab-width          4)
;; (setq-default c-basic-offset     4)
;; (setq-default standart-indent    4)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Scrolling settings
(setq scroll-step               1)
(setq scroll-margin             5)
(setq scroll-conservatively 10000)

;; Clipboard settings
(setq x-select-enable-clipboard t)

(custom-set-variables
 '(cua-mode t nil (cua-base))
 '(package-selected-packages
   '(go-guru flymake-go go-autocomplete auto-complete exec-path-from-shell go-mode company))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;; Font settings
(custom-set-faces
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 113 :width normal)))))

;; Color theme
(load-theme 'misterioso t)

;; PATH 
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; go-mode loads
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
