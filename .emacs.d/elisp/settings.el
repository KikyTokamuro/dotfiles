;;; settings.el --- Others settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

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
(menu-bar-mode      -1)
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

;; settings.el ends here
