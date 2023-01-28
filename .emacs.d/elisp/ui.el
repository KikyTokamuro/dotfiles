;;; ui.el --- UI tools
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Highlight current line
(use-package hl-line
  :ensure t
  :hook ((prog-mode org-mode) . hl-line-mode))

;; Modus themes 
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t))

;; Solarized theme (Colors)
;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-selenized-light t)
;;   (set-face-attribute 'mode-line nil
;; 		      :height 1.0
;; 		      :overline nil
;; 		      :underline nil)
;;   (set-face-attribute 'mode-line-inactive nil
;; 		      :height 1.0
;; 		      :overline nil
;; 		      :underline nil))

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

;; Treemacs
(use-package treemacs
  :ensure t
  :bind
  (("C-x C-n" . treemacs)))

;; Centaur-tabs
;; (use-package centaur-tabs
;;   :ensure t
;;   :demand
;;   :config
;;   (defun my-hide-centaur-tabs (buffer)
;;     "Hide tabs with * in BUFFER name."
;;     (let ((name (format "%s" buffer)))
;;       (or
;;        (string-prefix-p "*" name)
;;        (centaur-tabs-hide-tab buffer))))
;;   (setq centaur-tabs-set-bar 'over
;; 	centaur-tabs-set-modified-marker t
;; 	centaur-tabs-modifier-marker "."
;; 	centaur-tabs-hide-tab-function 'my-hide-centaur-tabs)
;;   (centaur-tabs-headline-match)
;;   (centaur-tabs-mode t))

;; Helm
(use-package helm
  :ensure t
  :init
  (setq-default helm-M-x-fuzzy-match t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . 'helm-find-files)
   ("C-x C-b" . 'helm-buffers-list)))

;; ui.el ends here
