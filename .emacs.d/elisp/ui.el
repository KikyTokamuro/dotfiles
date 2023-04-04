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

;; Helm
(use-package helm
  :ensure t
  :init
  (setq-default helm-M-x-fuzzy-match t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . 'helm-find-files)
   ("C-x C-b" . 'helm-buffers-list)))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode 1)
  :custom
  ((projectile-completion-system 'helm))
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/work/")
    (setq projectile-project-search-path '("~/work"))))

;; Helm-projectile
(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on)
  :config
  (setq projectile-switch-project-action 'helm-projectile))

;; ui.el ends here
