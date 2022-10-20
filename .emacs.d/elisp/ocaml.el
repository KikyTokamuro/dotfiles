;;; ocaml.el --- OCaml development settings
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Tuareg (Ocaml)
(use-package tuareg-mode
  :mode
  ("\\.ml[ily]?$" . tuareg-mode)
  :init
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (setq merlin-completion-with-doc t)
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

;; ocaml.el ends here
