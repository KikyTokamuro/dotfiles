;;; server.el --- Emacs server
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

;; Run server
(require 'server)
(unless (server-running-p)
  (server-start))

;; server.el ends here
