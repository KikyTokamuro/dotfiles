;;; utils.el --- Others Emacs utils
;; Copyright (C) 2021 Arhangelsky Daniil (Kiky Tokamuro)
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

(defun system-uptime ()
  "Get system uptime."
  (interactive)
  (message (shell-command-to-string "uptime")))

(provide 'utils)

;;; utils.el ends here