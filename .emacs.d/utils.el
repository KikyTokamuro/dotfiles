;;; utils.el --- Others Emacs utils
;; Copyright (C) 2021 Arhangelsky Daniil (Kiky Tokamuro)
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

(defun system-uptime ()
  "Get system uptime."
  (interactive)
  (message
   (replace-regexp-in-string "\n$" ""
			     (shell-command-to-string "uptime"))))

(defun current-weather ()
  "Get current weather."
  (interactive)
  (message
   (replace-regexp-in-string "\n$" ""
			     (shell-command-to-string "curl -s 'wttr.in/?format=3'"))))

(provide 'utils)

;;; utils.el ends here