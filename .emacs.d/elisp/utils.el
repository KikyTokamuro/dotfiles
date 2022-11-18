;;; utils.el --- Others Emacs utils
;; Copyright (C) 2021-2022 Arhangelsky Daniil (Kiky Tokamuro)
;; Author: Arhangelsky Daniil (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;;; Commentary:
;;; Code:

(require 'request)

(defun system-uptime ()
  "Get system uptime."
  (interactive)
  (message
    (string-trim (shell-command-to-string "uptime"))))

(defun current-weather ()
  "Get current weather."
  (interactive)
  (request "https://wttr.in/?format=4"
    :sync t
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(message (string-trim data))))))

(provide 'utils)

;;; utils.el ends here
