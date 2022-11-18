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

(defun convert-to-array (start end quote)
    "Quotes the words and separate it with a newline and comma.
Used START and END region, and QUOTE symbol."
    (interactive "r\nMQuote: ")
    (let ((insertion
	   (mapconcat
	    (lambda (x) (format "%s%s%s" quote x quote))
	    (split-string (buffer-substring start end))
	    ",\n")))
      (delete-region start end)
      (insert insertion)))

(provide 'utils)

;;; utils.el ends here
