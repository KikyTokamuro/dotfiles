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

(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (if (executable-find "perltidy")
      (save-excursion
        (shell-command-on-region (point) (mark) "perltidy -q" nil t))
    (message "Unable to find perltidy")))

(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

(defun perltidy-buffer ()
  "Run perltidy on current buffer."
  (interactive)
  (if (executable-find "perltidy")
      (let ((where-i-was (point)))
        (shell-command-on-region (point-min) (point-max) "perltidy -q" nil t)
        (goto-char where-i-was))
    (message "Unable to find perltidy")))

(provide 'utils)

;;; utils.el ends here
