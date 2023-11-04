;;; reddit-images.el --- Subreddit new images viewver

;; Author:  Daniil Arkhangelsky (Kiky Tokamuro) <kiky.tokamuro@yandex.ru>
;; Created: November 2023

;;; Commentary:
;; Simple code for view new images from subreddit

;;; Code:

(require 'json)
(require 'url)

(defun my/insert-image-from-url (url)
  "Insert image from URL."
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (let ((data (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
          (insert-image (create-image data nil t :width 200)))
      (kill-buffer buffer))))

(defun my/get-response-from-url (url)
  "Get data from URL."
  (let ((buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (unwind-protect
	  (progn
	    (goto-char (point-min))
	    (re-search-forward "^$")
	    (buffer-substring-no-properties (point) (point-max)))
	(kill-buffer buffer)))))

(defun my/normal-image-url-p (url)
  "Check image URL."
  (and (not (string-match-p "redgifs" url))
       (or (string-suffix-p ".png" url)
	   (string-suffix-p ".gif" url)
	   (string-suffix-p ".jpg" url)
	   (string-suffix-p ".jpeg" url))))

(defun my/get-new-images-from-subreddit (sub)
  "Get list of SUB new images."
  (let* ((reddit-url "https://www.reddit.com")
	 (url (concat reddit-url "/r/" sub "/new.json"))
	 (json-str (my/get-response-from-url url))
	 (json-object (json-read-from-string json-str)))
    (remq nil (mapcar
	       (lambda (child)
		 (let* ((data (cdr (assoc 'data child)))
			(image-url (cdr (assoc 'url_overridden_by_dest data))))
		   (when (my/normal-image-url-p image-url)
		     `((title . ,(cdr (assoc 'title data)))
		       (image . ,image-url)
		       (post . ,(concat reddit-url (cdr (assoc 'permalink data))))))))
	       (cdr (assoc 'children (cdr (assoc 'data json-object))))))))

(defun my/show-reddit-images (sub)
  "Show new image from SUB."
  (interactive "sEnter subreddit: ")
  (let ((buffer (generate-new-buffer (format "*Images [%s]*" sub))))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (item (my/get-new-images-from-subreddit sub))
	(dolist (key '(title post image))
	  (insert (concat
		   (propertize (capitalize (symbol-name key)) 'face 'bold)
		   ": "
		   (cdr (assoc key item))
		   "\n")))
	(my/insert-image-from-url (cdr (assoc 'image item)))
	(insert "\n\n")))
    (switch-to-buffer buffer)))

(provide 'reddit-images)

;;; reddit-images.el ends here
