;;; sb-opml-rss.el --- shimbun backend for american-rss
;; Modified from sb-reuters-rss.el by lightcyan <lightcyan.area51@gmail.com>
;; URL: http://d.hatena.ne.jp/lightcyan/
;; Below is the original copyright notice of sb-reuters-rss.el.
;;
;; Modified from sb-cnet-rss.el by Hiroyuki KUROSAKI <noir@st.rim.or.jp>
;; Below is the original copyright notice of sb-cnet-rss.el.
;;
;; Copyright (C) 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Jun 14, 2003

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; How to use with Mew.:
;; 1.  Save google-reader-subscriptions.xml to `shimbun-opml-rss-file',
;; 2.  Add ("opml-rss" ("opml-rss.0" . last)) to your `mew-shimbun-folder-groups',
;; 3.  Make "~/Mail/shimbun/opml-rss" folder. 
;; 4.  M-x `mew-summary-visit-folder'[RET]"+shimbun/opml-rss", then M-x `mew-shimbun-retrieve'.
;; 5.  Valid value of `mew-shimbun-folder-groups' for the opml-rss generated in a new buffer.
;; 6.  Save it to ~/.mew.opml.el or somewhere you like, and load it from somewhre.
;; 7.  Reboot Emacsen, And do M-x `mew-shimbun-retrieve-all'.


;;; Code:

(require 'shimbun)
(require 'sb-rss)
(require 'xml)

(luna-define-class shimbun-opml-rss (shimbun-rss) ())

(defvar shimbun-opml-rss-file "~/.w3m/google-reader-subscriptions.xml"
  "A opml file from Google reader")
(defvar shimbun-opml-rss-mew-file ".mew-opml.el"
  "A file to set `shimbun-opml-rss-groups-max' and to append `mew-shimbun-folder-groups'
Save `sb-opml-rss-initialize' Resulet in this file")
(defvar shimbun-opml-rss-mew-path (expand-file-name (format "~/%s" shimbun-opml-rss-mew-file))
"Path to a file `shimbun-opml-rss-mew-file'.")
(defvar shimbun-opml-rss-buffer " *w3m opml*"
"temporary buffer to be used readind opml file.")
(defvar shimbun-opml-rss-groups-max 0
"Number of how many feed you want to retrieve. This is set in `shimbun-opml-rss-mew-file'.")
(defvar shimbun-opml-rss-from-address  "foo@baa.baz")
(defvar shimbun-opml-rss-content-start nil)
(defvar shimbun-opml-rss-content-end nil)
(defvar shimbun-opml-rss-groups nil 
"A list of the row such as \"1\" \"2\" \"3\"... up to shimbun-opml-rss-groups-max.
nil means set automatically when sb-opml-rss is loaded.")
(if (not shimbun-opml-rss-groups)
;; Initialize shimbun-opml-rss-groups
    (progn
      (setq shimbun-opml-rss-groups ())
      (let ((i shimbun-opml-rss-groups-max))
	(while (< -1 i)
	  (setq shimbun-opml-rss-groups
		(cons (format "%s" i) shimbun-opml-rss-groups))
	  (setq i (1- i))))))

(defun sb-opml-rss-initialize ()
  "Generate value of `mew-shimbun-folder-groups' according to `shimbun-opml-rss-file'"
  (interactive)
  (let* ((root (xml-parse-file (expand-file-name "~/.w3m/google-reader-subscriptions.xml")))
	 (posts (car root)) (post (xml-node-children posts))	 
	 (i 1) folder-groups-for-opml opml-rss-group)
    (setq outlines (cdr (cdr (assoc 'body post))))
    (while outlines
      (setq folder (pop outlines))
      (if (listp folder)
	  (progn 
	    (pop folder)
	    (setq folder-title (cdr (assoc 'title (car folder)) ))
	    (pop folder)
	    (if (listp folder)
		(progn
		  (setq opml-rss-group (list))
		  (while folder
		    (setq site (pop folder))
		    (if (listp site)
			(progn
			  (pop site)
			  (setq site-title (cdr (assoc 'title (car site)) ))
			  (if (not opml-rss-group)
			      (setq opml-rss-group (list (cons (format "\"opml-rss.%d\"" i) 'all)))
			    (setq opml-rss-group (append opml-rss-group (list (cons (format "\"opml-rss.%d\"" i) 'all)))))
			  (setq i (1+ i)))))
		  (if (not folder-groups-for-opml)
		      (setq folder-groups-for-opml 
			    (list (cons (format "\"opml-rss/%s\"" folder-title) opml-rss-group)))
		    (setq folder-groups-for-opml 
			  (append folder-groups-for-opml 
				  (list (cons (format "\"opml-rss/%s\"" folder-title) opml-rss-group))))))))))
    (sb-opml-rss-initialize-display i folder-groups-for-opml)))

(defun sb-opml-rss-initialize-display (max result)
"Display `sb-opml-rss-initialize' result"
(if (file-exists-p shimbun-opml-rss-mew-path)
    (if (not (yes-or-no-p (format "%s is already exist. continue anyway?" shimbun-opml-rss-mew-path)))
	(error "User abort.")))
(find-file shimbun-opml-rss-mew-path)
(setq opml-buffer (get-buffer shimbun-opml-rss-mew-path))
(if (string= shimbun-opml-rss-mew-file (buffer-name))
    (progn
      ;; print
      (insert ";;; Save this buffer and load that file from your .mew file.\n")
      (insert (format ";;; e.g. Save in %s. (load \"%s\"). Then reboot Emacsen.\n" 
		      shimbun-opml-rss-mew-path shimbun-opml-rss-mew-path))
      (insert (format "(setq shimbun-opml-rss-groups-max %d)\n" max))
      (insert "(setq mew-shimbun-folder-groups (append mew-shimbun-folder-groups\n")
      (insert (format "'%s" result))
      (insert "))")
      ;; format output
      (goto-char (point-min))
      (replace-string "all) (" "all)
  (")
      (goto-char (point-min))
      (replace-string "\" (" "\"
  (")
      (goto-char (point-min))
      (replace-string ")) (" "))
 (")
      (lisp-mode)
      (goto-char (point-min))
      (switch-to-buffer shimbun-opml-rss-mew-file))
  (message "Something is wrong. Load sb-opml-rss.el then type M-x sb-opml-rss-initialize, and see shimbun-opml-rss-mew-file")
  ))

(luna-define-method shimbun-index-url ((shimbun shimbun-opml-rss))
;;;<DEBUG>
;;   (shimbun-index-url-1 shimbun))
;; (defun shimbun-index-url-1 (shimbun)
;;;</DEBUG>
  (if (and (string= "opml-rss.0" (car (nth 1 (assoc-string "opml-rss" mew-shimbun-folder-groups))))
	   (not(file-exists-p shimbun-opml-rss-mew-path)));; I wanna check if `mew-shimbun-folder-groups' changed but how?
      (progn
	(sb-opml-rss-initialize)
	(error (format "Please edit .mew file according to %s" shimbun-opml-rss-mew-path)))
    (let ((num (shimbun-current-group-internal shimbun))
	  opml-buffer)
      (save-excursion
	(setq opml-buffer (get-buffer shimbun-opml-rss-buffer))
	(if (not opml-buffer)
	    (progn
	      (set-buffer (get-buffer-create shimbun-opml-rss-buffer))
	      (insert-file-contents shimbun-opml-rss-file))
	  (progn
	    (set-buffer shimbun-opml-rss-buffer)))
	(goto-char (point-min))
	(let ((i 0))
	  (while (and (< i (string-to-number num))
		      (re-search-forward "xmlUrl=\"\\(http://[^\"]+\\)\"" nil t)
		      (setq i (1+ i))))
	  ;;(message "opml-rss:[%d/%s]%s" i num (match-string 1))
	  (match-string 1))))))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-opml-rss) url date)
  (unless (string-match "https?://\\(.+\\)" url)
    (error "Cannot find message-id base"))
  (concat (match-string-no-properties 1 url) ""))

(provide 'sb-opml-rss)

;;; sb-opml-rss.el ends here
