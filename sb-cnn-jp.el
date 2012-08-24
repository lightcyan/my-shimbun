;;; sb-cnn-jp.el --- shimbun backend for CNN.co.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>
;; Keywords: news

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

;;; Commentary:

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-cnn-jp
		   (shimbun-japanese-newspaper shimbun-text) ())

(defvar shimbun-cnn-jp-top-level-domain "cnn.co.jp"
  "Name of the top level domain for the CNN.co.jp.")

(defvar shimbun-cnn-jp-url
  (concat "http://www." shimbun-cnn-jp-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-cnn-jp-url-coding-system 'utf-8-dos
  "Coding system used to encode URLs containing non-ASCII letters.")

(defvar shimbun-cnn-jp-group-table
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\"/"
		    ;; 1. url
		    "\\(%s/"
		    ;; 2. serial number
		    "\\([0-9]+\\)"
		    "\\.html\\)"
		    "\"" s0 ">" s0
		    ;; 3. subject
		    "\\([^<>]+\\)"
		    s0 "</a>[\n\r]*" s0 "<span class=\"datetime\">"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    "/"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    ;; 6. year
		    ""
		    )
		   1 6 4 5 2 3)))
    `(("world" "ワールド" "" ,@default)
      ("usa" "USA" "" ,@default)
      ("business" "ビジネス" "" ,@default)
      ("showbiz" "エンタメ" "" ,@default)
      ("travel" "トラベル" "" ,@default)
      ("fringe" "こぼれ話" "" ,@default)
      ))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where index pages and regexps may contain the
\"%s\" token which is replaced with group names, numbers point to the
search result in order of [0]a url, [1,2]a serial number, [3]a subject,
\[4]a month, [5]a day, [6]an hour:minute and [7,8]an extra keyword.")

(defvar shimbun-cnn-jp-content-start
  "<!-- block_story -->[\t\n\ ]*")
(defvar shimbun-cnn-jp-content-end
  "<!-- //block_story -->[\t\n ]*")

(defvar shimbun-cnn-jp-x-face-alist nil)

(defvar shimbun-cnn-jp-expiration-days 6)

(luna-define-method initialize-instance :after ((shimbun shimbun-cnn-jp)
						 &rest init-args)
  (shimbun-set-server-name-internal shimbun "CNN-jp")
  (shimbun-set-from-address-internal shimbun
				     (concat "webmaster@www."
					     shimbun-cnn-jp-top-level-domain))
  ;; To share class variables between `shimbun-cnn-jp' and its
  ;; successor `shimbun-cnn-jp-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-cnn-jp-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun shimbun-cnn-jp-expiration-days)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-cnn-jp))
  (mapcar 'car shimbun-cnn-jp-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-cnn-jp))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-cnn-jp-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-cnn-jp))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat shimbun-cnn-jp-url group
	    (format (nth 2 (assoc group shimbun-cnn-jp-group-table))
		    group))))

(defun shimbun-cnn-jp-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-from-address shimbun))
	(case-fold-search t)
	regexp numbers cyear cmonth month year day serial num extra headers
	kansai-special)
    (setq regexp (assoc group shimbun-cnn-jp-group-table)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp) (regexp-quote group))
	  cyear (decode-time)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    (while (re-search-forward regexp nil t)
      (setq month (string-to-number (match-string (nth 2 numbers)))
	    year cyear
	    day (string-to-number (match-string (nth 3 numbers)))
	    serial (if (and (setq num (nth 4 numbers))
			    (match-beginning num))
		       (format "%d%02d%02d.%s"
			       year month day (match-string num))
		     (mapconcat 'identity
				(save-match-data
				  (split-string
				   (downcase (match-string (nth 2 numbers)))
				   "/"))
				"."))
; 	    extra (or (and (setq num (nth 7 numbers))
; 			   (match-beginning num)
; 			   (match-string num))
; 		      (and (setq num (nth 8 numbers))
; 			   (match-beginning num)
; 			   (match-string num)))
	    )
      (push (shimbun-make-header
	     ;; number
	     0
	     ;; subject
	     (shimbun-mime-encode-string
	      (match-string (nth 5 numbers)))
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string
	      year month day (when (and (setq num (nth 6 numbers))
					(match-beginning num))
			       (match-string num)))
	     ;; id
; 	     (if extra
; 		 (concat "<" serial "%" extra "." group "."
; 			 shimbun-cnn-jp-top-level-domain ">")
	     (concat "<" serial "%" group "."
		     shimbun-cnn-jp-top-level-domain ">")
; 	     )
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (shimbun-expand-url (match-string (nth 0 numbers))
				 shimbun-cnn-jp-url))
	    headers))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-cnn-jp)
					 &optional range)
  (shimbun-cnn-jp-get-headers shimbun))

(defun shimbun-cnn-jp-adjust-date-header (shimbun header)
  "Adjust a date header if there is a correct information available."
  (let ((case-fold-search t)
	date start end)
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t)
	       (progn
		 (setq end (match-beginning 0))
		 (beginning-of-buffer)
		 (re-search-forward
		  (concat
		   "\\(20[0-9][0-9]\\)\\.\\([01][0-9]\\)\\.\\([0-3][0-9]\\)[^0-9]+"
		   "\\([012][0-9]:[0-5][0-9]\\)"
		   )
		  end t)))
      (shimbun-header-set-date 
       header
       (shimbun-make-date-string
	(string-to-number (match-string 1))
	(string-to-number (match-string 2))
	(string-to-number (match-string 3))
	(match-string 4)))))
  (goto-char (point-min)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-cnn-jp)
						   header)
  (shimbun-cnn-jp-adjust-date-header shimbun header))

(provide 'sb-cnn-jp)

;;; sb-cnn-jp.el ends here
