;;; sb-tanakanews.el --- shimbun backend for www.tanakanews.co.jp -*- coding: iso-2022-7bit; -*-

;; Author: lightcyan
;; Keywords: news

;;; Copyright:

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

;; Original code was sb-yomiuri.el(emacs-w3m-1.3.5) written by 
;; TSUCHIYA Masatoshi <tsuchiya@xxxxxxxxxx>,
;; Yuuichi Teranishi  <teranisi@xxxxxxxxxx>
;; Katsumi Yamaoka    <yamaoka@xxxxxxx>

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-tanakanews
		   (shimbun-japanese-newspaper shimbun-text) ())

(defvar shimbun-tanakanews-top-level-domain "tanakanews.com"
  "Name of the top level domain for the tanakanews Web.")

(defvar shimbun-tanakanews-url
  (concat "http://" shimbun-tanakanews-top-level-domain "/" )
  "Name of the parent url.")

(defvar shimbun-tanakanews-group-table
  (let ((default
	  (list
	   (concat
	    "<[\t\n ]*a[\t\n ]+href[\t\n ]*=[\t\n ]*\""
;; 	    "http://tanakanews.com/"
	    ;; 1. url
	    "\\([0-9][0-9]"
;;	    "\\(g?"
	    ;; 4. month
	    "\\([01][0-9]\\)"
	    ;; 5. day
	    "\\([0-3][0-9]\\)"
	    ;; 2. serial number
	    "\\([a-zA-Z0-9]+\\)" "\\.html?\\)\">"
	    ;; 3. subject
;;	    "\\(.+\\)" "[\t\n ]*<[\t\n ]*/font[\t\n ]*>[\t\n ]*" "[\t\n ]*<[\t\n ]*/a[\t\n ]*>[\t\n ]*"
	    "\\(.+\\)" "[\t\n ]*" "[\t\n ]*<[\t\n ]*/a[\t\n ]*>[\t\n ]*"
	    )
	   1 4 5 2 3)))
    `(("news" "ニュース" "" ,@default)))
  "test")
(defvar shimbun-tanakanews-content-start "<div id=\"main\">[\t\n ]")

(defvar shimbun-tanakanews-content-end  "\n<div id=\"navi\">")

(defvar shimbun-tanakanews-x-face-alist nil)

(defvar shimbun-tanakanews-expiration-days 7)

(luna-define-method initialize-instance :after ((shimbun shimbun-tanakanews)
						 &rest init-args)
  (shimbun-set-server-name-internal shimbun "田中宇")
  (shimbun-set-from-address-internal shimbun
				     (concat "webmaster@"
					     shimbun-tanakanews-top-level-domain))
  ;; To share class variables between `shimbun-tanakanews' and its
  ;; successor `shimbun-tanakanews-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-tanakanews-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun
					shimbun-tanakanews-expiration-days)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-tanakanews))
  (mapcar 'car shimbun-tanakanews-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-tanakanews))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-tanakanews-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-tanakanews))
;;  (concat shimbun-tanakanews-url "index2007.html"))
  (concat shimbun-tanakanews-url ""))

(defmacro shimbun-tanakanews-japanese-string-to-number (string)
  "Convert a Japanese zenkaku number to just a number."
  (let ((alist ''((?０ . 0) (?１ . 1) (?２ . 2) (?３ . 3) (?４ . 4)
		  (?５ . 5) (?６ . 6) (?７ . 7) (?８ . 8) (?９ . 9))))
    (if (= (length "０") 1)
	`(let* ((str ,string)
		(alist ,alist)
		(len (length str))
		(idx 0)
		(num 0))
	   (while (< idx len)
	     (setq num (+ (cdr (assq (aref str idx) alist)) (* num 10))
		   idx (1+ idx)))
	   num)
      `(let* ((str ,string)
	      (alist ,alist)
	      (len (length str))
	      (idx 0)
	      (num 0)
	      char)
	 (while (< idx len)
	   (setq char (sref str idx)
		 num (+ (cdr (assq char alist)) (* num 10))
		 idx (+ idx (char-bytes char))))
	 num))))

(defun shimbun-tanakanews-shorten-brackets-in-string (string)
  "Replace Japanes zenkaku brackets with ascii characters in STRING.
It does also shorten too much spaces."
  (with-temp-buffer
    (insert string)
    (let ((alist '(("（" . " (") ("）" . ") ") ("［" . " [")
		   ("］" . "] ") ("｛" . " {") ("｝" . "} ")))
	  elem)
      (while alist
	(setq elem (pop alist))
	(goto-char (point-min))
	(while (search-forward (car elem) nil t)
	  (replace-match (cdr elem))))
      (goto-char (point-min))
      (while (re-search-forward "[\t 　]+" nil t)
	(replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "\\([])}]\\) \\([])}]\\)" nil t)
	(replace-match "\\1\\2")
	(forward-char -1))
      (goto-char (point-min))
      (while (re-search-forward "\\([[({]\\) \\([[({]\\)" nil t)
	(replace-match "\\1\\2")
	(forward-char -1))
      (goto-char (point-min))
      (while (re-search-forward " ?\\([「」]\\) ?" nil t)
	(replace-match "\\1"))
      (goto-char (point-min))
      (while (re-search-forward "^ \\| $" nil t)
	(replace-match "")))
    (buffer-string)))

(defun shimbun-tanakanews-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-from-address shimbun))
	(case-fold-search t)
	regexp numbers cyear cmonth cday month year day headers)
    (setq regexp (assoc group shimbun-tanakanews-group-table)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp) (regexp-quote group))
	  cyear (decode-time)
	  cday (nth 3 cyear)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear)
;; 	  cyear 2008)
    )
    (while (re-search-forward regexp nil t)
      (setq day (cond ((nth 4 numbers)
		       (string-to-number (match-string (nth 4 numbers))))
		      (t
		       1))
	    month (cond ((nth 3 numbers)
			 (string-to-number (match-string (nth 3 numbers))))
			((nth 6 numbers)
			 (shimbun-tanakanews-japanese-string-to-number
			  (match-string (nth 6 numbers))))
			(t
			 1))
	    year (cond ((and (= 12 month) (= 1 cmonth))
			(1- cyear))
		       ((and (= 1 month) (= 12 cmonth))
			(1+ cyear))
		       (t
 			cyear)))
;;	    year cyear)
      (push (shimbun-make-header
	     ;; number
	     0
	     ;; subject
	     (if (and (nth 6 numbers) (nth 7 numbers))
		 (shimbun-mime-encode-string
		  (format "%02d/%02d %s"
			  month day
			  (shimbun-tanakanews-shorten-brackets-in-string
			   (match-string (nth 2 numbers)))))
	       (shimbun-mime-encode-string (match-string (nth 2 numbers))))
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string year month day
				       (when (nth 5 numbers)
					 (match-string (nth 5 numbers))))
	     ;; id
	     (format "<%d%02d%02d.%s%%%s.%s>"
		     year month day (when (nth 1 numbers)
				      (match-string (nth 1 numbers))) group
		     shimbun-tanakanews-top-level-domain)
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (concat shimbun-tanakanews-url
		     (match-string (nth 0 numbers))))
	    headers))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-tanakanews)
					 &optional range)
  (shimbun-tanakanews-get-headers shimbun))

(defun shimbun-tanakanews-adjust-date-header (shimbun header)
  "Adjust a date header if there is a correct information available."
  (let ((case-fold-search t)
	(group (shimbun-current-group-internal shimbun))
	end)
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t)
	       (progn
		 (setq end (match-beginning 0))
		 (beginning-of-buffer)
		 (re-search-forward "(\\(20[0-9][0-9]\\))年\\(1?[0-9]\\)月\
\\([123]?[0-9]\\)日" 
				    end t)))
      (shimbun-header-set-date
       header
       (shimbun-make-date-string
	(string-to-number (match-string 1))
	(string-to-number (match-string 2))
	(string-to-number (match-string 3))))))
  (goto-char (point-min)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-tanakanews)
						   header)
  (shimbun-tanakanews-adjust-date-header shimbun header))

(provide 'sb-tanakanews)

;;; sb-tanakanews.el ends here