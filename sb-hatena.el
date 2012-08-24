;;; sb-hatena.el --- shimbun backend for d.hatena.co.jp -*- coding: iso-2022-7bit; -*-

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
;; (require 'sb-text)

(luna-define-class shimbun-hatena
		   (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-hatena-top-level-domain "d.hatena.ne.jp"
  "Name of the top level domain for the hatena Web.")

(defvar shimbun-hatena-url
  (concat "http://" shimbun-hatena-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-hatena-your-id
  "lightcyan")

(defvar shimbun-hatena-group-table
  (let ((default
	  (list
	   (concat
	    "<[\t\n ]*a[\t\n ]+href[\t\n ]*=[\t\n ]*\""
	    ;; 
 	    shimbun-hatena-url shimbun-hatena-your-id "/"
	    ;; 1. url
	    "\\("
	    ;; 2. year
	    "\\(20[0-9][0-9]\\)"
	    ;; 3. month
	    "\\([01][0-9]\\)"
	    ;; 4. day
	    "\\([0-3][0-9]\\)"
	    ;; 5. serial number
	    "/\\([p0-9]+\\)\\)" ""
	    ;; 6. subject
 	    "\">\\(.+\\)" "[\t\n ]*<[\t\n ]*/a[\t\n ]*>[\t\n ]*"
	    )
	   1 2 3 4 5 6)))
    `(("blog" "はてな" "archive/" ,@default)
;;       ("old1" "はてな" "archive?word=&of=50" ,@default)
;;       ("old2" "はてな" "archive?word=&of=100" ,@default)
;;       ("old3" "はてな" "archive?word=&of=150" ,@default)
      ))
  "test")

(defvar shimbun-hatena-content-start "<div class=\"section\">[\t\n ]")

(defvar shimbun-hatena-content-end  "</div>")

(defvar shimbun-hatena-x-face-alist nil)

(defvar shimbun-hatena-expiration-days 7)

(luna-define-method initialize-instance :after ((shimbun shimbun-hatena)
						 &rest init-args)
  (shimbun-set-server-name-internal shimbun shimbun-hatena-your-id)
  (shimbun-set-from-address-internal shimbun
				     (concat "webmaster@"
					     shimbun-hatena-top-level-domain))
  ;; To share class variables between `shimbun-hatena' and its
  ;; successor `shimbun-hatena-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-hatena-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun
					shimbun-hatena-expiration-days)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-hatena))
  (mapcar 'car shimbun-hatena-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-hatena))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-hatena-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-hatena))
  (concat shimbun-hatena-url shimbun-hatena-your-id  "/"
	    (nth 2 (assoc group shimbun-hatena-group-table))))

(defun shimbun-hatena-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-from-address shimbun))
	(case-fold-search t)
	regexp numbers cyear cmonth cday month year day headers)
    (setq regexp (assoc group shimbun-hatena-group-table)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp) (regexp-quote group))
	  cyear (decode-time)
	  cday (nth 3 cyear)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    (while (re-search-forward regexp nil t)
      (setq day (cond ((nth 3 numbers)
		       (string-to-number (match-string (nth 3 numbers))))
		      (t
		       1))
	    month (cond ((nth 2 numbers)
			 (string-to-number (match-string (nth 2 numbers))))
			(t
			 1))
	    year  (cond ((nth 1 numbers)
			 (string-to-number (match-string (nth 1 numbers))))
			(t
			 1)))
      (push (shimbun-create-header
 	     ;; number
 	     0
	     ;; subject
	     (match-string (nth 5 numbers))
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string year month day)
	     ;; id
	     (format "<%d%02d%02d.%s%%%s.%s>"
		     year month day (when (nth 4 numbers)
				      (match-string (nth 4 numbers))) "blog"
		     shimbun-hatena-top-level-domain)
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (concat shimbun-hatena-url shimbun-hatena-your-id "/"
		     (match-string (nth 1 numbers))
		     (match-string (nth 2 numbers))
		     (match-string (nth 3 numbers)) "/"
		     (match-string (nth 4 numbers))
		     ))
	    headers))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-hatena)
					 &optional range)
  (shimbun-hatena-get-headers shimbun))

(provide 'sb-hatena)

;;; sb-hatena.el ends here