;;; sb-taiwantoday.el --- shimbun backend for japanese.taiwantoday.com -*- coding: iso-2022-7bit; -*-

;; Modified from sb-yomiuri.el by lightcyan <lightcyan.area51@gmail.com>
;; URL: http://d.hatena.ne.jp/lightcyan/
;; Below is the original copyright notice of sb-reuters-rss.el.

;; Author: http://pc.2ch.net/test/read.cgi/unix/1013710106/405-
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
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;; Yuuichi Teranishi  <teranisi@gohome.org>
;; Katsumi Yamaoka    <yamaoka@jpl.org>

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-taiwantoday
		   (shimbun-japanese-newspaper shimbun-text) ())

(defvar shimbun-taiwantoday-top-level-domain "taiwantoday.tw"
  "Name of the top level domain for the Digital taiwantodayilbo.")

(defvar shimbun-taiwantoday-url
  (concat "http://" shimbun-taiwantoday-top-level-domain)
  "Name of the parent url.")

(defvar shimbun-taiwantoday-group-table
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (default
	   (list
	    (concat
	     "<a" s1 "href=\"" s0
	     ;; 1. url 2. serial number
	     "\\(http://[^\"]+NewsID=\\([0-9#]+\\)\\)\""
	     "[^>]+>"
	     ;; 3. subject
	     "\\([^<]+\\)</a>"
	     ;; 4. trash
	     "\\(<[^>]+>\\)+"
	     ;; 5. month
	     "\\([0-1][0-9]\\)/"
	     ;; 6. date
	     "\\([0-3][0-9]\\)/"
	     ;; 7. year
	     "\\(20[0-9][0-9]\\)"
	     )
	    1 2 3 5 6 nil)))
    `(("politics"     "政治"     "lp\.asp\?CtNode=1770\&CtUnit=146\&BaseDSD=12\&mp=1001" ,@default)
      ("diplomacy"    "外交"     "lp\.asp\?CtNode=1771\&CtUnit=146\&BaseDSD=12\&mp=1001" ,@default)
      ("economics"    "経済"     "lp\.asp\?CtNode=1772\&CtUnit=146\&BaseDSD=12\&mp=1001" ,@default)
      ("culture"      "文化"     "lp\.asp\?CtNode=1773\&CtUnit=146\&BaseDSD=12\&mp=1001" ,@default)
      ("sightseeing"  "観光"     "lp\.asp\?CtNode=1774\&CtUnit=146\&BaseDSD=12\&mp=1001" ,@default)
      ("opinion"      "主張"     "lp\.asp\?CtNode=1776\&CtUnit=146\&BaseDSD=12\&mp=1001" ,@default)
      ("critical"     "評論"     "lp\.asp\?CtNode=1777\&CtUnit=146\&BaseDSD=12\&mp=1001" ,@default)
      ("politics2"    "特報政治" "lp\.asp\?CtNode=1892\&CtUnit=158\&BaseDSD=12\&mp=1001" ,@default)
      ("diplomacy2"   "特報外交" "lp\.asp\?CtNode=1893\&CtUnit=158\&BaseDSD=12\&mp=1001" ,@default)
      ("economics2"   "特報経済" "lp\.asp\?CtNode=1894\&CtUnit=158\&BaseDSD=12\&mp=1001" ,@default)
      ("culture2"     "特報文化" "lp\.asp\?CtNode=1895\&CtUnit=158\&BaseDSD=12\&mp=1001" ,@default)
      ("sightseeing2" "特報観光" "lp\.asp\?CtNode=1896\&CtUnit=158\&BaseDSD=12\&mp=1001" ,@default)
      ))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.
Regexp may have the \"%s\" token which is replaced with a
regexp-quoted group name.  Numbers point to the search result in order
of a url, a serial number, a subject, a month, a day, an hour:minute
and an extra keyword.")

;; (defvar shimbun-taiwantoday-content-start "\n[\t\n ]*<!!--bodystart-->\n")
(defvar shimbun-taiwantoday-content-start "<div[\t\n ]class=\"article\"[\t\n ]id=\"news_content\"><p>")
;; (defvar shimbun-taiwantoday-content-end  "\n[\t\n ]*<!!--bodyend-->\n")
(defvar shimbun-taiwantoday-content-end  "</div><!--#credit-->")
(defvar shimbun-taiwantoday-x-face-alist nil)

(defvar shimbun-taiwantoday-expiration-days 7)

(luna-define-method initialize-instance :after ((shimbun shimbun-taiwantoday)
						 &rest init-args)
  (shimbun-set-server-name-internal shimbun "台湾")
  (shimbun-set-from-address-internal shimbun
				     (concat "webmaster@www."
					     shimbun-taiwantoday-top-level-domain))
  ;; To share class variables between `shimbun-taiwantoday' and its
  ;; successor `shimbun-taiwantoday-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-taiwantoday-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun
					shimbun-taiwantoday-expiration-days)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-taiwantoday))
  (mapcar 'car shimbun-taiwantoday-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-taiwantoday))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-taiwantoday-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-taiwantoday))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat shimbun-taiwantoday-url "/"
	    (nth 2 (assoc group shimbun-taiwantoday-group-table)))))

(defmacro shimbun-taiwantoday-japanese-string-to-number (string)
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

(defun shimbun-taiwantoday-shorten-brackets-in-string (string)
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

(defun shimbun-taiwantoday-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-from-address shimbun))
	(case-fold-search t)
	regexp numbers cyear cmonth month year day headers)
    (setq regexp (assoc group shimbun-taiwantoday-group-table)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp) (regexp-quote group))
	  cyear (decode-time)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    (while (re-search-forward regexp nil t)
      (setq month (cond ((nth 3 numbers)
			 (string-to-number (match-string (nth 3 numbers))))
			((nth 6 numbers)
			 (shimbun-taiwantoday-japanese-string-to-number
			  (match-string (nth 6 numbers))))
			(t
			 1))
	    year (cond ((and (= 12 month) (= 1 cmonth))
			(1- cyear))
		       ((and (= 1 month) (= 12 cmonth))
			(1+ cyear))
		       (t
			cyear))
	    day (cond ((nth 4 numbers)
		       (string-to-number (match-string (nth 4 numbers))))
		      ((nth 7 numbers)
		       (shimbun-taiwantoday-japanese-string-to-number
			(match-string (nth 7 numbers))))
		      (t
		       1)))
      (push (shimbun-make-header
	     ;; number
	     0
	     ;; subject
	     (if (and (nth 6 numbers) (nth 7 numbers))
		 (shimbun-mime-encode-string
		  (format "%02d/%02d %s"
			  month day
			  (shimbun-taiwantoday-shorten-brackets-in-string
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
		     year month day (match-string (nth 1 numbers)) group
		     shimbun-taiwantoday-top-level-domain)
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (concat (match-string (nth 0 numbers))))
	    headers))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-taiwantoday)
					 &optional range)
  (shimbun-taiwantoday-get-headers shimbun))

(defun shimbun-taiwantoday-adjust-date-header (shimbun header)
  "Adjust a date header if there is a correct information available."
  (let ((case-fold-search t)
	end)
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t)
	       (progn
		 (setq end (match-beginning 0))
		 (beginning-of-buffer)
		 (re-search-forward "\\(20[0-9][0-9]\\)\.\\([01][0-9]\\)\.\
\\([0123][0-9]\\).+\\([012][0-9]:[0-5][0-9]\\)"
				    end t)))
      (shimbun-header-set-date
       header
       (shimbun-make-date-string
	(string-to-number (match-string 1))
	(string-to-number (match-string 2))
	(string-to-number (match-string 3))
	(match-string 4)))))
  (goto-char (point-min)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-taiwantoday)
						   header)
  (shimbun-taiwantoday-adjust-date-header shimbun header))

(provide 'sb-taiwantoday)

;;; sb-taiwantoday.el ends here
