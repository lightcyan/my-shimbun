;;; sb-chosun.el --- shimbun backend for japanese.chosun.com -*- coding: iso-2022-7bit; -*-

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

(luna-define-class shimbun-chosun
		   (shimbun-japanese-newspaper shimbun-text) ())

(defvar shimbun-chosun-top-level-domain "www.chosunonline.com"
  "Name of the top level domain for the Digital Chosunilbo.")

(defvar shimbun-chosun-url
  (concat "http://" shimbun-chosun-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-chosun-group-table
  (let ((default
	  (list
	   (concat
	    "<[\t\n ]*a[\t\n ]+href[\t\n ]*=[\t\n ]*\""
	    ;; 1. url
	    "\\("
	    "http://www.chosunonline.com/site/data/html_dir/20[0-9][0-9]/"
	    ;; 2. month
	    "\\([01][0-9]\\)/"
	    ;; 3. day
	    "\\([0-3][0-9]\\)/"
	    ;; 4. serial number
	    "\\([0-9]+\\)\\.html\\)\"[\t\n ]*>"
	    ;; 5. subject
	    "\\(.*\\cj.*\\)" "[\t\n ]*<[\t\n ]*/a[\t\n ]*>"
	    )
	    1 4 5 2 3 nil)))
    `(("international" "国際面" "" ,@default)
      ("northkorea" "北朝鮮" "" ,@default)
      ("economy" "経済面" "" ,@default)
      ("society" "社会面" "" ,@default)
      ("politics" "政治面" "" ,@default)
      ("editorial" "社説" "" ,@default)
;;       ("culture" "文化芸能" "index.html" ,@default)
      ))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.
Regexp may have the \"%s\" token which is replaced with a
regexp-quoted group name.  Numbers point to the search result in order
of a url, a serial number, a subject, a month, a day, an hour:minute
and an extra keyword.")

;; (defvar shimbun-chosun-content-start "\n[\t\n ]*<!!--bodystart-->\n")
(defvar shimbun-chosun-content-start "<div[\t\n ]class=\"article\"[\t\n ]id=\"news_content\"><p>")
;; (defvar shimbun-chosun-content-end  "\n[\t\n ]*<!!--bodyend-->\n")
(defvar shimbun-chosun-content-end  "</div><!--#credit-->")
(defvar shimbun-chosun-x-face-alist nil)

(defvar shimbun-chosun-expiration-days 7)

(luna-define-method initialize-instance :after ((shimbun shimbun-chosun)
						 &rest init-args)
  (shimbun-set-server-name-internal shimbun "朝鮮日報")
  (shimbun-set-from-address-internal shimbun
				     (concat "webmaster@www."
					     shimbun-chosun-top-level-domain))
  ;; To share class variables between `shimbun-chosun' and its
  ;; successor `shimbun-chosun-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-chosun-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun
					shimbun-chosun-expiration-days)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-chosun))
  (mapcar 'car shimbun-chosun-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-chosun))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-chosun-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-chosun))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat shimbun-chosun-url group "/"
	    (nth 2 (assoc group shimbun-chosun-group-table)))))

(defmacro shimbun-chosun-japanese-string-to-number (string)
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

(defun shimbun-chosun-shorten-brackets-in-string (string)
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

(defun shimbun-chosun-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-from-address shimbun))
	(case-fold-search t)
	regexp numbers cyear cmonth month year day headers)
    (setq regexp (assoc group shimbun-chosun-group-table)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp) (regexp-quote group))
	  cyear (decode-time)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    (while (re-search-forward regexp nil t)
      (setq month (cond ((nth 3 numbers)
			 (string-to-number (match-string (nth 3 numbers))))
			((nth 6 numbers)
			 (shimbun-chosun-japanese-string-to-number
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
		       (shimbun-chosun-japanese-string-to-number
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
			  (shimbun-chosun-shorten-brackets-in-string
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
		     shimbun-chosun-top-level-domain)
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (concat (match-string (nth 0 numbers))))
	    headers))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-chosun)
					 &optional range)
  (shimbun-chosun-get-headers shimbun))

(defun shimbun-chosun-adjust-date-header (shimbun header)
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

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-chosun)
						   header)
  (shimbun-chosun-adjust-date-header shimbun header))

(provide 'sb-chosun)

;;; sb-chosun.el ends here
