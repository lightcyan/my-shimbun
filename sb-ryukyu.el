;;; sb-ryukyu.el --- shimbun backend for ryukyushimpo.jp -*- coding: iso-2022-7bit; -*-

;; Modified from sb-yomiuri.el by lightcyan <lightcyan.area51@gmail.com>
;; URL: http://d.hatena.ne.jp/lightcyan/
;; Below is the original copyright notice of sb-reuters-rss.el.

;; Copyright (C) 2001, 2002, 2003, 2004, 2005 Authors

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>

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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-ryukyu
		   (shimbun-japanese-newspaper shimbun-text) ())

(defvar shimbun-ryukyu-top-level-domain "ryukyushimpo.jp"
  "Name of the top level domain for the 琉球新報.")

(defvar shimbun-ryukyu-url
  (concat "http://" shimbun-ryukyu-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-ryukyu-group-table
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (default
	   (list
	    (concat
	     "<a" s1 "href=\""
	     ;; 1. url
	     "\\(http://[^/]+/news/"
	     ;; 2. serial
	     "storyid-\\([0-9a-z]+-storytopic-[0-9a-z]+\\)[^\"]+\\)\">"
	     ;; 3. subject
	     "\\([^<]+\\)</a>"
	     ;; 4. year
	     "(\\(20[0-9][0-9]\\)\\."
	     ;; 5. month
	     "\\([01]?[0-9]\\)\\."
	     ;; 6. date
	     "\\([0-3]?[0-9]\\))")
	    1 2 2 4 3 5 6)))
    `(("society" "社会" "storytopic-1.html" ,@default)
      ("politics" "政治" "storytopic-3.html" ,@default)
      ("economy" "経済" "storytopic-4.html" ,@default)
      ("region" "地域" "storytopic-5.html" ,@default)
      ("education" "教育" "storytopic-7.html" ,@default)
      ("ent" "エンタメ" "storytopic-137.html" ,@default)
      ("editorial" "社説" "storytopic-11.html" ,@default)
      ("column" "コラム" "storytopic-12.html" ,@default)
      ))
"Alist of group names, their Japanese translations, index pages,
regexps and numbers.
Regexp may contain the \"%s\" token which is replaced with a
regexp-quoted group name.  Numbers point to the search result in order
of [0]url, [1,2]serial numbers, [3]year, [4]subject, [5]month, [6]day,
\[7]hour:minute, [8]ja month, [9]ja day and [10]subgroup.")

(defvar shimbun-ryukyu-content-start
  "\n<!--// article_start //-->\n\\|\n<!--  honbun start  -->\n")

(defvar shimbun-ryukyu-content-end
  "\n<!--// article_end //-->\n\\|\n<!--  honbun end  -->\n")

(defvar shimbun-ryukyu-x-face-alist
  '(("default" . "X-Face: #sUhc'&(fVr$~<rt#?PkH,u-.fV(>y)\
i\"#,TNF|j.dEh2dAzfa4=IH&llI]S<-\"dznMW2_j\n [N1a%n{SU&E&\
Ex;xlc)9`]D07rPEsbgyjP@\"_@g-kw!~TJNilrSC!<D|<m=%Uf2:eebg")))

(defvar shimbun-ryukyu-expiration-days 7)

(luna-define-method initialize-instance :after ((shimbun shimbun-ryukyu)
						 &rest init-args)
  (shimbun-set-server-name-internal shimbun "琉球新報")
  (shimbun-set-from-address-internal shimbun "nobody@example.com")
  ;; To share class variables between `shimbun-ryukyu' and its
  ;; successor `shimbun-ryukyu-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-ryukyu-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun
					shimbun-ryukyu-expiration-days)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-ryukyu))
  (mapcar 'car shimbun-ryukyu-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-ryukyu))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-ryukyu-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-ryukyu))
  (let* ((group (shimbun-current-group-internal shimbun))
	 (index (nth 2 (assoc group shimbun-ryukyu-group-table))))
    (if (string-match "\\`http:" index)
	index
      (concat shimbun-ryukyu-url "news/" index))))

;; (defun shimbun-ryukyu-japanese-string-to-number (string)
;;   "Convert a Japanese zenkaku number to just a number."
;;   (let ((alist '((?０ . 0) (?１ . 1) (?２ . 2) (?３ . 3) (?４ . 4)
;; 		 (?５ . 5) (?６ . 6) (?７ . 7) (?８ . 8) (?９ . 9)))
;; 	(len (length string))
;; 	(idx 0)
;; 	(num 0))
;;     (while (< idx len)
;;       (setq num (+ (cdr (assq (aref string idx) alist)) (* num 10))
;; 	    idx (1+ idx)))
;;     num))

(defun shimbun-ryukyu-get-top-header (group from)
  "Return a list of a header for the top news."
  (when (re-search-forward
	 (format
	  (eval-when-compile
	    (let ((s0 "[\t\n ]*")
		  (s1 "[\t\n ]+"))
	      (concat
	       "<a" s1 "href=\"/"
	       ;; 1. url
	       "\\("
	       ;; 2. subgroup
	       "\\(?:\\([^/]+\\)/\\)?"
	       "news/"
	       ;; 3. serial number[1]
	       "\\("
	       ;; 4. year
	       "\\(20[0-9][0-9]\\)"
	       "[01][0-9][0-3][0-9]\\)"
	       ;; 5. serial number[2]
	       "\\([^.]+\\)"
	       "[^\"]+\\)"
	       "\"[^>]*>" s0
	       ;; 6. subject
	       "\\([^<]+\\)"
	       s0)))
	  group)
	 nil t)
    (let* ((url (shimbun-expand-url (match-string 1) shimbun-ryukyu-url))
	   (subgroup (match-string 2))
	   (id (concat "<" (match-string 3) "." (match-string 5)
		       "@" (when subgroup
			     (concat subgroup "."))
		       group "." shimbun-ryukyu-top-level-domain ">"))
	   (year (string-to-number (match-string 4)))
	   (subject (if subgroup
			(concat "[" subgroup "] " (match-string 6))
		      (match-string 6))))
      (prog1
	  (when (re-search-forward
		 (eval-when-compile
		   (let ((s0 "[\t\n ]*"))
		     (concat
		      ">" s0 "（" s0
		      ;; 1. month
		      "\\([01]?[0-9]\\)"
		      s0 "月" s0
		      ;; 2. day
		      "\\([0-3][0-9]\\)"
		      s0 "日" s0
		      ;; 3. hour:minute
		      "\\([012][0-9]:[0-5][0-9]\\)"
		      s0 "）" s0 "<")))
		 nil t)
	    (list (shimbun-create-header
		   0
		   subject
		   from
		   (shimbun-make-date-string
		    year
		    (string-to-number (match-string 1))
		    (string-to-number (match-string 2))
		    (match-string 3))
		   id "" 0 0 url)))
	(goto-char (point-min))))))

(defun shimbun-ryukyu-get-headers (shimbun)
  "Return a list of headers."
  (shimbun-strip-cr)
  (goto-char (point-min))
  (let ((group (shimbun-current-group-internal shimbun))
	(from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	(case-fold-search t)
	headers regexp numbers subject month day subgroup)
    ;; Extract top news.
    (when (member group '("atmoney" "entertainment" "national" "politics"
			  "science" "sports" "world"))
      (setq headers (shimbun-ryukyu-get-top-header group from)))
    (setq regexp (assoc group shimbun-ryukyu-group-table)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp) (regexp-quote group)))
    (while (re-search-forward regexp nil t)
      (setq subject (match-string (nth 4 numbers))
	    month (string-to-number (match-string (nth 5 numbers)))
	    day (string-to-number (match-string (nth 6 numbers)))
	    subgroup (when (nth 10 numbers)
		       (match-string (nth 10 numbers))))
      (cond ((string-equal group "editorial")
	     (setq subject
		   (format
		    "%02d/%02d %s"
		    month day
		    (save-match-data
		      (if (string-match "\\`［\\(.+\\)］「\\(.+\\)」\\'"
					subject)
			  (replace-match "\\1: \\2" nil nil subject)
			subject)))))
	    (subgroup
	     (setq subject (concat "[" subgroup "] " subject))))
      (push (shimbun-create-header
	     0 subject from
	     (shimbun-make-date-string
	      (string-to-number (match-string (nth 3 numbers)))
	      month day
	      (when (nth 7 numbers)
		(match-string (nth 7 numbers))))
	     (concat "<" (match-string (nth 1 numbers))
		     "." (match-string (nth 2 numbers))
		     "%" (when subgroup
			   (concat subgroup "."))
		     group "." shimbun-ryukyu-top-level-domain ">")
	     "" 0 0
	     (shimbun-expand-url (match-string (nth 0 numbers))
				 shimbun-ryukyu-url))
	    headers))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-ryukyu)
					 &optional range)
  (shimbun-ryukyu-get-headers shimbun))

(defun shimbun-ryukyu-prepare-article (shimbun header)
  (shimbun-strip-cr)
  (goto-char (point-min))
  (let ((case-fold-search t)
	start)
    (when (and (re-search-forward shimbun-ryukyu-content-start nil t)
	       (progn
		 (setq start (match-end 0))
		 (re-search-forward shimbun-ryukyu-content-end nil t)))
      (narrow-to-region start (match-beginning 0))
      (goto-char (point-min))
      ;; Remove trailing whitespace.
      (while (re-search-forward "[\t ]+$" nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      ;; Correct Date header.
      (when (re-search-forward
	     (eval-when-compile
	       (let ((s0 "[\t\n ]*")
		     (s1 "[\t\n ]+"))
		 (concat
		  "<!--" s0 "//" s1 "date_start" s1 "//" s0 "-->" s0
		  ;; 1. year
		  "\\(20[0-9][0-9]\\)"
		  s0 "年" s0
		  ;; 2. month
		  "\\([01]?[0-9]\\)"
		  s0 "月" s0
		  ;; 3. day
		  "\\([0-3]?[0-9]\\)"
		  s0 "日" s0
		  ;; 4. hour
		  "\\([012]?[0-9]\\)"
		  s0 "時" s0
		  ;; 5. minute
		  "\\([0-5]?[0-9]\\)"
		  s0 "分" s0 "<!--" s0 "//" s1 "date_end" s1 "//" s0 "-->")))
	     nil t)
	(shimbun-header-set-date
	 header
	 (shimbun-make-date-string
	  (string-to-number (match-string 1))
	  (string-to-number (match-string 2))
	  (string-to-number (match-string 3))
	  (format "%02d:%02d"
		  (string-to-number (match-string 4))
		  (string-to-number (match-string 5)))))
	(goto-char (point-min)))
      ;; Remove the フォトニュース, the 写真の拡大 buttons, etc.
      (while (re-search-forward
	      (eval-when-compile
		(let ((s0 "[\t\n ]*")
		      (s1 "[\t\n ]+")
		      (n1 "[^\t\n >]+"))
		  (concat
		   s0
		   "\\(?:"
		   "<a\\(?:" s1 n1 "\\)*" s1
		   "\\(?:class=\"photo-pn\"\\|target=\"photoWin\"\\)"
		   "\\(?:" s1 n1 "\\)*" s0 ">"
		   "\\|"
		   "<img\\(?:" s1 n1 "\\)*" s1
		   "\\(?:alt=\"フォトニュース\"\\|class=\"photo-el\"\\)"
		   "\\(?:" s1 n1 "\\)*" s0 ">"
		   "\\|"
		   "<div" s1
		   "class=\"enlargedphoto\">\\(?:[^<>]+\\)?"
		   "<img" s1 "[^>]+>" s0 "</div>"
		   "\\|"
		   "<div" s1 "class=\"[^\"]+\">" s0
		   "<img\\(?:" s1 n1 "\\)*" s1 "src=\"/g/d\\.gif\""
		   "\\(?:" s1 n1 "\\)*" s0 ">" s0 "</div>"
		   "\\|"
		   s0 "rectangle(\"[^\"]+\");" s0
		   "\\)" s0)))
	      nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      ;; Replace 写真の拡大 with 写真.
      (while (re-search-forward
	      (eval-when-compile
		(let ((s1 "[\t\n ]+")
		      (n1 "[^\t\n >]+"))
		  (concat "<img\\(?:" s1 n1 "\\)*" s1
			  "alt=\"写真\\(の拡大\\)\"")))
	      nil t)
	(delete-region (match-beginning 1) (match-end 1)))
      (goto-char (point-min))
      ;; Break continuous lines in editorial articles.
      (when (and (string-equal "editorial"
			       (shimbun-current-group-internal shimbun))
		 (string-match " \\(?:よみうり寸評\\|編集手帳\\)\\'"
			       (shimbun-header-subject header 'no-encode)))
	(goto-char (point-min))
	(while (search-forward "◆" nil t)
	  (replace-match "。<br><br>　")))
      (widen)))
  (goto-char (point-min)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-ryukyu)
						   header)
  (shimbun-ryukyu-prepare-article shimbun header))

(provide 'sb-ryukyu)

;;; sb-ryukyu.el ends here
