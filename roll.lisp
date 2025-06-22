;;;; A Rudimentary Imperfect Blogroll Generator
;;;; ==========================================

;;;; Copyright (c) 2025 Susam Pal
;;;;
;;;; You can use, copy, modify, merge, publish, distribute,
;;;; sublicense, and/or sell copies of it, under the terms of the MIT
;;;; License.  See COPYRIGHT.md for complete details.
;;;;
;;;; This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; express or implied.  See COPYRIGHT.md for complete details.


;;;; CAUTION
;;;; -------
;;;;
;;;; This program parses feed content using simple, non-standard
;;;; techniques such as string matching, which do not fully account
;;;; for the grammar of XML.  It performs basic validation and
;;;; sanitisation, and skips clearly invalid content as well, but it
;;;; does not handle all edge cases or adversarial input well.  It
;;;; assumes that feeds come from known, benign sources, an assumption
;;;; I can afford.  If used with a feed that is crafted in a contrived
;;;; or malicious way, this program may produce a broken blogroll.
;;;; This, of course, will be fixed soon by using a proper XML parser.


(defvar *site-mode* nil)

(load "site.lisp")


;;; Special Modes
;;; -------------

(defvar *roll-mode* t
  "Run main function iff true.")


;;; General Definitions
;;; -------------------

(defun string-within (text start-token end-token &optional (next-index 0))
  "Return text between start-token and end-token."
  (let ((start-token-index)
        (start-read-index)
        (end-token-index)
        (result))
    (setf start-token-index (search start-token text :start2 next-index))
    (when start-token-index
      (setf start-read-index (+ start-token-index (length start-token)))
      (setf end-token-index (search end-token text :start2 start-read-index))
      (when end-token-index
        (setf result (subseq text start-read-index end-token-index))
        (setf next-index (+ end-token-index (length end-token)))))
    (values result next-index)))

(defun string-within-or-original (text start-token end-token)
  "Like string-within but returns original string if no tokens found."
  (let ((within (string-within text start-token end-token)))
    (if within within text)))

(defun string-within-tag (text tag &optional (next-index 0))
  "Return text between <tag ...> and </tag>."
  (let* ((start-token1 (fstr "<~a>" tag))
         (start-token2 (fstr "<~a " tag))
         (angle-token ">")
         (end-token (fstr "</~a>" tag))
         (start-token-index)
         (start-read-index)
         (angle-token-index)
         (end-token-index)
         (result))
    (setf start-token-index (or (search start-token1 text :start2 next-index)
                                (search start-token2 text :start2 next-index)))
    (when start-token-index
      (setf angle-token-index (search angle-token text :start2 (1+ start-token-index)))
      (when angle-token-index
        (setf start-read-index (+ angle-token-index (length angle-token)))
        (setf end-token-index (search end-token text :start2 start-read-index))
        (when end-token-index
          (setf result (subseq text start-read-index end-token-index))
          (setf next-index (+ end-token-index (length end-token))))))
    (values result next-index)))

(defun strings-within-tag (text tag)
  "Return all text between start-token and end-token."
  (let ((next-index 0)
        (result)
        (results))
    (loop
      (setf (values result next-index)
            (string-within-tag text tag next-index))
      (unless result
        (return))
      (push result results))
    (reverse results)))

(defun string-truncate-words (text max-chars min-words)
  "Return a string truncated to a maximum length at word boundary."
  (when (> (length text) max-chars)
    (let ((word-count 0)
          (good-index 0)
          (next-index))
      (loop
        (setf next-index (position #\Space text :start (1+ good-index)))
        (unless next-index
          (return))
        (unless (<= next-index max-chars)
          (return))
        (incf word-count)
        (setf good-index next-index))
      (if (or (>= word-count min-words)
              (char= (char text max-chars) #\Space))
          (setf text (fstr "~a ..." (subseq text 0 good-index)))
          (setf text (fstr "~a..." (subseq text 0 max-chars))))))
  text)

(defun month-number (month-name)
  "Given a month name, return the month number."
  (let* ((months '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
         (index (position month-name months :test #'string=)))
    (when index
      (1+ index))))

(defun safe-parse-integer (string)
  (and (every #'digit-char-p string)
       (parse-integer string)))

(defun safe-parse-tz (string)
  (when (and (= (length string) 5)
             (every #'digit-char-p (subseq string 1 4)))
    (parse-tz string)))

(defun safe-parse-rss-date (date-string)
  "Parse RSS date string to universal time (integer)."
  ;; Example: Mon, 16 Jun 2025 00:00:00 +0000
  (let* ((parts (string-split date-string " "))
         (date (when (= (length parts) 6) (safe-parse-integer (nth 1 parts))))
         (month (when date (month-number (nth 2 parts))))
         (year (when month (safe-parse-integer (nth 3 parts))))
         (hms (when year (nth 4 parts)))
         (hour (when (= (length hms) 8) (safe-parse-integer (subseq hms 0 2))))
         (minute (when hour (safe-parse-integer (subseq hms 3 5))))
         (second (when minute (safe-parse-integer (subseq hms 6 8))))
         (tz (when second (safe-parse-tz (nth 5 parts)))))
    (when (and date month year hour minute second tz)
      (encode-universal-time second minute hour date month year tz))))

(defun safe-parse-atom-date (date-string)
  "Parse atom date string to universal time."
  ;; Example: 2025-06-16T00:00:00Z or 2025-06-16T00:00:00+00:00
  ;; -1---5---10---15---20----
  ;; 2025-06-16T00:00:00+00:00
  (setf date-string (string-replace "Z" "+00:00" date-string))
  (let* ((good (= (length date-string) 25))
         (year (when good (safe-parse-integer (subseq date-string 0 4))))
         (month (when year (safe-parse-integer (subseq date-string 5 7))))
         (date (when month (safe-parse-integer (subseq date-string 8 10))))
         (hour (when date (safe-parse-integer (subseq date-string 11 13))))
         (minute (when hour (safe-parse-integer (subseq date-string 14 16))))
         (second (when minute (safe-parse-integer (subseq date-string 17 19))))
         (atom-tz (when second (join-strings (list (subseq date-string 19 22)
                                                   (subseq date-string 23 25)))))
         (tz (when second (safe-parse-tz atom-tz))))
    (when (and year month date hour minute second)
      (encode-universal-time second minute hour date month year tz))))

(defun html-escape (text amp)
  "Escape special characters in HTML."
  (with-output-to-string (out)
    (loop for c across text do
      (case c
        (#\& (write-string (if amp "&amp;" "&") out))
        (#\< (write-string "&lt;" out))
        (#\> (write-string "&gt;" out))
        (#\" (write-string "&quot;" out))
        (#\' (write-string "&apos;" out))
        (t (write-char c out))))))

(defun good-link (link)
  "Check if we can accept the given URL to be included in href attribute."
  (setf link (string-downcase link))
  (and
   (or (string-starts-with "http://" link)
       (string-starts-with "https://" link))
   (every (lambda (c) (>= (char-code c) 32)) link)
   (not (search "javascript:" link))
   (not (search "data:" link))))


;;; Tool Definitions
;;; ----------------

(defun format-content-date (universal-time)
  "Convert universal-time (integer) to a simple human-readable date."
  (multiple-value-bind (second minute hour date month year day dst tz)
      (decode-universal-time universal-time 0)
    (declare (ignore day dst tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d +0000"
            year month date hour minute second)))

(defun extract-atom-link (item)
  "Extract the href attribute from a <link ... /> tag."
  (let* ((start-index (search "<link " item))
         (end-index (when start-index (search "/>" item :start2 start-index)))
         (tag (and start-index end-index (subseq item start-index end-index)))
         (link (when tag (string-within tag "href=\"" "\""))))
    link))

(defun make-feed-item (universal-time title link)
  "Create an alist for the given feed item."
  (list (cons "date" (format-content-date universal-time))
        (cons "simple-date" (format-short-date universal-time))
        (cons "title" (html-escape (string-truncate-words title 70 2) nil))
        (cons "link" (html-escape link t))
        (cons "domain" (html-escape (string-within link "://" "/") t))))

(defun read-rss-feed (text)
  (let ((posts))
    (dolist (item (strings-within-tag text "item"))
      (let* ((title (string-within-tag item "title"))
             (link (string-within-tag item "link"))
             (pub-date (string-within-tag item "pubDate"))
             (pub-time (and (stringp pub-date) (safe-parse-rss-date pub-date))))
        (when (and pub-time
                   (stringp title) (string/= title "")
                   (stringp link) (string/= link "")
                   (good-link link))
          (setf title (string-within-or-original title "<!CDATA[" "]]"))
          (write-log "Found RSS item: ~a ~a (~a)"
                     (format-content-date pub-time) link title)
          (push (make-feed-item pub-time title link) posts))))
    posts))

(defun read-atom-feed (text)
  (let ((posts))
    (dolist (item (strings-within-tag text "entry"))
      (let* ((title (string-within-tag item "title"))
             (link (extract-atom-link item))
             (pub-date (string-within-tag item "published"))
             (pub-time (and (stringp pub-date) (safe-parse-atom-date pub-date))))
        (when (and pub-time
                   (stringp title) (string/= title "")
                   (stringp link) (string/= link "")
                   (good-link link))
          (setf title (string-within-or-original title "<![CDATA[" "]]"))
          (write-log "Found Atom entry: ~a ~a (~a)"
                     (format-content-date pub-time) link title)
          (push (make-feed-item pub-time title link) posts))))
    posts))

(defun read-feed (src-path)
  "Read feed items from the given feed file path."
  (let ((text (read-file src-path)))
    (if (or (search "<rss>" text)
            (search "<rss " text))
        (read-rss-feed text)
        (read-atom-feed text))))

(defun read-feeds (src max-per-feed)
  "Read last n posts from each feed in the src directory."
  (loop for path in (directory src)
        do (write-log "~%Reading ~a ..." (file-namestring path))
        append (last-n max-per-feed (sort-by-date (read-feed path)))))

(defun make-roll (dst-path params)
  (let* ((max-per-feed 5)
         (page-layout (read-file "layout/page.html"))
         (list-layout (read-file "layout/roll/list.html"))
         (item-layout (read-file "layout/roll/item.html"))
         (items (sort-by-date (read-feeds "_cache/roll/*.xml" max-per-feed)))
         (count (length items))
         (rendered-items))
    (write-log "~%Rendering blogroll ...")
    (set-nested-template list-layout page-layout)
    (dolist (item items)
      (write-log "Rendering item: ~a ~a (~a)"
                 (aget "date" item) (aget "link" item) (aget "title" item))
      (push (render item-layout item) rendered-items))
    (aput "body" (join-strings rendered-items) params)
    (aput "count" count params)
    (aput "entry-label" (if (= count 1) "entry" "entries") params)
    (aput "gen-date" (format-long-date (get-universal-time) " at ") params)
    (aput "max-per-feed" (format nil "~r" max-per-feed) params)
    (write-page dst-path list-layout params)))

(defun roll ()
  (write-log "Creating blogroll ...")
  (let* ((dst-path "_cache/roll/roll.html")
         (params (list (cons "apex" "_cache/roll/")
                       (cons "current-year" (nth-value 5 (get-decoded-time)))
                       (cons "head" "main.css")
                       (cons "index" ""))))
    (when (probe-file "params.lisp")
      (setf params (append (read-list "params.lisp") params)))
    (when *params*
      (setf params (append *params* params)))
    (aput "title" (render "{{ nick }}'s Blogroll" params) params)
    (when (probe-file dst-path)
      (delete-file dst-path))
    (make-roll dst-path params))
  (write-log "~%Done"))

(when *roll-mode*
  (roll))
