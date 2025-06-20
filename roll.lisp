;;;; Blogroll Generator
;;;; ==================

;;;; Copyright (c) 2025 Susam Pal
;;;;
;;;; You can use, copy, modify, merge, publish, distribute,
;;;; sublicense, and/or sell copies of it, under the terms of the MIT
;;;; License.  See COPYRIGHT.md for complete details.
;;;;
;;;; This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; express or implied.  See COPYRIGHT.md for complete details.

;;;; Caution: The content parsing here is a collection of hacks.  It
;;;; isn't standards conformant but it works for me!

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
      (setf angle-token-index (search angle-token text :start2 start-token-index))
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
  ;; Example: 2025-06-16T00:00:00Z
  (let* ((good (= (length date-string) 20))
         (year (when good (safe-parse-integer (subseq date-string 0 4))))
         (month (when year (safe-parse-integer (subseq date-string 5 7))))
         (date (when month (safe-parse-integer (subseq date-string 8 10))))
         (hour (when date (safe-parse-integer (subseq date-string 11 13))))
         (minute (when hour (safe-parse-integer (subseq date-string 14 16))))
         (second (when hour (safe-parse-integer (subseq date-string 17 19)))))
    (when (and year month date hour minute second)
      (encode-universal-time second minute hour date month year 0))))


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
        (cons "title" title)
        (cons "link" link)))

(defun read-rss-feed (text)
  (let ((posts))
    (dolist (item (strings-within-tag text "item"))
      (let* ((title (string-within-tag item "title"))
             (link (string-within-tag item "link"))
             (pub-date (string-within-tag item "pubDate"))
             (pub-time (and (stringp pub-date) (safe-parse-rss-date pub-date))))
        (when (and (stringp title) (string/= title "")
                   (stringp link) (string/= link "")
                   pub-time)
          (setf title (string-within-or-original title "<!CDATA[" "]]"))
          (push (make-feed-item pub-time title link) posts))))
    posts))

(defun read-atom-feed (text)
  (let ((posts))
    (dolist (item (strings-within-tag text "entry"))
      (let* ((title (string-within-tag item "title"))
             (link (extract-atom-link item))
             (pub-date (string-within-tag item "published"))
             (pub-time (and (stringp pub-date) (safe-parse-atom-date pub-date))))
        (when (and (stringp title) (string/= title "")
                   (stringp link) (string/= link "")
                   pub-time)
          (setf title (string-within-or-original title "<![CDATA[" "]]"))
          (push (make-feed-item pub-time title link) posts))))
    posts))

(defun read-feed (src-path)
  "Read feed items from the given feed file path."
  (let ((text (read-file src-path)))
    (if (or (search "<rss>" text)
            (search "<rss " text))
        (read-rss-feed text)
        (read-atom-feed text))))

(defun read-feeds (src)
  (loop for path in (directory src)
        append (last-n 5 (sort-by-date (read-feed path)))))

(defun make-roll (params)
  (let ((page-layout (read-file "layout/page.html"))
        (list-layout (read-file "layout/blog/list.html"))
        (item-layout (read-file "layout/blog/item.html"))
        (items (sort-by-date (read-feeds "/tmp/feed/*.xml")))
        (rendered-items))
    (set-nested-template list-layout page-layout)
    (dolist (item items)
      (push (render item-layout item) rendered-items))
    (aput "body" (join-strings rendered-items) params)
    (write-page "_site/roll.html" list-layout params)))

(defun main ()
  (make-roll nil))

(when *roll-mode*
  (main))
