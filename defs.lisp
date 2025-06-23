;;;; Additional Definitions for Auxiliary Tools
;;;; ==========================================

;;;; Copyright (c) 2025 Susam Pal
;;;;
;;;; You can use, copy, modify, merge, publish, distribute,
;;;; sublicense, and/or sell copies of it, under the terms of the MIT
;;;; License.  See COPYRIGHT.md for complete details.
;;;;
;;;; This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; express or implied.  See COPYRIGHT.md for complete details.

(defun string-trim-ws (string)
  "Trim whitespace from both ends of the given string."
  (string-trim (list #\Space #\Tab #\Newline) string))

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

(defun try-parse-integer (string)
  "Parse the given integer; return NIL if parsing fails."
  (setf string (string-trim-ws string))
  (and (every #'digit-char-p string)
       (parse-integer string)))

(defun try-parse-ymd (ymd)
  "Parse yy-mm-dd date; return NILs if parsing fails."
  (let* ((parts (string-split (string-replace " " "" ymd) "-"))
         (year) (month) (date))
    (when (= (length parts) 3)
      (setf year (try-parse-integer (nth 0 parts)))
      (setf month (try-parse-integer (nth 1 parts)))
      (setf date (try-parse-integer (nth 2 parts))))
    (values year month date)))

(defun try-parse-hms (hms)
  "Parse hh:mm:ss time while tolerating errors; return 0s if parsing fails."
  (let* ((parts (string-split (string-replace " " "" hms) ":"))
         (hour (if (>= (length parts) 1) (try-parse-integer (nth 0 parts)) 0))
         (minute (if (>= (length parts) 2) (try-parse-integer (nth 1 parts)) 0))
         (second (if (>= (length parts) 3) (try-parse-integer (nth 2 parts)) 0)))
    (values hour minute second)))

(defun try-parse-tz (tz-string)
  "Parse timezone while tolerating errors; return 0 if parsing fails."
  (setf tz-string (string-replace " " "" tz-string))
  (setf tz-string (string-replace ":" "" tz-string))
  (when (member tz-string (list "Z" "UTC" "GMT") :test #'string=)
    (setf tz-string "+0000"))
  (let ((tz 0))
    (when (and (= (length tz-string) 5)
               (every #'digit-char-p (subseq tz-string 1 4)))
      (setf tz (parse-tz tz-string)))
    tz))

(defun try-parse-rss-date (date-string)
  "Parse RSS date string to universal time (integer) while tolerating errors."
  ;; Example: Mon, 16 Jun 2025 00:00:00 +0000
  (setf date-string (string-replace "GMT" "+0000" date-string))
  (setf date-string (string-replace "UTC" "+0000" date-string))
  (let (parts date month year (hour 0) (minute 0) (second 0) (tz 0))
    (setf parts (string-split date-string " " :ignore-empty t))
    (when (>= (length parts) 4)
      (setf date (try-parse-integer (nth 1 parts)))
      (setf month (month-number (nth 2 parts)))
      (setf year (try-parse-integer (nth 3 parts))))
    (when (>= (length parts) 5)
      (setf (values hour minute second) (try-parse-hms (nth 4 parts))))
    (when (>= (length parts) 6)
      (setf tz (try-parse-tz (nth 5 parts))))
    (when (and date month year hour minute second tz)
      (encode-universal-time second minute hour date month year tz))))

(defun try-parse-iso-date (date-string)
  "Parse atom date string to universal time."
  ;; Example: 2025-06-16T00:00:00Z or 2025-06-16T00:00:00+00:00 or
  ;; 2025-06-16 00:00:00 00:00
  (setf date-string (string-trim-ws date-string))
  (setf date-string (string-replace "Z" "+00:00" date-string))
  (let* ((tz-index (or (position #\+ date-string) (position #\- date-string) -1))
         (dt-string date-string)
         (tz-string "+0000")
         (hour 0) (minute 0) (second 0) (tz 0)
         parts year month date)
    (when (>= tz-index 10)
      (setf dt-string (string-trim-ws (subseq date-string 0 tz-index)))
      (setf tz-string (string-trim-ws (subseq date-string tz-index))))
    (cond ((position #\T dt-string) ; Try splitting "yyyy-mm-ddThh:mm:ss".
           (setf parts (string-split dt-string "T")))
          ((position #\Space dt-string) ; Try splitting "yyyy-mm-dd hh:mm:ss".
           (setf parts (string-split dt-string " ")))
          (t                    ; Assume date looks like "yyyy-mm-dd".
           (setf parts (list dt-string))))
    (when (>= (length parts) 1)
      (setf (values year month date) (try-parse-ymd (nth 0 parts))))
    (when (and year month date)
      (when (>= (length parts) 2)
        (setf (values hour minute second) (try-parse-hms (nth 1 parts))))
      (setf tz (try-parse-tz tz-string)))
    (when (and date month year hour minute second tz)
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


(defun format-content-date (universal-time)
  "Convert universal-time (integer) to a simple human-readable date."
  (multiple-value-bind (second minute hour date month year day dst tz)
      (decode-universal-time universal-time 0)
    (declare (ignore day dst tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d +0000"
            year month date hour minute second)))
