;;;; Site Generator
;;;; ==============

;;;; Copyright (c) 2021-2026 Susam Pal
;;;;
;;;; You can use, copy, modify, merge, publish, distribute,
;;;; sublicense, and/or sell copies of it, under the terms of the MIT
;;;; License.  See COPYRIGHT.md for complete details.
;;;;
;;;; This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; express or implied.  See COPYRIGHT.md for complete details.

(require "uiop")


;;; Special Modes
;;; -------------

(defvar *log-mode* t
  "Write logs iff true.")

(defvar *site-mode* t
  "Run main function iff true.")


;;; General Definitions
;;; -------------------

(defun make-directory (path)
  "Create a new directory along with its parents."
  (ensure-directories-exist path))

(defun remove-directory (path)
  "Remove the specified directory tree from the file system."
  (uiop:delete-directory-tree (pathname path) :validate t
                                              :if-does-not-exist :ignore))

(defun directory-basename (path)
  "Return the final directory component of the pathname."
  (let ((name (car (last (pathname-directory path)))))
    (namestring (make-pathname :directory (list :relative name)))))

(defun copy-directory (src-path dst-path)
  "Copy the directory at src-path to dst-path."
  (make-directory dst-path)
  (dolist (pathname (uiop:directory-files src-path))
    (let* ((basename (file-namestring pathname))
           (dst-file-path (merge-pathnames basename dst-path)))
      (uiop:copy-file pathname dst-file-path)))
  (dolist (pathname (uiop:subdirectories src-path))
    (let* ((basename (directory-basename pathname))
           (dst-dir-path (merge-pathnames basename dst-path)))
      (copy-directory pathname dst-dir-path))))

(defun read-file (filename)
  "Read file and close the file."
  (uiop:read-file-string filename))

(defun read-list (filename)
  "Read Lisp file."
  (read-from-string (read-file filename)))

(defun write-file (filename text)
  "Write text to file and close the file."
  (make-directory filename)
  (with-open-file (f filename :direction :output :if-exists :error)
    (write-sequence text f)))

(defun copy-file (src-path dst-path)
  "Copy the file at src-path to dst-path."
  (make-directory dst-path)
  (uiop:copy-file src-path dst-path))

(defun find-files (src-dir)
  "Recursively find all files in the given directory."
  (append (uiop:directory-files src-dir)
          (mapcan #'find-files (uiop:subdirectories src-dir))))

(defun write-log (fmt &rest args)
  "Log message with specified arguments."
  (when *log-mode*
    (apply #'format t fmt args)
    (terpri)))

(defun err (fmt &rest args)
  "Print an error message and exit with status 1."
  (terpri *error-output*)
  (format *error-output* "ERROR: ")
  (apply #'format *error-output* fmt args)
  (terpri *error-output*)
  (uiop:quit 1))

(defun fstr (fmt &rest args)
  "Format string using specified format and arguments."
  (apply #'format nil fmt args))

(defun string-starts-with (prefix string)
  "Test that string starts with the given prefix."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun string-ends-with (suffix string)
  "Test whether the string ends with the given suffix."
  (and (<= (length suffix) (length string))
       (string= suffix string :start2 (- (length string) (length suffix)))))

(defun substring-at (substring string index)
  "Test that substring exists in string at given index."
  (let ((end-index (+ index (length substring))))
    (and (<= end-index (length string))
         (string= substring string :start2 index :end2 end-index))))

(defun string-replace (old new string)
  "Replace old substring in string with new substring."
  (with-output-to-string (s)
    (let* ((next-index 0)
           (match-index))
      (loop
        (setf match-index (search old string :start2 next-index))
        (unless match-index
          (format s "~a" (subseq string next-index))
          (return))
        (format s "~a~a" (subseq string next-index match-index) new)
        (cond ((zerop (length old))
               (when (= next-index (length string))
                 (return))
               (format s "~a" (char string next-index))
               (incf next-index))
              (t
               (setf next-index (+ match-index (length old)))))))))

(defun string-split (string separator)
  "Split a string into a list of strings using the given separator."
  (let ((next-index 0)
        (match-index)
        (result))
    (loop
      (setf match-index (search separator string :start2 next-index))
      (unless match-index
        (return))
      (push (subseq string next-index match-index) result)
      (setf next-index (+ match-index (length separator))))
    (when (< next-index (length string))
      (push (subseq string next-index) result))
    (reverse result)))

(defun join-string-list (strings)
  "Join a list of strings into a single string."
  (apply #'concatenate 'string strings))

(defun join-strings (&rest strings)
  "Join separate string arguments into a single string."
  (apply #'concatenate 'string strings))

(defun repeat-string (count string)
  "Repeat string count number of times."
  (join-string-list (loop repeat count collect string)))

(defun indent-lines (count string)
  "Indent lines by count spaces."
  (let* ((clean-string (string-right-trim '(#\Newline) string))
         (lines (string-split clean-string (fstr "~%")))
         (indent (repeat-string count " ")))
    (join-string-list (loop for line in lines
                            collect (if (zerop (length line))
                                        (fstr "~%")
                                        (fstr "~a~a~%" indent line))))))

(defun last-n (n sequence)
  "Return at most the last n elements of a sequence as a new sequence."
  (subseq sequence (max 0 (- (length sequence) n))))

(defun plural (count word)
  "Pluralise word unless count is one."
  (if (= count 1) word (fstr "~as" word)))

(defun aget (key alist)
  "Given a key, return its value found in alist."
  (cdr (assoc key alist :test #'string=)))

(defun aset (key value alist)
  "Set the value of an existing key in alist."
  (setf (cdr (assoc key alist :test #'string=)) value))

(defmacro aput (key value alist)
  "Put a new key value pair in alist."
  `(push (cons ,key ,value) ,alist))

(defmacro aput-list (key value alist)
  "Add value to a list corresponding to the key in alist."
  `(progn
     (unless (assoc ,key ,alist :test #'string=)
       (push (cons ,key ()) ,alist))
     (push ,value (cdr (assoc ,key ,alist :test #'string=)))))

(defun hget (key table)
  "Given a key, return its value found in hash table."
  (gethash key table))

(defun hmake ()
  "Create a hash table suitable for string keys."
  (make-hash-table :test #'equal))

(defun hset (key value table)
  "Set key-value pair in hash table."
  (setf (gethash key table) value))

(defun hpush (key value table)
  "Push given value to the list value for key in hash table."
  (push value (gethash key table)))

(defun mapp (function items &rest args)
  "Map given function over items and return a new list."
  (mapcar (lambda (item) (apply function item args)) items))

(defun select-if (predicate items &rest args)
  "Select items satisfying predicate; forward arguments to remove-if-not."
  (apply #'remove-if-not predicate items args))

(defun xml-escape (text)
  "Escape special characters for XML."
  (with-output-to-string (out)
    (loop for c across text
          do (write-string (case c
                             (#\& "&amp;")
                             (#\< "&lt;")
                             (#\> "&gt;")
                             (#\" "&quot;")
                             (#\' "&apos;")
                             (t (string c))) out))))


;;; Date Utilities
;;; --------------

(defun weekday-name (weekday-index)
  "Given an index, return the corresponding day of week."
  (nth weekday-index '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

(defun parse-tz (tz-string)
  "Parse time zone string like to CL-friendly rational, e.g., +0530 as -9/2."
  (let* ((sign (if (char= (char tz-string 0) #\-) 1 -1))
         (hours (parse-integer tz-string :start 1 :end 3))
         (minutes (parse-integer tz-string :start 3)))
    (* sign (+ hours (/ minutes 60)))))

(defun parse-content-date (date-string)
  "Parse yyyy-mm-dd[ HH:MM:[:SS[ TZ]]] date to universal time (integer)."
  (let ((len (length date-string))
        (year (parse-integer date-string :start 0 :end 4))
        (month (parse-integer date-string :start 5 :end 7))
        (date (parse-integer date-string :start 8 :end 10))
        (hour 0)
        (minute 0)
        (second 0)
        (tz 0))
    ;; Consider example date: 2020-01-01 10:10:01 +0000
    (when (>= len 16)
      (setf hour (parse-integer date-string :start 11 :end 13))
      (setf minute (parse-integer date-string :start 14 :end 16)))
    (when (>= len 19)
      (setf second (parse-integer date-string :start 17 :end 19)))
    (when (>= len 25)
      (setf tz (parse-tz (subseq date-string 20))))
    (encode-universal-time second minute hour date month year tz)))

(defun month-name (month-number)
  "Given a number, return the corresponding month."
  (nth (1- month-number) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defun format-iso-date (universal-time)
  "Format universal time as an ISO 8601 date string."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month date hour minute second)))

(defun format-short-date (universal-time)
  "Convert universal-time (integer) to a simple human-readable date."
  (multiple-value-bind (second minute hour date month year day dst tz)
      (decode-universal-time universal-time 0)
    (declare (ignore second minute hour day dst tz))
    (format nil "~2,'0d ~a ~4,'0d" date (month-name month) year)))

(defun format-long-date (universal-time &optional (sep " "))
  "Convert universal-time (integer) to a simple human-readable date."
  (multiple-value-bind (second minute hour date month year day dst tz)
      (decode-universal-time universal-time 0)
    (declare (ignore second day dst tz))
    (format nil "~2,'0d ~a ~4,'0d~a~2,'0d:~2,'0d ~a"
            date (month-name month) year sep hour minute "UTC")))

(defun iso-date (date)
  "Parse a content date and format it as an ISO 8601 date."
  (format-iso-date (parse-content-date date)))

(defun short-date (date)
  "Parse a content date and format it as a short date."
  (format-short-date (parse-content-date date)))

(defun long-date (date &optional (sep " "))
  "Parse a content date and format it as a long date."
  (format-long-date (parse-content-date date) sep))


;;; Validators
;;; ----------

(defun validate-required-params (docs)
  "Validate pages to ensure required parameters exist."
  (dolist (doc docs)
    (dolist (key '("date" "title" "tag" "uuid"))
      (unless (aget key doc)
        (err "Missing key ~a for document ~a" key (aget "doc-path" doc))))))

(defun validate-unique-dates (docs)
  "Validate pages to ensure their dates are unique."
  (let ((dates (hmake)))
    (dolist (doc docs)
      (let ((date (aget "date" doc)))
        (when (hget date dates)
          (err "Duplicate date ~a in document ~a" date (aget "doc-path" doc)))
        (hset date t dates)))))

(defun validate-date-order (items)
  "Check that entries are arranged in chronological order."
  (let ((previous-date))
    (dolist (item items)
      (let ((date (if (consp (first item))
                      (aget "date" item)
                      (getf item :date))))
        (when (and previous-date (string< date previous-date))
          (err "Incorrect order for item ~a" date))
        (setf previous-date date)))))

(defun validate-name-order (entries)
  "Check that entries are arranged in the order of names."
  (let ((previous-name))
    (dolist (entry entries)
      (let ((name (getf entry :name)))
        (when (and previous-name (string< name previous-name))
          (err "Incorrect blogroll name order: ~a follows ~a" name previous-name))
        (setf previous-name name)))))

(defun validate-comment-slugs (docs)
  "Validate comment slug parameters."
  (dolist (doc docs)
    (when (and (aget "cslug" doc) (aget "ccslug" doc))
      (err "Document ~a cannot have both cslug and ccslug"
           (aget "doc-path" doc)))))


;;; Content Parsers
;;; ---------------

(defun read-header-line (text next-index)
  "Parse one line of header in text."
  (let* ((start-token "<!-- ")
         (end-token (format nil " -->~%"))
         (sep-token ": ")
         (search-index (+ next-index (length start-token)))
         (end-index)       ; Index of end-token.
         (sep-index)       ; Index of sep-token.
         (key)             ; Text between start-token and end-token.
         (val))            ; Text between sep-token and end-token.
    (when (and (substring-at start-token text next-index)
               (setf end-index (search end-token text :start2 search-index))
               (setf sep-index (search sep-token text :start2 search-index
                                                      :end2 end-index)))
      (setf key (subseq text search-index sep-index))
      (setf val (subseq text (+ sep-index (length sep-token)) end-index))
      (setf next-index (+ end-index (length end-token))))
    (values key val next-index)))

(defun read-headers (text next-index)
  "Parse all headers in text and return (values headers next-index)."
  (let ((key)
        (val)
        (headers))
    (loop
      (setf (values key val next-index)
            (read-header-line text next-index))
      (unless key
        (return))
      (push (cons key val) headers))
    (values headers next-index)))

(defun read-block (text start-index)
  "Read a single block from a multi-block content file."
  (let ((start-token "<!-- ")  ; Header delimiter.
        (blk)                  ; Parsed block headers and body.
        (next-index))          ; Index at which to search next subdoc.
    (setf (values blk start-index) (read-headers text start-index))
    (setf next-index (search start-token text :start2 start-index))
    (aput "body" (subseq text start-index next-index) blk)
    (values blk next-index)))

(defun read-blocks (text)
  "Read all blocks from a multi-block content file."
  (let ((next-index 0)
        (blocks)
        (blk))
    (loop
      (setf (values blk next-index) (read-block text next-index))
      (push blk blocks)
      (unless next-index
        (return)))
    (reverse blocks)))

(defun read-content (text)
  "Read content file."
  (let ((doc))
    (multiple-value-bind (headers next-index) (read-headers text 0)
      (setf doc (append headers doc))
      (aput "body" (subseq text next-index) doc))
    doc))

(defun read-multi-content (text)
  "Read a multi-block content file."
  (let ((blocks (read-blocks text))
        (doc))
    ;; If file header is present, then the header and following body
    ;; becomes the document header and body.
    (when (aget "title" (car blocks))
      (setf doc (car blocks))
      (setf blocks (cdr blocks)))
    (validate-date-order blocks)
    ;; Date of the first block becomes the document date.
    (aput "date" (aget "date" (car blocks)) doc)
    (aput "blocks" blocks doc)
    doc))


;;; Document Finder
;;; ---------------

(defun doc-path (src-path src-dir)
  "Derive document path relative to source directory."
  (let* ((rel-path (subseq src-path (length src-dir)))
         (last-dot (position #\. rel-path :from-end t))
         (penul-dot (position #\. rel-path :end last-dot :from-end t)))
    (join-strings (subseq rel-path 0 penul-dot)
                  (subseq rel-path last-dot))))

(defun find-docs (src-dir)
  "Find all documents recursively in the source directory."
  (let ((docs))
    (dolist (src-path (find-files src-dir))
      (let ((basename (file-namestring src-path))
            (src-path (enough-namestring src-path (truename ".")))
            (doc))
        (aput "src-path" src-path doc)
        ;; Both doc-type and doc-path are set in the following 'cond';
        ;; The doc-path value decides all other paths like root,
        ;; dst-path, neat-path.
        (cond
          ;; Auxilliary files
          ((string-ends-with ".aux.html" basename)
           (aput "doc-type" "aux" doc)
           (aput "doc-path" (doc-path src-path src-dir) doc)
           (setf doc (nconc doc (read-content (read-file src-path)))))
          ;; CSS files.
          ((string-ends-with ".css.css" basename)
           (aput "doc-type" "css" doc)
           (aput "doc-path" (doc-path src-path src-dir) doc)
           (setf doc (nconc doc (read-content (read-file src-path)))))
          ((string-ends-with ".xsl.xsl" basename)
           (aput "doc-type" "xsl" doc)
           (aput "doc-path" (doc-path src-path src-dir) doc)
           (setf doc (nconc doc (read-content (read-file src-path)))))
          ;; Renderable pages.
          ((string-ends-with ".page.html" basename)
           (aput "doc-type" "page" doc)
           (aput "doc-path" (doc-path src-path src-dir) doc)
           (setf doc (nconc doc (read-content (read-file src-path)))))
          ;; Renderable posts.
          ((string-ends-with ".post.html" basename)
           (aput "doc-type" "post" doc)
           (aput "doc-path" (doc-path src-path src-dir) doc)
           (setf doc (nconc doc (read-content (read-file src-path)))))
          ;; Post discussion files.
          ((string-ends-with ".disc.html" basename)
           (aput "doc-type" "disc" doc)
           (aput "doc-path" (doc-path src-path src-dir) doc)
           (setf doc (nconc doc (read-multi-content (read-file src-path)))))
          ;; Talk pages.
          ((string-ends-with ".talk.html" basename)
           (aput "doc-type" "talk" doc)
           (aput "doc-path" (doc-path src-path src-dir) doc)
           (setf doc (nconc doc (read-multi-content (read-file src-path)))))
          ;; Raw files
          (t
           (aput "doc-type" "raw" doc)
           (aput "doc-path" (subseq src-path (length src-dir)) doc)))
        (push doc docs)))
    docs))


;;; Renderer
;;; --------

(defun render (layout params)
  "Render parameter tokens in layout with their values from params."
  (with-output-to-string (s)
    (let* ((ltoken "{{ ")
           (rtoken " }}")
           (next-index 0)     ; Next place to start searching "{{ ".
           (start-index)      ; Starting of "{{ ".
           (end-index))       ; Starting of " }}".
      (loop
        ;; Look for ltoken and extract static text before it.
        (setf start-index (search ltoken layout :start2 next-index))
        (unless start-index
          (return))
        (format s "~a" (subseq layout next-index start-index))
        ;; Extract parameter name between ltoken and rtoken.
        (incf start-index (length ltoken))
        (setf end-index (search rtoken layout :start2 start-index))
        (let* ((key (subseq layout start-index end-index))
               (val (aget key params)))
          ;; If key exists in params, replace key with value.
          ;; Otherwise, leave the key intact in text.
          (if val
              (format s "~a" val)
              (format s "~a~a~a" ltoken key rtoken)))
        (setf next-index (+ end-index (length rtoken))))
      ;; Extract static text after the last parameter token.
      (format s "~a" (subseq layout next-index)))))


;;; Head Filler
;;; -----------

(defun head-html (imports root layouts tex)
  "Given import names and TeX configuration, return head HTML."
  (let ((html)
        (snippets))
    (dolist (name imports)
      (cond ((string-ends-with ".css" name)
             (setf html (fstr "  <link rel=\"stylesheet\" href=\"~acss/~a\">~%" root name)))
            ((string-ends-with ".js" name)
             (setf html (fstr "  <script src=\"~ajs/~a\"></script>~%" root name)))
            (t
             (err "Unknown import type ~a" name)))
      (push html snippets))
    (when (string= tex "yes")
      (push (render (aget "katex" layouts) (list (cons "root" root))) snippets))
    (join-string-list (reverse snippets))))

(defun csv-split (csv)
  "Convert comma-separated values to a list of strings."
  (string-split csv ", "))

(defun fill-head (doc layouts extra-imports params)
  "Insert head parameter into the given document."
  (aput "head" (head-html (append (csv-split (aget "import" params))
                                  (csv-split (aget "import" doc))
                                  (csv-split extra-imports))
                          (aget "root" doc)
                          layouts
                          (aget "tex" doc))
        doc)
  doc)


;;; Path Filler
;;; -----------

(defun root-path (path)
  "Return relative path to web root from the given rendered file path."
  ;; foo/bar/baz.html => 2 => ../../
  (let ((depth (count #\/ path)))
    (if (zerop depth) "./" (repeat-string depth "../"))))

(defun neat-path (path params)
  "Create canonical path component of the URL for the given rendered file path."
  (string-replace "index.html" (aget "index" params) path))

(defun neat-url (path params)
  "Create the canonical URL for the given rendered file path."
  (join-strings (aget "site-url" params)
                (string-replace "index.html" "" path)))

(defun fill-path (doc params)
  "Insert path parameters into the given document."
  (aput "dst-path" (fstr "~a~a" (aget "pub" params) (aget "doc-path" doc)) doc)
  (let* ((doc-path (aget "doc-path" doc)))
    (aput "slug" (pathname-name doc-path) doc)
    (aput "root" (root-path doc-path) doc)
    (aput "neat-path" (neat-path doc-path params) doc)
    (aput "neat-url" (neat-url doc-path params) doc))
  doc)


;;; Document Fillers
;;; ----------------

(defun tag-slug (tag)
  "Convert tag title to tag slug."
  (substitute #\- #\Space (string-downcase tag)))

(defun format-tags (tags indent root)
  "Create HTML to display tags."
  (let ((html "")
        (sep ""))
    (dolist (tag tags)
      (setf tag (tag-slug tag))
      (setf html (fstr "~a~a<a href=\"~atag/~a.html\">#~a</a>" html sep root tag tag))
      (setf sep (fstr " |~%~a" (repeat-string indent " "))))
    html))

(defun zone-link (doc zones params)
  "Create HTML for zone link for the given document."
  (let* ((zone-key (find-if (lambda (key) (find key zones :key #'first :test #'string=))
                            (csv-split (aget "zone" doc))))
         (zone (find zone-key zones :key #'first :test #'string=)))
    ;; If zone-link-text is set, create zone-link.
    (if (fourth zone)
        (fstr "~%    <a href=\"~a~a\">~a</a>"
              (aget "root" doc)
              (neat-path (second zone) params)
              (fourth zone))
        "")))

(defun update-mark (date)
  "Format update date. "
  (join-strings " | Updated on "
                (short-date date)))

(defun toc-indent (level)
  "Create leading indentation items in table of contents."
  (repeat-string (* 2 level) " "))

(defun toc-ol-tag (level)
  "Create an <ol> tag with a marker decided by the list level."
  (if (= level 2)
      "<ol>"                            ; First level is <h2>
      (format nil "<ol type=\"~a\">" (char "012aiai" level))))

(defun toc-html (text ordered)
  "Generate HTML for table of contents."
  (with-output-to-string (tt)
    (let* ((h-begin-index)     ; -> <h2 id="foo">
           (h-close-index)     ; -> </h2>
           (id-end-index)      ; <h2 id="foo" <-
           (title-begin-index) ; <h2 id="foo">F <-
           (next-index 0)      ; <h2 <-
           (indent -1)         ; We want the first indent to be 0.
           (new-level 1)       ; We want the first heading to be <h2>.
           (old-level)
           (open-tag (if ordered "<ol>" "<ul>"))
           (close-tag (if ordered "</ol>" "</ul>"))
           (href)
           (title)
           (init))
      (format tt "<h2 id=\"contents\">Contents</h2>~%")
      (loop
        (setf h-begin-index (search "<h" text :start2 next-index))
        (unless h-begin-index
          (return))
        (cond ((and (digit-char-p (char text (+ h-begin-index 2)))
                    (substring-at "id=\"" text (+ h-begin-index 4)))
               (setf old-level new-level)
               (setf new-level (parse-integer (string (char text (+ h-begin-index 2)))))
               (setf id-end-index (search "\"" text :start2 (+ h-begin-index 8)))
               (setf title-begin-index (1+ (search ">" text :start2 (1+ id-end-index))))
               (setf h-close-index (search "</h" text :start2 (+ id-end-index 2)))
               (setf href (fstr "#~a" (subseq text (+ h-begin-index 8) id-end-index)))
               (setf title (subseq text title-begin-index h-close-index))
               (when ordered
                 (setf open-tag (toc-ol-tag new-level)))
               (cond ((string= href "#contents"))
                     ((> new-level (1+ old-level))
                      (err "Incorrect heading level ~a after ~a: ~a (~a)"
                           new-level old-level href title))
                     ((= new-level (1+ old-level))
                      (format tt (if init "~%" ""))
                      (format tt "~a~a~%" (toc-indent (incf indent)) open-tag)
                      (format tt "~a<li><a href=\"~a\">~a</a>"
                              (toc-indent (incf indent)) href title)
                      (setf init t))
                     ((= new-level old-level)
                      (format tt "</li>~%")
                      (format tt "~a<li><a href=\"~a\">~a</a>"
                              (toc-indent indent) href title))
                     ((< new-level old-level)
                      (format tt "</li>~%")
                      (dotimes (n (- old-level new-level))
                        (format tt "~a~a~%" (toc-indent (decf indent)) close-tag)
                        (format tt "~a</li>~%" (toc-indent (decf indent))))
                      (format tt "~a<li><a href=\"~a\">~a</a>"
                              (toc-indent indent) href title)))
               (setf next-index (+ h-close-index 3)))
              (t
               (setf next-index (+ h-begin-index 2)))))
      (when init
        (format tt "</li>~%")
        (dotimes (n (floor indent 2))
          (format tt "~a~a~%" (toc-indent (decf indent)) close-tag)
          (format tt "~a</li>~%" (toc-indent (decf indent))))
        (format tt "~a" close-tag)))))

(defun fill-fancy-doc (doc params)
  "Insert parameters required for rendering/listing fancy documents."
  (aput "draft-mark" (if (aget "draft" doc) " [draft]" "") doc)
  (aput "short-date" (short-date (aget "date" doc)) doc)
  (aput "iso-date" (iso-date (aget "date" doc)) doc)
  (let ((tags (csv-split (aget "tag" doc))))
    (aput "tags-for-page" (format-tags tags 2 (aget "root" doc)) doc)
    (aput "tags-for-list" (format-tags tags 4 "") doc)
    (aput "tags-for-feed" (xml-escape (format-tags tags 2 (aget "site-url" params))) doc))
  doc)

(defun fill-pp-doc (doc cm-map zones params)
  "Insert additional parameters required by posts and pages."
  (aput "zone-link" (zone-link doc zones params) doc)
  (let* ((cslug (or (aget "ccslug" doc)
                    (aget "cslug" doc)
                    (aget "slug" doc)))
         (cm-doc (hget cslug cm-map))
         (cm-path (if cm-doc (aget "neat-path" cm-doc) "cm/none.html"))
         (toc (aget "toc" doc))
         (update (aget "update" doc)))
    (aput "cm-path" cm-path doc)
    (aput "toc" (toc-html (aget "body" doc) (string= toc "num")) doc)
    (aput "update-mark" (if update (update-mark update) "") doc))
  doc)


;;; Document Renderer
;;; -----------------

(defun refine-headings (text)
  "Add links inside headings that target their own IDs."
  (with-output-to-string (out)
    (let ((h-begin-index)               ; ->  <h1 id="foo">
          (h-close-index)               ; ->  </h1>
          (id-end-index)                ;     <h1 id="foo"   <-
          (next-index 0))               ;     <h1            <-
      (loop
        (setf h-begin-index (search "<h" text :start2 next-index))
        (unless h-begin-index
          (return))
        (cond ((and (digit-char-p (char text (+ h-begin-index 2)))
                    (substring-at "id=\"" text (+ h-begin-index 4)))
               (setf id-end-index (search "\"" text :start2 (+ h-begin-index 8)))
               (setf h-close-index (search "</h" text :start2 (+ id-end-index 2)))
               (format out "~a<a href=\"#~a\"></a></h"
                       (subseq text next-index h-close-index)
                       (subseq text (+ h-begin-index 8) id-end-index))
               (setf next-index (+ h-close-index 3)))
              (t
               (format out "~a" (subseq text next-index (+ h-begin-index 2)))
               (setf next-index (+ h-begin-index 2)))))
      (format out "~a" (subseq text next-index)))))

(defun refine-tex (text)
  "Reformat LaTeX code to make the punctuation stick to formulas."
  (setf text (string-replace " \\)." ".  \\)" text))
  (setf text (string-replace " \\)?" "?  \\)" text))
  (setf text (string-replace " \\)," ", \\)" text))
  (setf text (string-replace " \\);" "; \\)" text)))

(defun render-body (doc params)
  "Render the body in doc and refine it."
  (let* ((doc-type (aget "doc-type" doc))
         (rendered-body (render (aget "body" doc) (append doc params))))
    ;; Add heading anchors to pages and posts.
    (when (member doc-type '("page" "post") :test #'string=)
      (setf rendered-body (refine-headings rendered-body)))
    ;; Slurp punctuation in TeX markups.
    (when (string= (aget "tex" doc) "yes")
      (setf rendered-body (refine-tex rendered-body)))
    rendered-body))

(defun render-doc (doc layouts params)
  "Render the given document within a layout matching its document type."
  (let* ((layout (aget (aget "doc-type" doc) layouts))
         (rendered-body (render-body doc params))
         (rendered-body-param (list (cons "body" rendered-body))))
    (write-log "Writing ~a ~a" (aget "doc-type" doc) (aget "dst-path" doc))
    (write-file (aget "dst-path" doc)
                (render layout (append rendered-body-param doc params)))))

(defun copy-doc (doc)
  "Copy a raw document file to publication directory."
  (write-log "Copying ~a ~a" (aget "doc-type" doc) (aget "dst-path" doc))
  (copy-file (aget "src-path" doc) (aget "dst-path" doc)))


;;; Filters
;;; -------

(defun doc-in-zone-p (doc zone)
  "Check whether a document belongs to the given zone."
  (let ((key (first zone)))
    (or (and (string-ends-with "/" key)
             (string-starts-with key (aget "doc-path" doc)))
        (member key (csv-split (aget "zone" doc)) :test #'string=))))

(defun sort-by-date (items)
  "Sort items in chronological order."
  (sort items (lambda (x y) (string< (aget "date" x) (aget "date" y)))))

(defun yes-p (string)
  "Check if the given string has the value 'yes'."
  (string= string "yes"))

(defun remove-if-keys (items &rest keys)
  "Remove items that have any key from the given keys set to yes."
  (remove-if (lambda (item) (some (lambda (key) (yes-p (aget key item))) keys)) items))

(defun select-if-zone (docs zone)
  "Select documents that belong to the given zone."
  (select-if (lambda (doc) (doc-in-zone-p doc zone)) docs))

(defun select-of-types (docs &rest types)
  "Select documents that match the given types."
  (select-if (lambda (doc)
               (member (aget "doc-type" doc) types :test #'string=)) docs))


;;; Comments
;;; --------

(defun number-blocks (blocks serial-key)
  "Insert one-based serial number to each comment block."
  (loop for blk in blocks
        for serial from 1
        do (aput serial-key serial blk)
        collect blk))

(defun fill-cm-block (blk params tex)
  "Insert comment parameters into a comment block."
  (let* ((author (aget "author" params))
         (date (aget "date" blk))
         (cm-name (aget "name" blk))
         (cm-url (if (string= cm-name author)
                     (aget "site-url" params)
                     (aget "url" blk))))
    (when (string= tex "yes")
      (aput "tex" "yes" blk))
    (aput "cm-author" (if cm-url (fstr "<a href=\"~a\">~a</a>" cm-url cm-name) cm-name) blk)
    (aput "cm-class" (if (string= cm-name author) "author" "visitor") blk)
    (aput "cm-long-date" (long-date date) blk))
  blk)

(defun fill-cm-doc (doc params)
  "Insert parameters into a comment document."
  (let ((blocks (aget "blocks" doc)))
    (setf blocks (number-blocks blocks "cm-fserial"))
    (setf blocks (mapp #'fill-cm-block blocks params (aget "tex" doc)))
    (aset "blocks" blocks doc)
    (aput "cm-count" (length blocks) doc))
  doc)

(defun fill-disc-doc (disc-doc ccslug-map)
  "Insert discussion parameters into a discussion document."
  (let* ((slug (aget "slug" disc-doc))
         (on-doc (hget slug ccslug-map)))
    (unless on-doc
      (err "No subject document found for discussion document ~a"
           (aget "doc-path" disc-doc)))
    (aput "import" (aget "import" on-doc) disc-doc)
    (aput "tex" (aget "tex" on-doc) disc-doc)
    (aput "on-hidden" (aget "hide" on-doc) disc-doc)
    (aput "on-path" (aget "neat-path" on-doc) disc-doc)
    (aput "on-title" (aget "title" on-doc) disc-doc)
    (aput "title" (fstr "Comments on ~a" (aget "title" on-doc)) disc-doc))
  disc-doc)

(defun fill-talk-doc (talk-doc)
  "Insert talk parameters into a talk document."
  (unless (aget "title" talk-doc)
    (err "Talk document ~a must have its own title" (aget "slug" talk-doc)))
  talk-doc)

(defun collect-cm-blocks (cm-docs)
  "Collect all blocks from the given comment documents."
  (let ((blocks))
    (dolist (cm-doc cm-docs)
      (dolist (blk (aget "blocks" cm-doc))
        (aput "on-hidden" (aget "on-hidden" cm-doc) blk)
        (aput "cm-page-path" (aget "neat-path" cm-doc) blk)
        (aput "on-path" (or (aget "on-path" cm-doc)
                            (aget "neat-path" cm-doc)) blk)
        (aput "on-title" (or (aget "on-title" cm-doc)
                             (aget "title" cm-doc)) blk)
        (push blk blocks)))
    (setf blocks (remove-if-keys blocks "on-hidden"))
    (setf blocks (sort-by-date blocks))
    (number-blocks blocks "cm-gserial")))

(defun render-cm-block (blk layout params)
  "Render a given comment document block"
  (let* ((rendered-body (render-body blk params))
         (rendered-body-param (list (cons "body" rendered-body))))
    (render layout (append rendered-body-param blk params))))

(defun render-cm-doc (cm-doc layouts params)
  "Render a given comment document."
  (let* ((slug (aget "slug" cm-doc))
         (doc-type (aget "doc-type" cm-doc))
         (cm-doc-layout (or (aget (fstr "cm-~a-list" slug) layouts)
                            (aget (fstr "cm-~a-list" doc-type) layouts)))
         (cm-item-layout (or (aget (fstr "cm-~a-item" slug) layouts)
                             (aget "cm-item" layouts)))
         (blocks (aget "blocks" cm-doc))
         (count (length blocks))
         (label (plural count "comment"))
         (cm-params (append params (list (cons "cm-count" count)
                                         (cons "cm-label" label)
                                         (cons "root" (aget "root" cm-doc)))))
         (rendered-blocks (mapp #'render-cm-block blocks cm-item-layout cm-params))
         (dst-path (aget "dst-path" cm-doc)))
    (let ((body-params (list (cons "body" (join-string-list rendered-blocks))
                             (cons "cslug" slug))))
      (write-log "Writing ~a ~a" (aget "doc-type" cm-doc) dst-path)
      (write-file dst-path (render cm-doc-layout (append body-params cm-doc params))))))

(defun render-cm-vdocs (all-blocks layouts params)
  "Render all comments and no comments pages."
  (dolist (cm-doc (list (list (cons "blocks" (reverse all-blocks))
                              (cons "doc-path" "cm/index.html")
                              (cons "title" "All Comments")
                              (cons "tex" "yes"))
                        (list (cons "blocks" nil)
                              (cons "doc-path" "cm/none.html")
                              (cons "title" "No Comments"))))
    (setf cm-doc (fill-path cm-doc params))
    (setf cm-doc (fill-head cm-doc layouts "comment.css" params))
    (render-cm-doc cm-doc layouts params)))


;;; Document List Renderer
;;; ----------------------

(defun render-item (doc item-layout root params)
  "Render a document using the given list-item layout."
  (render item-layout (append (list (cons "root" root)) doc params)))

(defun render-list (docs vdoc list-layout item-layout params &optional layouts)
  "Render and write a page containing a list of documents."
  (setf vdoc (fill-path vdoc params))
  (setf vdoc (fill-head vdoc layouts "" params))
  (let* ((rendered-items (mapp #'render-item docs item-layout
                               (aget "root" vdoc) params))
         (body-params (list (cons "body" (join-string-list rendered-items))))
         (rendered-page (render list-layout (append body-params vdoc params))))
    (setf rendered-page (refine-headings rendered-page))
    (write-log "Writing list ~a" (aget "dst-path" vdoc))
    (write-file (aget "dst-path" vdoc) rendered-page)))


;;; Tags
;;; ----

(defun make-tag-map (docs)
  "Group pages by tags; return a map of tags and page lists."
  (let ((tag-map (hmake)))
    (dolist (doc docs)
      (dolist (tag (csv-split (aget "tag" doc)))
        (hpush tag doc tag-map)))
    tag-map))

(defun sort-tag-count-pairs (tag-map)
  "Return keys sorted by their list-value lengths in descending order."
  (let (pairs)
    (maphash (lambda (k v) (push (cons k (length v)) pairs)) tag-map)
    (sort pairs #'> :key #'cdr)))

(defun tag-title (name tag-titles params)
  "Determine the title for a tag page."
  (render (or (aget name tag-titles) (aget nil tag-titles))
          (list* (cons "tag-name" name) params)))

(defun render-tag-index (tag-count-pairs layouts params)
  "Render the index of all tags."
  (let ((docs (loop for (name . count) in tag-count-pairs
                    collect (list (cons "tag-name" name)
                                  (cons "tag-slug" (tag-slug name))
                                  (cons "count" count)
                                  (cons "page-label" (plural count "page")))))
        (vdoc (list (cons "doc-path" "tag/index.html")
                    (cons "title" (fstr "~a's Tags" (aget "nick" params)))
                    (cons "subtitle" "")
                    (cons "all-tags-count" (length tag-count-pairs))
                    (cons "all-tags-label" (plural (length tag-count-pairs) "tag")))))
    (render-list docs vdoc
                 (aget "tag-index-list" layouts)
                 (aget "tag-index-item" layouts) params)))

(defun render-tag-page (tag-count tag-map tag-titles layouts params)
  "Render a listing page for one tag."
  (let* ((name (car tag-count))
         (count (cdr tag-count))
         (docs (hget name tag-map))
         (vdoc (list (cons "doc-path" (fstr "tag/~a.html" (tag-slug name)))
                     (cons "title" (tag-title name tag-titles params))
                     (cons "subtitle" "")
                     (cons "tag-name" name)
                     (cons "tag-slug" (tag-slug name))
                     (cons "count" count)
                     (cons "page-label" (plural count "page")))))
    (render-list (reverse (sort-by-date (copy-list docs))) vdoc
                 (aget "tag-page-list" layouts)
                 (aget "tag-page-item" layouts) params)))


;;; Feeds
;;; -----

(defun fill-feed-doc (doc params)
  "Add the rendered, XML-escaped body to a feed document."
  (let ((rendered-body (render-body doc params)))
    (aput "escaped-body" (xml-escape rendered-body) doc))
  doc)

(defun render-tag-feed (tag-count tag-map tag-titles layouts params)
  "Render the Atom feed for one tag."
  (let* ((name (car tag-count))
         (docs (remove-if-keys (hget name tag-map) "draft"))
         (docs (reverse (last-n 20 (sort-by-date (copy-list docs)))))
         (docs (mapp #'fill-feed-doc docs params))
         (path (fstr "tag/~a.xml" (tag-slug name)))
         (title (tag-title name tag-titles params))
         (updated (if docs (aget "iso-date" (first docs)) "1970-01-01T00:00:00Z"))
         (vdoc (list (cons "doc-path" path)
                     (cons "title" title)
                     (cons "description" (fstr "Feed for ~a" title))
                     (cons "feed-id" (neat-url path params))
                     (cons "iso-date" updated))))
    (render-list docs vdoc
                 (aget "feed-list" layouts)
                 (aget "feed-item" layouts) params)))

(defun render-feed (docs layouts params)
  "Render the site-wide Atom feed."
  (let* ((docs (remove-if-keys docs "draft"))
         (docs (select-if (lambda (doc) (string>= (aget "date" doc) "2026-05-03")) docs))
         (docs (reverse (last-n 20 (sort-by-date (copy-list docs)))))
         (docs (mapp #'fill-feed-doc docs params))
         (updated (if docs (aget "iso-date" (first docs)) "1970-01-01T00:00:00Z"))
         (vdoc (list (cons "doc-path" "feed.xml")
                     (cons "title" (aget "author" params))
                     (cons "description" (fstr "~a's Feed" (aget "nick" params)))
                     (cons "feed-id" (fstr "urn:uuid:~a" (aget "site-uuid" params)))
                     (cons "iso-date" updated))))
    (render-list docs vdoc
                 (aget "feed-list" layouts)
                 (aget "feed-item" layouts) params)))

(defun xsl-params (params)
  "Add CSS content to the parameters for the feed stylesheet."
  (let ((css (join-string-list
              (list (read-file (fstr "~acss/main.css" (aget "pub" params)))
                    (read-file (fstr "~acss/extra.css" (aget "pub" params)))))))
    (cons (cons "css" (fstr "~%~a" (indent-lines 10 css))) params)))


;;; Page Lists
;;; ----------

(defun render-all-page (docs layouts params)
  "Render the listing of all pages."
  (let* ((count (length docs))
         (vdoc (list (cons "doc-path" "pages.html")
                     (cons "title" "All Pages")
                     (cons "subtitle" "")
                     (cons "count" count)
                     (cons "page-label" (plural count "page")))))
    (render-list (reverse (sort-by-date (copy-list docs))) vdoc
                 (aget "all-page-list" layouts)
                 (aget "all-page-item" layouts) params)))

(defun render-zone (zone doc-path-map docs layouts params)
  "Render a listing page for a content zone."
  (let ((zone-key (first zone))
        (zone-dst-path (second zone)))
    ;; Don't clobber existing document with zone list.
    (unless (hget zone-dst-path doc-path-map)
      (let* ((docs (select-if-zone docs zone))
             (zone-title (third zone))
             (count (length docs))
             (vdoc (list (cons "doc-path" zone-dst-path)
                         (cons "title" (render zone-title params))
                         (cons "subtitle" "")
                         (cons "count" count)
                         (cons "page-label" (plural count "page"))))
             (zone-layout (aget (fstr "~a-list" zone-key) layouts))
             (item-layout (aget "tag-page-item" layouts)))
        (render-list (reverse (sort-by-date docs))
                     vdoc zone-layout item-layout params)))))


;;; Directory Lists
;;; ---------------

(defun format-size (size)
  "Convert a byte count to a human-readable size."
  (let ((powers (list (cons (expt 2 30) "G")
                      (cons (expt 2 20) "M")
                      (cons (expt 2 10) "K")
                      (cons (expt 2 0) "B")))
        (chosen-power)
        (chosen-suffix))
    (dolist (entry powers)
      (setf chosen-power (car entry))
      (setf chosen-suffix (cdr entry))
      (when (<= chosen-power size)
        (return)))
    (fstr "~a&nbsp;~a" (round (/ size chosen-power)) chosen-suffix)))

(defun render-ls (current-pathname apex-pathname path-sizes layouts params)
  "Render directory index pages for the given pathname."
  (let* ((apex-path-length (length (namestring apex-pathname)))
         (rel-path (subseq (namestring current-pathname) apex-path-length))
         (rel-path (if (string= rel-path "") "/" rel-path))
         (list-layout (aget "ls-list" layouts))
         (item-layout (aget "ls-item" layouts))
         (items))
    (setf path-sizes (sort (copy-list path-sizes) #'string< :key #'car))
    (dolist (entry path-sizes)
      (let* ((path (car entry))
             (size (cdr entry))
             (neat-path path))
        (when (string-ends-with "/" neat-path)
          (setf neat-path (fstr "~aindex.html" neat-path)))
        (unless (string= neat-path "index.html")
          (setf neat-path (neat-path neat-path params)))
        (push (list (cons "path" path)
                    (cons "size" (if size (format-size size) "-"))
                    (cons "neat-path" neat-path))
              items)))
    (setf items (nreverse items))
    (dolist (filename '("index.html" "ls.html"))
      (let* ((dst-path (merge-pathnames filename current-pathname))
             (vdoc (list (cons "doc-path" (subseq (namestring dst-path) apex-path-length))
                         (cons "title" (fstr "Index of ~a" rel-path))
                         (cons "subtitle" ""))))
        (unless (probe-file dst-path)
          (render-list items vdoc list-layout item-layout params))))))

(defun make-ls (apex-pathname current-pathname layouts params)
  "Recursively create directory index pages and return total size."
  (let ((total-size 0)
        (path-sizes))
    (dolist (path (uiop:subdirectories current-pathname))
      (let* ((basename (directory-basename path))
             (child-pathname (merge-pathnames basename current-pathname))
             (size (make-ls apex-pathname child-pathname layouts params)))
        (push (cons basename size) path-sizes)
        (incf total-size size)))
    (dolist (path (uiop:directory-files current-pathname))
      (let ((size (with-open-file (stream path) (file-length stream))))
        (push (cons (file-namestring path) size) path-sizes)
        (incf total-size size)))
    (unless (equal apex-pathname current-pathname)
      (push (cons "../" nil) path-sizes))
    (push (cons "./" total-size) path-sizes)
    (render-ls current-pathname apex-pathname path-sizes layouts params)
    total-size))


;;; Backlinks
;;; ---------

(defun parse-domain (url)
  "Parse printable domain name from URL."
  (let* ((start-index (+ (search "://" url) 3))
         (end-index (position #\/ url :start start-index))
         (host (subseq url start-index end-index)))
    (string-replace "www." "" host)))

(defun format-backlink (text params)
  "Encode special characters and expand placeholders."
  (render (string-replace "&" "&amp;" text) params))

(defun render-backlinks (layouts params)
  "Render the backlinks page."
  (let* ((backlinks (read-list "content/lisp/backlinks.lisp"))
         (list-layout (aget "backlink-list" layouts))
         (item-layout (aget "backlink-item" layouts))
         (vdoc (list (cons "doc-path" "backlinks.html")
                     (cons "title" "Backlinks")
                     (cons "subtitle" "")
                     (cons "count" (length backlinks))))
         (docs))
    (validate-date-order backlinks)
    (dolist (backlink backlinks)
      (push (list (cons "short-date" (short-date (getf backlink :date)))
                  (cons "lang" (if (string= (getf backlink :lang) "English") ""
                                   (fstr " (~a)" (getf backlink :lang))))
                  (cons "domain" (parse-domain (getf backlink :url1)))
                  (cons "txt1" (format-backlink (getf backlink :txt1) params))
                  (cons "url1" (format-backlink (getf backlink :url1) params))
                  (cons "txt2" (format-backlink (getf backlink :txt2) params))
                  (cons "url2" (format-backlink (getf backlink :url2) params)))
            docs))
    (setf docs (nreverse docs))
    (render-list docs vdoc list-layout item-layout params)))


;;; Blogroll
;;; --------

(defun render-blogroll (layouts params)
  "Render the blogroll page and OPML export."
  (let* ((entries (read-list "content/lisp/roll.lisp"))
         (html-list-layout (aget "roll-html-list" layouts))
         (html-item-layout (aget "roll-html-item" layouts))
         (opml-list-layout (aget "roll-opml-list" layouts))
         (opml-item-layout (aget "roll-opml-item" layouts))
         (title (fstr "~a's Blogroll" (aget "nick" params)))
         (html-items)
         (opml-items))
    (validate-name-order entries)
    (dolist (entry entries)
      (push (list (cons "name" (getf entry :name))
                  (cons "home" (getf entry :home))
                  (cons "feed" (getf entry :feed))
                  (cons "domain" (parse-domain (getf entry :home))))
            html-items)
      (push (list (cons "name" (xml-escape (getf entry :name)))
                  (cons "home" (xml-escape (getf entry :home)))
                  (cons "feed" (xml-escape (getf entry :feed))))
            opml-items))
    (setf html-items (nreverse html-items))
    (setf opml-items (nreverse opml-items))
    (render-list html-items
                 (list (cons "doc-path" "roll.html")
                       (cons "title" title)
                       (cons "subtitle" "")
                       (cons "count" (length entries)))
                 html-list-layout html-item-layout params)
    (render-list opml-items
                 (list (cons "doc-path" "roll.opml")
                       (cons "title" (xml-escape title))
                       (cons "count" (length entries)))
                 opml-list-layout opml-item-layout params)))


;;; Meets
;;; -----

(defun format-meet-date (universal-time)
  "Format a meeting date for display in a meeting log."
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time universal-time 0)
    (declare (ignore second))
    (format nil "~a,&nbsp;~2,'0d&nbsp;~a&nbsp;~4,'0d&nbsp;~2,'0d:~2,'0d&nbsp;UTC"
            (weekday-name day) date (month-name month) year hour minute)))

(defun future-meet-p (meet)
  "Check whether a meeting is scheduled for the future."
  (minusp (getf meet :members)))

(defun meet-log-path (current-track-slug other-slug)
  "Return the relative path to a meeting track log page."
  (fstr "~a~alog.html"
        (if current-track-slug "../" "")
        (if other-slug (fstr "~a/" other-slug) "")))

(defun meet-tracks-html (current-track-slug tracks)
  "Create HTML links to all meeting tracks."
  (join-string-list
   (loop for (slug name) in tracks
         collect (fstr "<li><a href=\"~a\">~a</a></li>~%"
                       (meet-log-path current-track-slug slug)
                       (if slug name "All Meetings")))))

(defun make-meet-row (meet previous-meet tracks row-id)
  "Create the parameters for one meeting log row."
  (let* ((upcoming (if (and previous-meet
                            (not (future-meet-p previous-meet))
                            (future-meet-p meet))
                       " id=\"upcoming\""
                       ""))
         (track (find-if (lambda (x) (string= (getf meet :slug) (first x))) tracks))
         (name (second track)))
    (list (cons "row-id" row-id)
          (cons "upcoming" upcoming)
          (cons "class" (if (future-meet-p meet) "future" "past"))
          (cons "date" (format-meet-date (parse-content-date (getf meet :date))))
          (cons "duration" (getf meet :duration))
          (cons "members" (if (future-meet-p meet) "-" (getf meet :members)))
          (cons "topic" (fstr "~a: ~a" name (getf meet :topic))))))

(defun make-meet-rows (meets tracks)
  "Create the parameters for all meeting log rows."
  (loop for meet in meets
        for previous-meet = nil then meet
        for row-id from 1
        collect (make-meet-row meet previous-meet tracks row-id)))

(defun render-meet-log (track meets tracks layouts params)
  "Render a meeting log page with statistics for one track."
  (when (first track)
    (setf meets (select-if (lambda (m) (string= (getf m :slug) (first track))) meets)))
  (let* ((slug (first track))
         (name (second track))
         (past-count (count-if-not #'future-meet-p meets))
         (past-meets (remove-if #'future-meet-p meets))
         (total-minutes (loop for m in past-meets sum (getf m :duration)))
         (total-members (loop for m in past-meets sum (getf m :members)))
         (divisor (max past-count 1))
         (list-layout (aget "meet-list" layouts))
         (item-layout (aget "meet-item" layouts))
         (title (fstr "~a Meeting Log" name))
         (doc-path (if slug (fstr "cc/~a/log.html" slug) "cc/log.html"))
         (row-docs (make-meet-rows meets tracks))
         (vdoc (list (cons "doc-path" doc-path)
                     (cons "title" title)
                     (cons "subtitle" "")
                     (cons "import" "extra.css, meet.css")
                     (cons "tex" "yes")
                     (cons "track-name" (if slug name "club"))
                     (cons "other-tracks" (meet-tracks-html slug tracks))
                     (cons "total-count" past-count)
                     (cons "meeting-label" (if (= past-count 1) "meeting" "meetings"))
                     (cons "total-minutes" total-minutes)
                     (cons "total-hours" (fstr "~,1f" (/ total-minutes 60)))
                     (cons "average-minutes" (fstr "~,1f" (/ total-minutes divisor)))
                     (cons "average-members" (fstr "~,1f" (/ total-members divisor))))))
    (render-list row-docs vdoc list-layout item-layout params layouts)))

(defun render-meet-logs (layouts params tracks)
  "Render the full meeting log and each per-track log."
  (let* ((meets (read-list "content/lisp/meet.lisp"))
         (tracks (append tracks (list (list nil "Full")))))
    (validate-date-order meets)
    (mapp #'render-meet-log tracks meets tracks layouts params)))


;;; Complete Website
;;; ----------------

(defvar *params* nil
  "Global parameters that may be provided externally to override any
  default local parameters.")

(defun read-layouts (src-dir)
  "Read layout files from given source directory."
  (loop for src-path in (uiop:directory-files src-dir)
        collect (cons (pathname-name src-path)
                      (read-content (read-file src-path)))))

(defun fill-layout (layout layouts)
  "Render each layout within its parent layout (if any) iteratively."
  (let* ((layout-name (car layout))
         (layout-alist (cdr layout))
         (result (aget "body" layout-alist))
         parent-name)
    (loop
      (unless (setf parent-name (aget "layout" layout-alist))
        (return))
      (setf layout-alist (aget parent-name layouts))
      ;; Replace {{ body }} in parent layout text with current layout text.
      (setf result (render (aget "body" layout-alist)
                           (list (cons "body" result)))))
    (cons layout-name result)))

(defun make-ccslug-map (docs)
  "Map unique document slugs and explicit ccslugs to document metadata."
  (let ((count-map (hmake))
        (ccslug-map (hmake)))
    ;; Count documents for each slug.
    (dolist (doc docs)
      (let ((slug (aget "slug" doc)))
        (hset slug (1+ (or (hget slug count-map) 0)) count-map)))
    ;; Add only slugs that identify a single document.
    (dolist (doc docs)
      (let ((slug (aget "slug" doc)))
        (when (= 1 (hget slug count-map))
          (hset slug doc ccslug-map))))
    ;; Add ccslug aliases.  Reject collision with an existing target.
    (dolist (doc docs)
      (let ((ccslug (aget "ccslug" doc)))
        (when ccslug
          (when (hget ccslug ccslug-map)
            (err "Document ~a has ccslug ~a which conflicts with slug/ccslug of document ~a"
                 (aget "doc-path" doc) ccslug (aget "doc-path" (hget ccslug ccslug-map))))
          (hset ccslug doc ccslug-map))))
    ccslug-map))

(defun make-doc-path-map (docs)
  "Map document paths to their document metadata."
  (let ((doc-path-map (hmake)))
    (dolist (doc docs)
      (hset (aget "doc-path" doc) doc doc-path-map))
    doc-path-map))

(defun make-cm-map (cm-docs)
  "Create a hash table containing the document slugs."
  (let ((cm-map (hmake)))
    (dolist (cm-doc cm-docs)
      (let ((slug (aget "slug" cm-doc)))
        (when (hget slug cm-map)
          (err "Duplicate comment document slug: ~a" slug))
        (hset slug cm-doc cm-map)))
    cm-map))

(defun main ()
  "Generate website."
  (let* ((config (read-list "config.lisp"))
         (params (list (cons "index" "")
                       (cons "import" "main.css")
                       (cons "pub" "_site/")
                       (cons "year" (nth-value 5 (get-decoded-time)))
                       (cons "zone-link" "")))
         (params (nconc *params* (aget "params" config) params))
         (raw-layouts (read-layouts "layout/"))
         (layouts (mapp #'fill-layout raw-layouts raw-layouts))
         (zones (aget "zones" config))
         (docs (mapp #'fill-path (find-docs "content/tree/") params))
         (fancy-docs (select-of-types docs "aux" "page" "post" "talk"))
         (fancy-docs (mapp #'fill-fancy-doc fancy-docs params))
         (listed-docs)
         (pp-docs (select-of-types fancy-docs "page" "post"))
         (disc-docs (select-of-types docs "disc"))
         (talk-docs (select-of-types fancy-docs "talk"))
         (ccslug-map (make-ccslug-map (select-of-types docs "aux" "page" "post")))
         (doc-path-map (make-doc-path-map docs))
         (tag-map)
         (tag-count-pairs)
         (cm-docs (append disc-docs talk-docs))
         (cm-map (make-cm-map cm-docs))
         (cm-all-blocks))
    (validate-required-params fancy-docs)
    (validate-unique-dates fancy-docs)
    (validate-comment-slugs docs)
    ;; Set up dependencies.
    (remove-directory (aget "pub" params))
    (copy-directory "_fs/pub/" (aget "pub" params))
    ;; Fill documents.
    (setf pp-docs (mapp #'fill-pp-doc pp-docs cm-map zones params))
    (setf cm-docs (append (mapp #'fill-disc-doc disc-docs ccslug-map)
                          (mapp #'fill-talk-doc talk-docs)))
    (setf cm-docs (mapp #'fill-cm-doc cm-docs params))
    (setf listed-docs
          (remove-if-keys (append pp-docs (select-of-types fancy-docs "aux")
                                  (select-of-types cm-docs "talk")) "hide"))
    (setf tag-map (make-tag-map listed-docs))
    (setf tag-count-pairs (sort-tag-count-pairs tag-map))
    ;; Fill imports.
    (setf pp-docs (mapp #'fill-head pp-docs layouts "" params))
    (setf cm-docs (mapp #'fill-head cm-docs layouts "comment.css" params))
    (setf cm-all-blocks (collect-cm-blocks cm-docs))
    ;; Render output documents.
    (mapp #'render-doc (select-of-types docs "css") layouts (aget "style" config))
    (mapp #'render-doc (select-of-types docs "xsl") layouts (xsl-params params))
    (mapp #'render-doc pp-docs layouts params)
    (mapp #'render-cm-doc cm-docs layouts params)
    (render-cm-vdocs cm-all-blocks layouts params)
    (render-backlinks layouts params)
    (render-blogroll layouts params)
    (render-meet-logs layouts params (aget "meets" config))
    (render-all-page listed-docs layouts params)
    (mapp #'render-zone zones doc-path-map listed-docs layouts params)
    (render-tag-index tag-count-pairs layouts params)
    (mapp #'render-tag-page tag-count-pairs tag-map (aget "tags" config) layouts params)
    (mapp #'render-tag-feed tag-count-pairs tag-map (aget "tags" config) layouts params)
    (render-feed listed-docs layouts params)
    ;; Copy raw files.
    (mapp #'copy-doc (select-of-types docs "raw"))
    (make-ls (aget "pub" params) (aget "pub" params) layouts params)))

(when *site-mode*
  (main))
