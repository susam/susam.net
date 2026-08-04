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
  "Return the parent directory of the specified pathname."
  (let ((name (car (last (pathname-directory path)))))
    (namestring (make-pathname :directory (list :relative name)))))

(defun copy-directory (src dst)
  "Copy directory from a directory path to a directory path"
  (make-directory dst)
  (dolist (pathname (uiop:directory-files src))
    (let* ((basename (file-namestring pathname))
           (destpath (merge-pathnames basename dst)))
      (uiop:copy-file pathname destpath)))
  (dolist (pathname (uiop:subdirectories src))
    (let* ((basename (directory-basename pathname))
           (destpath (merge-pathnames basename dst)))
      (copy-directory pathname destpath))))

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
  "Write text to file and close the file."
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
  "Test that the string ends with the given prefix."
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

(defun join-strings (strings)
  "Join strings into a single string."
  (apply #'concatenate 'string strings))

(defun repeat-string (count string)
  "Repeat string count number of times."
  (join-strings (loop repeat count collect string)))

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
  "Convert universal-time (integer) to RFC-2822 date string."
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


;;; Content Parsing and Rendering
;;; -----------------------------

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
    ;; Date of the first block becomes the document date.
    (aput "date" (aget "date" (car blocks)) doc)
    (aput "blocks" blocks doc)
    doc))

(defun render (template params)
  "Render parameter tokens in template with their values from params."
  (with-output-to-string (s)
    (let* ((ltoken "{{ ")
           (rtoken " }}")
           (next-index 0)     ; Next place to start searching "{{ ".
           (start-index)      ; Starting of "{{ ".
           (end-index))       ; Starting of " }}".
      (loop
        ;; Look for ltoken and extract static text before it.
        (setf start-index (search ltoken template :start2 next-index))
        (unless start-index
          (return))
        (format s "~a" (subseq template next-index start-index))
        ;; Extract parameter name between ltoken and rtoken.
        (incf start-index (length ltoken))
        (setf end-index (search rtoken template :start2 start-index))
        (let* ((key (subseq template start-index end-index))
               (val (aget key params)))
          ;; If key exists in params, replace key with value.
          ;; Otherwise, leave the key intact in text.
          (if val
              (format s "~a" val)
              (format s "~a~a~a" ltoken key rtoken)))
        (setf next-index (+ end-index (length rtoken))))
      ;; Extract static text after the last parameter token.
      (format s "~a" (subseq template next-index)))))


;;; Document Reader
;;; ---------------

(defun doc-path (src-path src-dir remove-suffix add-suffix)
  "Derive document path relative to source directory."
  (let* ((rel-path (enough-namestring (truename src-path) (truename src-dir)))
         (base-len (- (length rel-path) (length remove-suffix))))
    (concatenate 'string (subseq rel-path 0 base-len) add-suffix)))

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
           (aput "doc-path" (doc-path src-path src-dir ".aux.html" ".html") doc)
           (setf doc (nconc doc (read-content (read-file src-path)))))
          ;; Comment files.
          ((string-ends-with ".cm.html" basename)
           (aput "doc-type" "cm" doc)
           (aput "doc-path" (doc-path src-path src-dir ".cm.html" ".html") doc)
           (setf doc (nconc doc (read-multi-content (read-file src-path)))))
          ;; CSS files.
          ((string-ends-with ".css.css" basename)
           (aput "doc-type" "css" doc)
           (aput "doc-path" (doc-path src-path src-dir ".css.css" ".css") doc)
           (setf doc (nconc doc (read-content (read-file src-path)))))
          ;; Renderable pages.
          ((string-ends-with ".page.html" basename)
           (aput "doc-type" "page" doc)
           (aput "doc-path" (doc-path src-path src-dir ".page.html" ".html") doc)
           (setf doc (nconc doc (read-content (read-file src-path)))))
          ;; Renderable posts.
          ((string-ends-with ".post.html" basename)
           (aput "doc-type" "post" doc)
           (aput "doc-path" (doc-path src-path src-dir ".post.html" ".html") doc)
           (setf doc (nconc doc (read-content (read-file src-path)))))
          ;; Raw files
          (t
           (aput "doc-type" "raw" doc)
           (aput "doc-path" (doc-path src-path src-dir "" "") doc)))
        (push doc docs)))
    docs))


;;; Fill Head
;;; ---------

(defun head-html (imports root includes)
  "Given a list of import names, return HTML for all given imports."
 (let ((html)
        (snippets))
    (dolist (name imports)
      (cond ((string-ends-with ".css" name)
             (setf html (fstr "  <link rel=\"stylesheet\" href=\"~acss/~a\">~%" root name)))
            ((string-ends-with ".js" name)
             (setf html (fstr "  <script src=\"~ajs/~a\"></script>~%" root name)))
            ((string-ends-with ".inc" name)
             (setf html (render (aget name includes) (list (cons "root" root)))))
            (t
             (err "Unknown import type ~a" name)))
      (push html snippets))
    (join-strings (reverse snippets))))

(defun fill-head (doc includes extra-imports params)
  "Insert head parameter into the given document."
  (aput "head" (head-html (append (string-split (aget "import" params) ", ")
                                  (string-split (aget "import" doc) ", ")
                                  (string-split extra-imports ", "))
                          (aget "root" doc)
                          includes)
        doc)
  doc)


;;; Fill Paths
;;; ----------

(defun root-path (path)
  "Return relative path to web root from the given rendered file path."
  (let ((depth (count #\/ path))) ;; foo/bar/baz.html => 2 => ../../
    (if (zerop depth) "./" (repeat-string depth "../"))))

(defun neat-path (path params)
  "Create canonical path component of the URL for the given rendered file path."
  (string-replace "index.html" (aget "index" params) path))

(defun neat-url (path params)
  (concatenate 'string (aget "site-url" params)
               (string-replace "index.html" "" path)))

(defun fill-path (doc params)
  "Insert path parameters into the given document."
  (aput "dst-path" (render "{{ pub }}{{ doc-path }}" (append doc params)) doc)
  (let* ((doc-path (aget "doc-path" doc)))
    (aput "root" (root-path doc-path) doc)
    (aput "neat-path" (neat-path doc-path params) doc)
    (aput "neat-url" (neat-url doc-path params) doc))
  doc)


;;; Fill Renderable Document Parameters
;;; -----------------------------------

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
  (let* ((doc-path (aget "doc-path" doc))
         (doc-zones (string-split (aget "zone" doc) ", "))
         (zone (find-if (lambda (z)
                          (or (string-starts-with (first z) doc-path)
                              (member (first z) doc-zones :test #'string=))) zones)))
    (if zone (fstr "~%    <a href=\"~a\">~a</a>"
                   (render (second zone) (append doc params)) (third zone)) "")))

(defun doc-slug (doc)
  "Determine document slug from a document path."
  (pathname-name (aget "doc-path" doc)))

(defun update-mark (date)
  "Format update date. "
  (concatenate 'string " | Updated on "
               (format-short-date (parse-content-date date))))

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
    (let* ((h-begin-index)              ; -> <h2 id="foo">
           (h-close-index)              ; -> </h2>
           (id-end-index)               ; <h2 id="foo" <-
           (title-begin-index)          ; <h2 id="foo">F <-
           (next-index 0)               ; <h2 <-
           (indent -1)                  ; We want the first indent to be 0.
           (new-level 1)                ; We want the first heading to be <h2>.
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

(defun fill-list-doc (doc)
  "Insert placeholder values needed when listing a document."
  (aput "draft-mark" (if (aget "draft" doc) " [draft]" "") doc)
  (aput "short-date" (format-short-date
                      (parse-content-date (aget "date" doc))) doc)
  doc)

(defun fill-ren-doc (doc cm-map zones params)
  "Insert placeholder values to renderable documents."
  (setf doc (fill-list-doc doc))
  (aput "zone-link" (zone-link doc zones params) doc)
  (let* ((cm-doc (hget (or (aget "cmid" doc) (doc-slug doc)) cm-map))
         (cm-path (if cm-doc (aget "neat-path" cm-doc) "cm/none.html"))
         (date (aget "date" doc))
         (tags (string-split (aget "tag" doc) ", "))
         (toc (aget "toc" doc))
         (update (aget "update" doc)))
    (aput "cm-path" cm-path doc)
    (aput "iso-date" (format-iso-date (parse-content-date date)) doc)
    (aput "tags-for-page" (format-tags tags 2 (aget "root" doc)) doc)
    (aput "tags-for-list" (format-tags tags 4 "") doc)
    (aput "tags-for-feed" (format-tags tags 2 (aget "site-url" params)) doc)
    (aput "toc" (toc-html (aget "body" doc) (string= toc "num")) doc)
    (aput "update-mark" (if update (update-mark update) "") doc))
  doc)


;;; Document Renderers
;;; ------------------

(defun render-doc (doc layouts params)
  "Render the given document within a layout matching its document type."
  (let* ((layout (aget (aget "doc-type" doc) layouts))
         (body (render (aget "body" doc) (append doc params)))
         (body-param (list (cons "body" body))))
    (write-log "Writing ~a ~a" (aget "doc-type" doc) (aget "dst-path" doc))
    (write-file (aget "dst-path" doc)
                (render layout (append body-param doc params)))))

(defun copy-doc (doc)
  "Copy a raw document file to publication directory."
  (write-log "Copying ~a ~a" (aget "doc-type" doc) (aget "dst-path" doc))
  (copy-file (aget "src-path" doc) (aget "dst-path" doc)))


;;; Sorting, Filtering and Validations
;;; ----------------------------------

(defun sort-by-date (items)
  "Sort items in chronological order."
  (sort items (lambda (x y) (string< (aget "date" x) (aget "date" y)))))

(defun yes-p (string)
  "Check if the given string has the value 'yes'."
  (string= string "yes"))

(defun filter-out (items &rest keys)
  "Remove items whose ignored keys are set."
  (remove-if (lambda (item) (some (lambda (key) (yes-p (aget key item))) keys)) items))


;;; Comments
;;; --------

(defun number-blocks (blocks serial-key)
  "Insert one-based serial number to each comment block."
  (loop for blk in blocks
        for serial from 1
        do (aput serial-key serial blk)
        collect blk))

(defun fill-cm-block (blk params)
  "Insert comment parameters into a comment block."
  (let* ((author (aget "author" params))
         (date (aget "date" blk))
         (cm-name (aget "name" blk))
         (cm-url (if (string= cm-name author)
                     (aget "site-url" params)
                     (aget "url" blk))))
    (aput "cm-author" (if cm-url (fstr "<a href=\"~a\">~a</a>" cm-url cm-name) cm-name) blk)
    (aput "cm-class" (if (string= cm-name author) "author" "visitor") blk)
    (aput "cm-long-date" (format-long-date (parse-content-date date)) blk))
  blk)

(defun fill-cm-doc (cm-doc doc-map params)
  "Insert comment parameters into a comment document."
  (let* ((blocks (aget "blocks" cm-doc))
         (cm-slug (doc-slug cm-doc))
         (on-doc (hget cm-slug doc-map))
         (self-title (aget "title" cm-doc))
         (tags (string-split (aget "tag" cm-doc) ", ")))
    (setf blocks (number-blocks blocks "cm-fserial"))
    (setf blocks (mapp #'fill-cm-block blocks params))
    (aset "blocks" blocks cm-doc)
    (aput "cmid" cm-slug cm-doc) ; Used in 'Post Comment' link.
    (aput "cm-count" (length blocks) cm-doc)
    (when (and on-doc self-title)
      (err "Comment document ~a cannot have both on-doc and self-title" cm-slug))
    (unless (or on-doc self-title)
      (error "Comment document ~a must have either on-doc or self-title" cm-slug))
    (when (aget "title" cm-doc)
      (aput "cm-doc-type" (or (aget "cm-doc-type" cm-doc) "cm-solo") cm-doc)
      (aput "tags-for-feed" (format-tags tags 2 (aget "site-url" params)) cm-doc)
      (aput "tags-for-list" (format-tags tags 4 "") cm-doc)
      (aput "tags-for-page" (format-tags tags 2 (aget "root" cm-doc)) cm-doc))
    (when on-doc
      (aput "cm-doc-type" "cm-on" cm-doc)
      (aput "import" (aget "import" on-doc) cm-doc)
      (aput "on-hidden" (aget "hide" on-doc) cm-doc)
      (aput "on-path" (aget "neat-path" on-doc) cm-doc)
      (aput "on-title" (aget "title" on-doc) cm-doc)
      (aput "title" (fstr "Comments on ~a" (aget "title" on-doc)) cm-doc)))
  cm-doc)

(defun collect-cm-all-blocks (cm-docs)
  "Collect all blocks from the given comment documents."
  (let ((cm-all))
    (dolist (cm-doc cm-docs)
      (dolist (blk (aget "blocks" cm-doc))
        (aput "on-hidden" (aget "on-hidden" cm-doc) blk)
        (aput "cm-page-path" (aget "neat-path" cm-doc) blk)
        (aput "on-path" (or (aget "on-path" cm-doc)
                            (aget "neat-path" cm-doc)) blk)
        (aput "on-title" (or (aget "on-title" cm-doc)
                             (aget "title" cm-doc)) blk)
        (push blk cm-all)))
    (setf cm-all (filter-out cm-all "on-hidden"))
    (setf cm-all (sort-by-date cm-all))
    (number-blocks cm-all "cm-gserial")))

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

(defun select-docs (docs &rest types)
  "Select documents that match the given types."
  (remove-if-not (lambda (doc)
                   (member (aget "doc-type" doc) types :test #'string=)) docs))

(defun render-cm-block (blk layout params)
  "Render a given comment document block"
  (let* ((rendered-body (render (aget "body" blk) params))
         (body-param (list (cons "body" rendered-body))))
    (render layout (append body-param blk params))))

(defun render-cm-doc (cm-doc layouts params)
  "Render a given comment document."
  (let* ((cm-doc-type (aget "cm-doc-type" cm-doc))
         (cm-item-type (fstr "~a-item" cm-doc-type))
         (cm-doc-layout (or (aget (fstr "~a-list" cm-doc-type) layouts)
                            (aget cm-doc-type layouts)))
         (cm-item-layout (or (aget cm-item-type layouts)
                             (aget "cm-on-item" layouts)))
         (blocks (aget "blocks" cm-doc))
         (count (length blocks))
         (label (plural count "comment"))
         (cm-params (append params (list (cons "cm-count" count)
                                         (cons "cm-label" label)
                                         (cons "root" (aget "root" cm-doc)))))
         (rendered-blocks (mapp #'render-cm-block blocks cm-item-layout cm-params))
         (body-params (list (cons "body" (join-strings rendered-blocks))))
         (dst-path (aget "dst-path" cm-doc)))
    (write-log "Writing ~a ~a" (aget "doc-type" cm-doc) dst-path)
    (write-file dst-path (render cm-doc-layout (append body-params cm-doc params)))))

(defun render-cm-vdocs (all-blocks layouts params)
  "Render all comments and no comments pages."
  (dolist (cm-doc (list (list (cons "blocks" (reverse all-blocks))
                              (cons "cm-doc-type" "cm-all")
                              (cons "doc-path" "cm/index.html")
                              (cons "title" "All Comments"))
                        (list (cons "blocks" nil)
                              (cons "cm-doc-type" "cm-none")
                              (cons "doc-path" "cm/none.html")
                              (cons "title" "No Comments"))))
    (setf cm-doc (fill-path cm-doc params))
    (setf cm-doc (fill-head cm-doc nil "comment.css" params))
    (render-cm-doc cm-doc layouts params)))


;;; Tags
;;; ----

(defun make-tag-map (docs)
  "Group pages by tags; return a map of tags and page lists."
  (setf docs (filter-out docs "hide"))
  (let ((tag-map (hmake)))
    (dolist (doc docs)
      (dolist (tag (string-split (aget "tag" doc) ", "))
        (hpush tag doc tag-map)))
    tag-map))

(defun tag-counts (tag-map)
  "Return keys sorted by their list-value lengths in descending order."
  (let (pairs)
    (maphash (lambda (k v) (push (cons k (length v)) pairs)) tag-map)
    (sort pairs #'> :key #'cdr)))

(defun render-item (doc item-layout root params)
  (render item-layout (append (list (cons "root" root)) doc params)))

(defun render-list (docs vdoc list-layout item-layout params)
  (setf vdoc (fill-path vdoc params))
  (setf vdoc (fill-head vdoc nil "" params))
  (let* ((rendered-items (mapp #'render-item docs item-layout
                               (aget "root" vdoc) params))
         (body-params (list (cons "body" (join-strings rendered-items)))))
    (write-log "Writing list ~a" (aget "dst-path" vdoc))
    (write-file (aget "dst-path" vdoc)
                (render list-layout (append body-params vdoc params)))))

(defun tag-title (name special-titles params)
  "Determine the title for a tag page."
  (render (or (aget name special-titles) "{{ nick }}'s {{ tag-name }} Pages")
          (list* (cons "tag-name" name) params)))

(defun render-tag-index (tag-counts layouts params)
  "Render the index of all tags."
  (let ((docs (loop for (name . count) in tag-counts
                    collect (list (cons "tag-name" name)
                                  (cons "tag-slug" (tag-slug name))
                                  (cons "count" count)
                                  (cons "page-label" (plural count "page")))))
        (vdoc (list (cons "doc-path" "tag/index.html")
                    (cons "title" (render "{{ nick }}'s Tags" params))
                    (cons "subtitle" "")
                    (cons "all-tags-count" (length tag-counts))
                    (cons "all-tags-label" (plural (length tag-counts) "tag")))))
    (render-list docs vdoc
                 (aget "tag-all-list" layouts)
                 (aget "tag-all-item" layouts) params)))

(defun render-tag (tag-count tag-map special-titles layouts params)
  (let* ((name (car tag-count))
         (count (cdr tag-count))
         (docs (hget name tag-map))
         (vdoc (list (cons "doc-path" (fstr "tag/~a.html" (tag-slug name)))
                     (cons "title" (tag-title name special-titles params))
                     (cons "subtitle" "")
                     (cons "tag-name" name)
                     (cons "tag-slug" (tag-slug name))
                     (cons "count" count)
                     (cons "page-label" (plural count "page")))))
    (format t ":::: tag: ~a -> ~a~%" name count)
    (render-list (reverse (sort-by-date docs)) vdoc
                 (aget "tag-page-list" layouts)
                 (aget "tag-page-item" layouts) params)))


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

(defun read-includes (src-dir)
  "Read include files from given source directory."
  (loop for src-path in (uiop:directory-files src-dir)
        collect (cons (file-namestring src-path)
                      (read-file src-path))))

(defun doc-on-id (doc)
  "Return the document's effective on-id used by comments."
  (or (aget "onid" doc) (doc-slug doc)))

(defun make-doc-map (docs)
  "Create hashtable to map document slugs to document metadata."
  (let ((count-map (hmake))
        (doc-map (hmake)))
    (dolist (doc docs)
      (let ((onid (doc-on-id doc)))
        (hset onid (1+ (or (hget onid count-map) 0)) count-map)))
    (dolist (doc docs)
      (let ((onid (doc-on-id doc)))
        (when (= 1 (hget onid count-map))
          (hset onid doc doc-map))))
    doc-map))

(defun make-cm-map (cm-docs)
  "Create a hash table containing the document slugs."
  (let ((cm-map (hmake)))
    (dolist (cm-doc cm-docs)
      (let ((slug (doc-slug cm-doc)))
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
         (includes (read-includes "includes/"))
         (zones (aget "zones" config))
         (all-docs (mapp #'fill-path (find-docs "content/tree/") params))
         (ren-docs (select-docs all-docs "page" "post"))
         (list-docs (mapp #'fill-list-doc (select-docs all-docs "aux" "page" "post")))
         (cm-docs (select-docs all-docs "cm"))
         (doc-map (make-doc-map (select-docs all-docs "aux" "page" "post")))
         (tag-map (make-tag-map list-docs))
         (tag-counts (tag-counts tag-map))
         (cm-map (make-cm-map cm-docs))
         (cm-all-blocks))
    ;; Set up dependencies.
    (remove-directory (aget "pub" params))
    (copy-directory "_cache/katex/" (render "{{ pub }}js/katex/" params))
    ;; Fill documents.
    (setf ren-docs (mapp #'fill-ren-doc ren-docs cm-map zones params))
    (setf cm-docs (mapp #'fill-cm-doc cm-docs doc-map params))
    ;; Fill imports.
    (setf ren-docs (mapp #'fill-head ren-docs includes "" params))
    (setf cm-docs (mapp #'fill-head cm-docs includes "comment.css" params))
    (setf cm-all-blocks (collect-cm-all-blocks cm-docs))
    ;; Render output documents.
    (mapp #'render-doc (select-docs all-docs "css") layouts (aget "style" config))
    (mapp #'render-doc ren-docs layouts params)
    (mapp #'render-cm-doc cm-docs layouts params)
    (render-cm-vdocs cm-all-blocks layouts params)
    (render-tag-index tag-counts layouts params)
    (mapp #'render-tag tag-counts tag-map (aget "tag-titles" config) layouts params)
    ;; Copy raw files.
    (mapp #'copy-doc (select-docs all-docs "raw"))))

(when *site-mode*
  (main))
