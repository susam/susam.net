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

(defun write-log (fmt &rest args)
  "Log message with specified arguments."
  (when *log-mode*
    (apply #'format t fmt args)
    (terpri)))

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

(defun string-split (string separator &key ignore-empty)
  "Split a string into a list of strings using the given separator."
  (let ((next-index 0)
        (match-index)
        (split)
        (result))
    (loop
      (setf match-index (search separator string :start2 next-index))
      (when (or (not match-index))
        (return))
      (setf split (subseq string next-index match-index))
      (unless (and (= (length split) 0) ignore-empty)
        (push split result))
      (setf next-index (+ match-index (length separator))))
    (when (< next-index (length string))
      (setf split (subseq string next-index match-index))
      (unless (and (= (length split) 0) ignore-empty)
        (push split result)))
    (reverse result)))

(defun join-strings (strings)
  "Join strings into a single string."
  (apply #'concatenate 'string strings))

(defun repeat-string (count string)
  "Repeat string count number of times."
  (join-strings (loop repeat count collect string)))

(defun aget (key alist)
  "Given a key, return its value found in alist."
  (cdr (assoc key alist :test #'string=)))

(defmacro aput (key value alist)
  "Add key-value pair to alist."
  `(push (cons ,key ,value) ,alist))

(defun hget (key table)
  "Given a key, return its value found in hash table."
  (gethash key table))

(defun hset (key value table)
  "Set key-value pair in hash table."
  (setf (gethash key table) value))


;;; Tool Definitions
;;; ----------------

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



;;; Documents
;;; ---------

(defun read-content (text)
  "Parse content file."
  (let ((doc))
    (multiple-value-bind (headers next-index) (read-headers text 0)
      (setf doc (append headers doc))
      (aput "body" (subseq text next-index) doc))
    doc))

(defun write-docs (docs)
  (setf docs (remove-if-not (lambda (d) (member (aget "doc-type" d) '("page" "post")
                                                :test #'string=)) docs)))

(defun head-html (head import root includes)
  "Given the value of an import header, return HTML code for it."
  (let ((names (append (string-split head ", ")
                       (string-split import ", ")))
        (html)
        (snippets))
    (dolist (name names)
      (cond ((string-ends-with ".css" name)
             (setf html (fstr "  <link rel=\"stylesheet\" href=\"~acss/~a\">~%" root name)))
            ((string-ends-with ".js" name)
             (setf html (fstr "  <script src=\"~ajs/~a\"></script>~%" root name)))
            ((string-ends-with ".inc" name)
             (setf html (render (aget name includes) (list (cons "root" root)))))
            (t
             (error "Unknown import type ~a" name)))
      (push html snippets))
    (join-strings snippets)))


;;; Tree
;;; ----

(defun parse-slug (filename suffix)
  (subseq filename 0 (- (length filename) (length suffix))))

(defun doc-path (src-path src-dir remove-suffix add-suffix)
  (let* ((rel-path (enough-namestring (truename src-path) (truename src-dir)))
         (base-len (- (length rel-path) (length remove-suffix))))
    (concatenate 'string (subseq rel-path 0 base-len) add-suffix)))

(defun all-files (src-dir)
  "Recursively find all files in the given directory."
  (append (uiop:directory-files src-dir)
          (mapcan #'all-files (uiop:subdirectories src-dir))))

(defun read-block (text start-index)
  "Read a single block from a multi-block content file."
  (let ((start-token "<!-- ")  ; Header delimiter.
        (blck)                 ; Parsed block headers and body.
        (next-index))          ; Index at which to search next subdoc.
    (setf (values blck start-index) (read-headers text start-index))
    (setf next-index (search start-token text :start2 start-index))
    (aput "body" (subseq text start-index next-index) blck)
    (values blck next-index)))

(defun read-blocks (text)
  "Read all comments from a comment file."
  (let ((next-index 0)
        (blocks)
        (blck))
    (loop
      (setf (values blck next-index) (read-block text next-index))
      (push blck blocks)
      (unless next-index
        (return)))
    (reverse blocks)))

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

(defun read-cm-doc (text)
  (let ((blocks (read-blocks text))
        (doc))
    (when (aget "title" (car blocks))
      (setf doc (car blocks))
      (setf blocks (cdr blocks)))
    (aput "date" (aget "date" (car blocks)) doc)
    (aput "blocks" blocks doc)
    doc))

(defun fill-cm-blocks (blocks params)
  (let ((author (aget "author" params))
        (serial 0)
        (results))
    (dolist (blck blocks)
      (let* ((date (aget "date" blck))
             (cm-name (aget "name" blck))
             (cm-url (if (string= cm-author author)
                         (aget "site-url" params)
                         (aget "url" blck))))
        (aput "cm-fserial" (incf serial))
        (aput "cm-author" (if cm-url (fstr "<a href=\"~a\">~a</a>" cm-url cm-name) cm-name))
        (aput "cm-class" (if (string= cm-name author) "author" "visitor"))
        (aput "cm-long-date" (format-long-date (parse-content-date date)) blck)
        (aput "cm-on-path" (aget ))
        )
      (push blck results))
    results))

(defun doc-slug (doc)
  (pathname-name (aget "doc-path" doc)))

(defun fill-cm (cm-docs doc-map cmo params)
  (format t ":::: cmo: ~a~%" cmo)
  (let ((results))
    (dolist (cm-doc cm-docs)
      (aput "blocks" (fill-cm-blocks (aget "blocks" cm-doc)) cm-doc)
      (let* ((cm-slug (doc-slug (cm-doc)))
             (doc-info (hget cm-slug doc-map))
             (date (aget "date" cm-doc))
             (tags (string-split (aget "tag" cm-doc) ", ")))
        (aput "import" (aget "import" cm-doc-info) cm-doc)
        (aput "short-date" (format-short-date (parse-content-date date)) cm-doc)
        (aput "tags-for-page" (format-tags tags 2 (aget "root" cm-doc)) cm-doc)
        (aput "tags-for-list" (format-tags tags 4 "") cm-doc)
        (aput "tags-for-feed" (format-tags tags 2 (aget "site-url" params)) cm-doc))
      (push cm-doc results))
    results))

(defun read-docs (src-dir)
  (let ((docs))
    (dolist (src-path (all-files src-dir))
      (let ((basename (file-namestring src-path))
            (src-path (enough-namestring src-path (truename ".")))
            (doc))
        (aput "src-path" src-path doc)
        ;; Both doc-type and doc-path are set in the following 'cond'.
        ;; The doc-path value decides all paths like dst-path, neat-path, etc.
        (cond
          ;; Auxilliary files
          ((string-ends-with ".aux.html" basename)
           (aput "doc-type" "aux" doc)
           (setf doc (nconc doc (read-content (read-file src-path)))))
          ;; Comment files.
          ((string-ends-with ".cm.html" basename)
           (aput "doc-type" "cm" doc)
           (aput "doc-path" (doc-path src-path src-dir ".cm.html" ".html") doc)
           (setf doc (nconc doc (read-cm-doc (read-file src-path)))))
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

(defun read-includes (src-dir)
  "Read include files from given source directory."
  (loop for src-path in (uiop:directory-files src-dir)
        collect (cons (file-namestring src-path)
                      (read-file src-path))))

(defun read-layouts (src-dir)
  "Read layout files from given source directory."
  (loop for src-path in (uiop:directory-files src-dir)
        collect (cons (pathname-name src-path)
                      (read-content (read-file src-path)))))

(defun fill-layouts (layouts)
  "Render each layout within its parent layout (if any) recursively."
  (let ((results))
    (dolist (layout layouts)
      (let* ((layout-name (car layout))
             (layout-alist (cdr layout))
             (result (aget "body" layout-alist))
             (parent-name))
        (loop
          (unless (setf parent-name (aget "layout" layout-alist))
            (return))
          (setf layout-alist (aget parent-name layouts))
          (setf result (render (aget "body" layout-alist)
                               (list (cons "body" result)))))
        (push (cons layout-name result) results)))
    results))

(defun root-path (path)
  "Return relative path to web root from the given rendered file path."
  (let ((depth (count #\/ path)))
    (if (zerop depth) "./" (repeat-string depth "../"))))

(defun neat-path (path params)
  "Create canonical path component of the URL for the given rendered file path."
  (string-replace "index.html" (aget "index" params) path))

(defun neat-url (path params)
  (concatenate 'string (aget "site-url" params)
               (string-replace "index.html" "" path)))

(defun zone-link (doc zones params)
  (let* ((doc-path (aget "doc-path" doc))
         (lists (aget "lists" doc))
         (zone (find-if (lambda (z)
                          (or (string-starts-with (first z) doc-path)
                              (member (first z) lists :test #'string=))) zones)))
    (if zone
        (fstr "~%    <a href=\"~a\">~a</a>"
              (render (second zone) (append doc params)) (third zone)) "")))

(defun fill-paths (docs params)
  (let ((results))
    (dolist (doc docs)
      (let* ((doc-path (aget "doc-path" doc))
             (dst-path (render "{{ pub }}{{ doc-path }}" (append doc params)))
             (root (root-path doc-path)))
        (aput "dst-path" dst-path doc)
        (aput "root" root doc)
        (aput "neat-path" (neat-path doc-path params) doc)
        (aput "neat-url" (neat-url doc-path params) doc))
      (push doc results))
    results))

(defun fill-imports (docs includes params)
  (let ((results))
    (dolist (doc docs)
      (aput "heads" (head-html (aget "import" params)
                               (aget "import" doc)
                               (aget "root" doc) includes) doc)
      (push doc results))
    results))

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
                     (error "Incorrect heading level ~a after ~a: ~a (~a)"
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

(defun find-cmo (doc-path cmo)
  (cdr (find-if (lambda (x) (string-starts-with (car x) doc-path)) cmo)))

(defun fill-render (docs zones params)
  "Fill parameters in renderable documents."
  (let ((results))
    (dolist (doc docs)
      (aput "draft-mark" (if (aget "draft" doc) " [draft]" "") doc)
      (aput "zone-link" (zone-link doc zones params) doc)
      (let* ((date (aget "date" doc))
             (lists (string-split (aget "list" doc) ", "))
             (tags (string-split (aget "tag" doc) ", "))
             (toc (aget "toc" doc))
             (update (aget "update" doc)))
        (aput "lists" lists doc)
        (aput "iso-date" (format-iso-date (parse-content-date date)) doc)
        (aput "short-date" (format-short-date (parse-content-date date)) doc)
        (aput "tags-for-page" (format-tags tags 2 (aget "root" doc)) doc)
        (aput "tags-for-list" (format-tags tags 4 "") doc)
        (aput "tags-for-feed" (format-tags tags 2 (aget "site-url" params)) doc)
        (aput "toc" (toc-html (aget "body" doc) (string= toc "num")) doc)
        (aput "update-mark" (if update (update-mark update) "") doc))
      (push doc results))
    results))

(defun render-body-docs (docs params)
  (dolist (doc docs)
    (write-log "Writing ~a ~a" (aget "doc-type" doc) (aget "dst-path" doc))
    (write-file (aget "dst-path" doc) (render (aget "body" doc) params))))

(defun render-layout-docs (docs layouts params)
  (dolist (doc docs)
    (let* ((layout (aget (aget "doc-type" doc) layouts))
           (body (render (aget "body" doc) (append doc params)))
           (body-param (list (cons "body" body)))
           (final (render layout (append body-param doc params))))
      (write-log "Writing ~a ~a" (aget "doc-type" doc) (aget "dst-path" doc))
      (write-file (aget "dst-path" doc) final))))

(defun select-docs (docs &rest types)
  "Select documents that match the given types."
  (remove-if-not (lambda (doc)
                   (member (aget "doc-type" doc) types :test #'string=)) docs))

(defun render-cm-blocks (blocks layouts params)
  (let ((results)
        (layout (aget "cm-item" layouts)))
    (dolist (blck blocks)
      (let ((result))
        (setf result (render (aget "body" blck) (append blck params)))
        (setf result (render layout (list (cons "body" result))))
        (push result results)))
    results))

(defun render-cm-docs (docs layouts params)
  (dolist (doc docs)
    (let* ((rendered-blocks (render-cm-blocks (aget "blocks" doc) layouts params))
           (body-param (list (cons "body" (join-strings rendered-blocks))))
           (layout (aget "cm-page" layouts))
           (final (render layout (append body-param doc params))))
      (write-log "Writing ~a ~a" (aget "doc-type" doc) (aget "dst-path" doc))
      (write-file (aget "dst-path" doc) final))))

(defun copy-docs (docs)
  (dolist (doc docs)
    (write-log "Copying ~a ~a" (aget "doc-type" doc) (aget "dst-path" doc))
    (copy-file (aget "src-path" doc) (aget "dst-path" doc))))


;;; Complete Website
;;; ----------------

(defvar *params* nil
  "Global parameters that may be provided externally to override any
  default local parameters.")

(defun make-doc-map (docs)
  "Create hashtable to map document slugs to document metadata."
  (let ((count-map (make-hash-table :test #'equal))
        (doc-map (make-hash-table :test #'equal)))
    (dolist (doc docs)
      (let ((slug (doc-slug doc)))
        (hset slug (1+ (or (hget slug count-map) 0)) count-map)))
    (dolist (doc docs)
      (let ((slug (doc-slug doc)))
        (when (= 1 (hget slug count-map))
          (hset slug
                (list (cons "import" (aget "import" doc)))
                doc-map))))
    doc-map))

;;; TODO: Duplicate onid not allowed.

(defun main ()
  "Generate website."
  (let* ((params (list (cons "index" "")
                       (cons "import" "main.css")
                       (cons "pub" "_site/")
                       (cons "year" (nth-value 5 (get-decoded-time)))
                       (cons "zone-link" "")))
         (params (nconc *params* (read-list "params.lisp") params))
         (config (read-list "config.lisp"))
         (layouts (fill-layouts (read-layouts "layout/")))
         (includes (read-includes "includes/"))
         (cmo (aget "cmo" config))
         (zones (aget "zones" config))
         (all-docs (fill-paths (read-docs "content/tree/") params))
         (ren-docs (select-docs all-docs "post" "page"))
         (doc-map (make-doc-map (select-docs all-docs "aux" "post" "page")))
         (cm-docs (select-docs all-docs "cm")))
    ;; Set up dependencies.
    (remove-directory (aget "pub" params))
    (copy-directory "_cache/katex/" (render "{{ pub }}js/katex/" params))
    ;; Fill documents.
    (setf ren-docs (fill-render ren-docs zones params))
    (setf cm-docs (fill-cm cm-docs doc-map cmo params))
    ;; Fill imports.
    (setf ren-docs (fill-imports ren-docs includes params))
    (setf cm-docs (fill-imports cm-docs includes params))
    (render-body-docs (select-docs all-docs "css") (aget "style" config))
    (render-layout-docs ren-docs layouts params)
    (render-cm-docs cm-docs layouts params)
    (copy-docs (select-docs all-docs "raw"))))

(when *site-mode*
  (main))
