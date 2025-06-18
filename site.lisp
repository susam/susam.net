;;;; Site Generator
;;;; ==============

;;;; Copyright (c) 2021-2025 Susam Pal
;;;;
;;;; You can use, copy, modify, merge, publish, distribute,
;;;; sublicense, and/or sell copies of it, under the terms of the MIT
;;;; License. See COPYRIGHT.md for complete details.
;;;;
;;;; This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; express or implied. See COPYRIGHT.md for complete details.

;;;; This site generator is inspired by and based on my lovely wife's
;;;; <https://github.com/sunainapai/makesite/>.

(require "uiop")


;;; Special Modes
;;; -------------

(defvar *log-mode* t
  "Write logs iff true.")

(defvar *main-mode* t
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
      (make-directory destpath)
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

(defun string-split (string separator)
  "Split a string into a list of strings using the given separator."
  (let ((next-index 0)
        (match-index)
        (result))
    (loop
      (setf match-index (search separator string :start2 next-index))
      (when (or (not match-index) (= match-index (length string)))
        (return))
      (setf match-index (max (1+ next-index) match-index))
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

(defun indent-lines (count string)
  "Indent lines by count spaces."
  (let* ((clean-string (string-right-trim '(#\Newline) string))
         (lines (string-split clean-string (fstr "~%")))
         (indent (repeat-string count " ")))
    (join-strings (loop for line in lines
                        collect (if (zerop (length line))
                                    (fstr "~%")
                                    (fstr "~a~a~%" indent line))))))

(defmacro aput (key value alist)
  "Add key-value pair to alist."
  `(push (cons ,key ,value) ,alist))

(defmacro aput-list (key value alist)
  "Add value to a list corresponding to the key in alist."
  `(progn
     (unless (assoc ,key ,alist :test #'string=)
       (push (cons ,key ()) ,alist))
     (push ,value (cdr (assoc ,key ,alist :test #'string=)))))

(defun aget (key alist)
  "Given a key, return its value found in the list of parameters."
  (cdr (assoc key alist :test #'string=)))

(defun last-n (n sequence)
  "Return at most the last n elements of a sequence as a new sequence."
  (subseq sequence (max 0 (- (length sequence) n))))

(defmacro extend-list (old-list new-list)
  "Append new-list to old-list and return the resulting list."
  `(setf ,old-list (append ,old-list ,new-list)))

(defun parse-tz (tz-string)
  "Parse time zone string like to CL-friendly rational, e.g., +0530 as -9/2."
  (let* ((sign (if (char= (char tz-string 0) #\-) 1 -1))
         (hours (parse-integer tz-string :start 1 :end 3))
         (minutes (parse-integer tz-string :start 3)))
    (* sign (+ hours (/ minutes 60)))))

(defun format-tz (tz)
  "Format tz (rational) into a +HHMM format time zone string."
  (let* ((minutes (* -60 tz))
         (sign (if (minusp minutes) "-" "+"))
         (abs-minutes (abs minutes))
         (hh (truncate abs-minutes 60))
         (mm (mod abs-minutes 60)))
    (format nil "~a~2,'0d~2,'0d" sign hh mm)))



;;; Tool Definitions
;;; ----------------

(defun read-header-line (text next-index)
  "Parse one line of header in text and return multiple values: key,
value, next-index."
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

(defun weekday-name (weekday-index)
  "Given an index, return the corresponding day of week."
  (nth weekday-index '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

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

(defun format-rss-date (universal-time)
  "Convert universal-time (integer) to RFC-2822 date string."
  (multiple-value-bind (second minute hour date month year day dst tz)
      (decode-universal-time universal-time 0)
    (declare (ignore dst))
    (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d ~a"
            (weekday-name day) date (month-name month) year hour minute second (format-tz tz))))

(defun format-short-date (universal-time)
  "Convert universal-time (integer) to a simple human-readable date."
  (multiple-value-bind (second minute hour date month year day dst tz)
      (decode-universal-time universal-time 0)
    (declare (ignore second minute hour day dst tz))
    (format nil "~2,'0d ~a ~4,'0d" date (month-name month) year)))

(defun format-long-date (universal-time)
  "Convert universal-time (integer) to a simple human-readable date."
  (multiple-value-bind (second minute hour date month year day dst tz)
      (decode-universal-time universal-time 0)
    (declare (ignore second day dst tz))
    (format nil "~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d ~a"
            date (month-name month) year hour minute "GMT")))

(defun date-slug (filename)
  "Parse filename to extract date and slug."
  (let* ((basename (file-namestring filename))
         (dot-index (search "." basename))
         (slug (subseq basename 0 dot-index))
         (date))
    (when (and (>= (length basename) 11)
               (every #'digit-char-p (loop for i in '(0 1 2 3 5 6 8 9)
                                           collect (char basename i))))
      (setf date (subseq basename 0 10))
      (setf slug (subseq basename 11 dot-index)))
    (values date slug)))

(defun render (template params)
  "Render parameter tokens in template with their values from params."
  (with-output-to-string (s)
    (let* ((start-token "{{ ")
           (end-token " }}")
           (next-index 0)     ; Next place to start searching "{{".
           (start-index)      ; Starting of "{{ ".
           (end-index))       ; Starting of " }}".
      (loop
        ;; Look for start-token and extract static text before it.
        (setf start-index (search start-token template :start2 next-index))
        (unless start-index
          (return))
        (format s "~a" (subseq template next-index start-index))
        ;; Extract parameter name between start-token and end-token.
        (incf start-index (length start-token))
        (setf end-index (search end-token template :start2 start-index))
        (let* ((key (subseq template start-index end-index))
               (val (aget key params)))
          ;; If key exists in params, replace key with value.
          ;; Otherwise, leave the key intact in text.
          (if val
              (format s "~a" val)
              (format s "{{ ~a }}" key)))
        (setf next-index (+ end-index (length end-token))))
      ;; Extract static text after the last parameter token.
      (format s "~a" (subseq template next-index)))))

(defmacro set-nested-template (template outer-template)
  "Set template to outer-template rendered with inner-template."
  `(setf ,template (render ,outer-template (list (cons "body" ,template)))))

(defun head-css-html (name root)
  "Create HTML tag for CSS."
  (fstr "  <link rel=\"stylesheet\" href=\"~acss/~a\">~%" root name))

(defun head-js-html (name root)
  "Create HTML tag for JavaScript."
  (fstr "  <script src=\"~ajs/~a\"></script>~%" root name))

(defun head-inc-html (name params)
  "Render include file for inclusion in a page."
  (render (read-file (fstr "layout/include/~a" name)) params))

(defun head-html (import-header params)
  "Given the value of an import header, return HTML code for it."
  (let ((names (string-split import-header ", "))
        (root (aget "root" params))
        (snippets))
    (dolist (name names)
      (cond ((string-ends-with ".css" name)
             (push (head-css-html name root) snippets))
            ((string-ends-with ".js" name)
             (push (head-js-html name root) snippets))
            ((string-ends-with ".inc" name)
             (push (head-inc-html name params) snippets))
            (t
             (error "Unknown import type ~a in ~a" name import-header))))
    (if snippets (fstr "~{~a~}" (reverse snippets)) "")))

(defun relative-root-path (path)
  "Return relative path to web root from the given rendered file path."
  (let ((depth (count #\/ (string-replace "_site/" "" path))))
    (if (zerop depth) "./" (repeat-string depth "../"))))

(defun neat-path (path params)
  "Create canonical path component of the URL for the given rendered file path."
  (setf path (string-replace "index.html" (aget "index" params) path))
  (string-replace "_site/" "" path))

(defun neat-url (path params)
  "Create canonical URL for the given rendered file path."
  (push (cons "index" "") params)
  (fstr "~a~a" (aget "site-url" params) (neat-path path params)))

(defmacro add-zone-params (dst-path params)
  `(let ((blog-name (aget "blog-name" ,params))
         (root (relative-root-path ,dst-path))
         (zone-link "")
         (zone-name)
         (zone-index))
     (cond ((string-starts-with "_site/cc/" ,dst-path)
            (setf zone-index (render "cc/{{ index }}" ,params))
            (setf zone-name "Club"))
           ((string/= blog-name "Blog")
            (setf zone-index (fstr "~a.html" (string-downcase blog-name)))
            (setf zone-name blog-name)))
     (when (and zone-index zone-name)
       (setf zone-link (fstr "~%    <a href=\"~a~a\">~a</a>"
                             root zone-index zone-name)))
     (aput "zone-link" zone-link ,params)
     (when (string= blog-name "Maze")
       (let ((zone-title (render "{{ nick }}'s Maze" ,params)))
         (if (string= ,dst-path (render "_site/{{ blog-slug }}.html" ,params))
             (aput "title" zone-title ,params)
             (aput "subtitle" (fstr " - ~a" zone-title) ,params))))))

(defmacro add-output-params (dst-path params)
  "Given an output file path, set a canonical URL for that file."
  `(progn
     (aput "root" (relative-root-path ,dst-path) ,params)
     (aput "heads" (head-html (aget "head" ,params) ,params) ,params)
     (aput "imports" (head-html (aget "import" ,params) ,params) ,params)))

(defmacro add-page-params (dst page params)
  `(let* ((all-params (append ,page ,params))
          (dst-path (render ,dst all-params))
          (root (relative-root-path dst-path)))
     (aput "tags-for-page" (format-tags-for-page ,page root) ,page)
     (aput "tags-for-list" (format-tags-for-list ,page) ,page)
     (aput "tags-for-feed" (format-tags-for-feed ,page ,params) ,page)
     (aput "keys-for-list" (format-keys-for-list ,page) ,page)
     (aput "neat-url" (neat-url dst-path ,params) ,page)
     (aput "neat-path" (neat-path dst-path ,params) ,page)))

(defmacro invoke-callbacks (params)
  "Run callbacks and add the parameters returned by it to params."
  `(dolist (callback (aget "callbacks" ,params))
     (setf ,params (append (funcall callback ,params) ,params))))

(defun extra-markup (text)
  "Add extra markup to the page to create heading anchor links."
  (with-output-to-string (ss)
    (let* ((h-begin-index)              ; ->  <h1 id="foo">
           (h-close-index)              ; ->  </h1>
           (id-end-index)               ;     <h1 id="foo"   <-
           (next-index 0))              ;     <h1            <-
      (loop
       (setf h-begin-index (search "<h" text :start2 next-index))
       (unless h-begin-index
         (return))
       (cond ((and (digit-char-p (char text (+ h-begin-index 2)))
                   (substring-at "id=\"" text (+ h-begin-index 4)))
              (setf id-end-index (search "\"" text :start2 (+ h-begin-index 8)))
              (setf h-close-index (search "</h" text :start2 (+ id-end-index 2)))
              (format ss "~a<a href=\"#~a\"></a></h"
                      (subseq text next-index h-close-index)
                      (subseq text (+ h-begin-index 8) id-end-index))
              (setf next-index (+ h-close-index 3)))
             (t
              (format ss "~a" (subseq text next-index (+ h-begin-index 2)))
              (setf next-index (+ h-begin-index 2)))))
      (format ss "~a" (subseq text next-index)))))

(defun latex-markup (text)
  "Reformat LaTeX code to make the punctuation stick to formulas."
  (setf text (string-replace " \\)."
                             ".  \\)"
                             text))
  (setf text (string-replace " \\)?"
                             "?  \\)"
                             text))
  (setf text (string-replace " \\),"
                             ", \\)"
                             text))
  (setf text (string-replace " \\);"
                             ";  \\)"
                             text)))

(defun format-markup (text)
  "Perform final formatting to pages before rendering them."
  (setf text (extra-markup text))
  (setf text (latex-markup text)))

(defun toc-indent (level)
  "Create leading indentation items in table of contents."
  (repeat-string (* 2 level) " "))

(defun toc-html (text)
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
              (cond ((string= href "#contents"))
                    ((> new-level (1+ old-level))
                     (error "Incorrect heading level ~a after ~a: ~a (~a)"
                            new-level old-level href title))
                    ((= new-level (1+ old-level))
                     (format tt (if init "~%" ""))
                     (format tt "~a<ul>~%" (toc-indent (incf indent)))
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
                       (format tt "~a</ul>~%" (toc-indent (decf indent)))
                       (format tt "~a</li>~%" (toc-indent (decf indent))))
                     (format tt "~a<li><a href=\"~a\">~a</a>"
                             (toc-indent indent) href title)))
              (setf next-index (+ h-close-index 3)))
             (t
              (setf next-index (+ h-begin-index 2)))))
      (when init
        (format tt "</li>~%")
        (dotimes (n (floor indent 2))
          (format tt "~a</ul>~%" (toc-indent (decf indent)))
          (format tt "~a</li>~%" (toc-indent (decf indent))))
        (format tt "</ul>")))))

(defun format-size (size)
  "Convert size in bytes to human-readable size."
  (let ((powers (list (cons (expt 2 30) "G")
                      (cons (expt 2 20) "M")
                      (cons (expt 2 10) "K")
                      (cons (expt 2  0) "B")))
        (chosen-power)
        (chosen-suffix))
    (dolist (entry powers)
      (setf chosen-power (car entry))
      (setf chosen-suffix (cdr entry))
      (when (<= chosen-power size)
        (return)))
    (fstr "~a&nbsp;~a" (round (/ size chosen-power)) chosen-suffix)))

(defun tag-slug (tag)
  "Convert tag title to tag slug."
  (string-replace " " "-" (string-downcase tag)))

(defun format-tags-for-html (page indent root)
  "Create HTML to display tags."
  (let ((html "")
        (sep ""))
    (dolist (tag (aget "tags" page))
      (setf tag (tag-slug tag))
      (setf html (fstr "~a~a<a href=\"~atag/~a.html\">#~a</a>" html sep root tag tag))
      (setf sep (fstr " |~%~a" (repeat-string indent " "))))
    html))

(defun format-keys-for-html (page indent)
  "Create HTML to display keys."
  (let ((html "")
        (sep ""))
    (dolist (key (aget "keys" page))
      (setf html (fstr "~a~a<a href=\"?~a\">~a</a>" html sep key key))
      (setf sep (fstr " |~%~a" (repeat-string indent " "))))
    html))

(defun format-tags-for-page (page root)
  "Create HTML to display tags on a page."
  (format-tags-for-html page 2 root))

(defun format-tags-for-list (page)
  "Create HTML to display tags on the full page list."
  (format-tags-for-html page 4 ""))

(defun format-keys-for-list (page)
  "Create HTML to display keys on the short link list."
  (format-keys-for-html page 4))

(defun format-tags-for-feed (page params)
  "Create HTML to display tags for the given page."
  (let ((html "")
        (sep "")
        (site-url (aget "site-url" params))
        (tags (aget "tags" page)))
    (dolist (tag tags)
      (setf tag (tag-slug tag))
      (setf html (fstr "~a~a<a href=\"~atag/~a.html\">#~a</a>"
                       html sep site-url tag tag))
      (setf sep (fstr " |~%  ")))
    html))

(defun write-page (dst-path layout params)
  "Render given layout with given parameters and write page."
  (setf dst-path (render dst-path params))
  ;; Standalone pages (e.g., ls.html, comment pages, guestbook, etc.)
  ;; need neat-url to be set for rendering the canonical link tag.
  ;; This function happens to be one common place where the neat-url
  ;; parameter can be added to all such pages.
  (add-page-params dst-path params params)
  (write-log "Writing ~a ..." dst-path)
  (add-output-params dst-path params)
  (add-zone-params dst-path params)
  ;; Perform format-markup only for .html pages.
  (write-file dst-path (format-markup (render layout params))))


;;; Pages
;;; -----

(defun read-page (filename)
  "Parse content file."
  (let ((text (read-file filename))
        (page)
        (date)
        (draft))
    (multiple-value-bind (date slug) (date-slug filename)
      (aput "date" date page)
      (aput "slug" slug page))
    (multiple-value-bind (headers next-index) (read-headers text 0)
      (setf page (append headers page))
      (aput "body" (subseq text next-index) page))
    ;; Date.
    (setf date (aget "date" page))
    (when (aget "date" page)
      (aput "rss-date" (format-rss-date (parse-content-date date)) page)
      (aput "simple-date" (format-short-date (parse-content-date date)) page))
    ;; Updated date.
    (aput "update-mark" "" page)
    (let ((updated (aget "updated" page)))
      (when updated
        (aput "update-mark" (fstr " (updated on ~a)"
                                  (format-short-date (parse-content-date updated)))
              page)))
    ;; Other metadata to be parsed.
    (aput "keys" (string-split (aget "key" page) ", ") page)
    (aput "tags" (string-split (aget "tag" page) ", ") page)
    (aput "pkey" (car (aget "keys" page)) page)
    ;; Draft status.
    (setf draft (aget "draft" page))
    (aput "draft-mark" (if draft " [draft]" "") page)
    page))

(defun make-page (src-path dst layout params)
  "Generate page from content file."
  (let* ((page (read-page src-path))
         (body))
    ;; Set neat-url and tags-for-feed so that the returned page alist
    ;; can be used to render feeds.
    (add-page-params dst page params)
    ;; Read content and merge its parameters with call parameters.
    (setf params (append page params))
    ;; Run callbacks.
    (invoke-callbacks params)
    ;; Render placeholders in page body if requested.
    (when (string= (aget "render" params) "yes")
      (aput "toc" (toc-html (aget "body" params)) params)
      (setf body (render (aget "body" params) params))
      (aput "body" body params)
      ;; Update body in page to the rendered body.
      (aput "body" body page))
    (write-page dst layout params)
    page))

(defun copy-page (src-path dst-path params)
  "Copy an HTML page to destination path."
  (uiop:copy-file src-path dst-path)
  (let* ((meta-path (string-replace ".html" ".aux.html" (namestring src-path)))
         (page (read-page meta-path)))
    ;; Set neat-url and tags-for-feed so that the returned page alist
    ;; can be used to render feeds.
    (add-page-params dst-path page params)
    page))

(defun sort-by-date-desc (items)
  "Sort items in reverse chronological order."
  (sort (copy-list items) (lambda (x y) (string> (aget "date" x)
                                                 (aget "date" y)))))

(defun sort-by-date (items)
  "Sort items in chronological order."
  (sort (copy-list items) (lambda (x y) (string< (aget "date" x)
                                                 (aget "date" y)))))

(defun make-pages (src dst layout params)
  "Generate pages from content files."
  (let ((pages))
    (dolist (src-path (directory src))
      (push (make-page src-path dst layout params) pages))
    pages))

(defun only-listed-items (items)
  "Select items that are allowed to be listed on pages."
  (remove-if (lambda (item) (string= (aget "unlist" item) "yes")) items))

(defun only-feeding-items (items)
  "Select items that are allowed to be listed on web feeds."
  (remove-if (lambda (item) (or (string= (aget "unfeed" item) "yes")
                                (string= (aget "unlist" item) "yes")
                                (string= (aget "draft" item) "yes"))) items))

(defun make-page-list (pages dst list-layout item-layout params)
  "Generate list page for content pages that are allowed to be listed."
  (setf pages (sort-by-date (only-listed-items pages)))
  (let ((count (length pages))
        (rendered-pages))
    ;; Render each page.
    (dolist (page pages)
      (setf page (append page params))
      (invoke-callbacks page)
      (push (render item-layout page) rendered-pages))
    ;; Add list parameters.
    (aput "body" (join-strings rendered-pages) params)
    (aput "count" count params)
    (aput "page-label" (if (= count 1) "page" "pages") params)
    ;; Determine destination path and URL.
    (write-page dst list-layout params)))

(defun make-feed-list (pages limit dst list-layout item-layout params)
  "Generate feed list for pages that are not draft and allowed to be listed."
  (make-page-list (last-n limit (sort-by-date (only-feeding-items pages)))
                  dst list-layout item-layout params))


;;; Comments
;;; --------

(defun read-comment (text start-index)
  "Read a single comment from a comment file."
  (let ((start-token "<!-- ") ; Header prefix.
        (date)                ; Date.
        (commenter)           ; Rendered commenter display name.
        (url)                 ; URL of commenter.
        (comment)             ; Parsed comment parameters.
        (next-index))         ; Index at which to search next comment.
    (setf (values comment start-index) (read-headers text start-index))
    ;; Determine commenter's display name.
    (setf commenter (aget "name" comment))
    (setf url (aget "url" comment))
    (when url
      (setf commenter (fstr "<a href=\"~a\">~a</a>" url commenter)))
    (aput "commenter" commenter comment)
    ;; Formatted dates.
    (setf date (aget "date" comment))
    (aput "rss-date" (format-rss-date (parse-content-date date)) comment)
    (aput "simple-date" (format-long-date (parse-content-date date)) comment)
    ;; Select content until next header or end-of-text as body.
    (setf next-index (search start-token text :start2 start-index))
    (aput "body" (subseq text start-index next-index) comment)
    (values comment next-index)))

(defun read-comments (filename)
  "Read all comments from a comment file."
  (let ((text (read-file filename))
        (next-index 0)
        (slug (nth-value 1 (date-slug filename)))
        (serial 0)
        (comment)
        (comments))
    (loop
      (setf (values comment next-index) (read-comment text next-index))
      ;; Current comment date must be more recent than the previous comment.
      (let ((date (aget "date" comment)))
        (when (and (consp comments) (string< date (aget "date" (car comments))))
          (error "Incorrect order for comment ~a in ~a" date filename))
        ;; Ensure time zone is specified in comment date.
        (unless (and (= (length date) 25) (string-ends-with " +0000" date))
          (error "Time zone missing in comment date ~a in ~a" date filename)))
      (aput "slug" slug comment)
      (aput "comment-file-serial" (incf serial) comment)
      (push comment comments)
      (unless next-index
        (return)))
    (values (reverse comments) slug)))

(defun collect-comments (src-patterns)
  "Collect comments using the given glob patterns."
  (let ((comments)
        (seen-slugs (make-hash-table :test #'equal)))
    ;; Iterate over each source pattern in source pattern list.
    (dolist (src-pattern src-patterns)
      ;; Iterate over each source path matched by the source pattern.
      (dolist (src-path (directory src-pattern))
        ;; Read comments and their slug from the source path.
        (multiple-value-bind (src-comments slug) (read-comments src-path)
          ;; Ensure new slug does not conflict with an existing slug.
          (when (gethash slug seen-slugs)
            (error "Duplicate slug ~a for comment file ~a" slug src-path))
          (setf (gethash slug seen-slugs) t)
          (setf comments (append comments src-comments)))))
    comments))

(defun make-comment-list (comments dst list-layout item-layout params)
  "Generate comment list page. Honour the order of comments provided."
  (let* ((post-import (aget "import" params))
         (comment-dst (render dst params))
         (comment-count (length comments))
         (comment-label (if (= comment-count 1) "comment" "comments"))
         (common-params)
         (rendered-comments))
    (aput "comment-count" comment-count common-params)
    (aput "comment-label" comment-label common-params)
    (dolist (comment comments)
      (push (render item-layout (append comment common-params)) rendered-comments))
    ;; Inherit imports from post.
    (if post-import
        (setf post-import (fstr "comment.css, ~a" post-import))
        (setf post-import "comment.css"))
    (aput "import" post-import params)
    ;; Determine destination path and URL.
    (aput "body" (join-strings (reverse rendered-comments)) params)
    (write-page comment-dst list-layout params)))

(defun make-comment-none (dst none-layout params)
  "Generate a comment page with no comments."
  (aput "import" "" params)             ; Empty list needs no imports.
  (write-page dst none-layout params))

(defun comments-by-slug (comments slug)
  "Filter comments by slug and return comments with matching slug."
  (remove-if-not (lambda (c) (string= (aget "slug" c) slug)) comments))


(defun enrich-comments (comments page dst-path params)
  "Enrich a comment by adding relevant page metadata to it."
  (let ((enriched-comments))
    (dolist (comment comments)
      (aput "unlist" (aget "unlist" page) comment)
      (aput "commented-page-title" (aget "title" page) comment)
      (aput "commented-page-path" (aget "neat-path" page) comment)
      (aput "comment-page-path" (neat-path dst-path params) comment)
      (aput "comment-class" (if (string= (aget "name" comment)
                                         (aget "author" params))
                                "author" "visitor") comment)
      (push comment enriched-comments))
    (reverse enriched-comments)))

(defun number-comments (comments)
  "Sort comments by date and number them."
  (setf comments (sort-by-date comments))
  (loop for comment in comments
        for serial from 1 to (length comments)
        do (aput "comment-list-serial" serial comment)
        collect comment))

(defun make-page-comments (pages all-comments name page-layout params)
  "Generate comment list pages or no comments pages for all pages."
  (let ((none-layout (read-file "layout/comment/none.html"))
        (list-layout (read-file "layout/comment/list.html"))
        (item-layout (read-file "layout/comment/item.html"))
        (comment-dst "_site/comments/{{ slug }}.html")
        (enriched-comments))
    ;; Combine layouts to form final layouts.
    (set-nested-template none-layout page-layout)
    (set-nested-template list-layout page-layout)
    ;; All parent pages belong to the same blog.
    (aput "blog-name" name params)
    ;; For each page, render its comment list page.
    (dolist (page pages)
      (let* ((page-comments (comments-by-slug all-comments (aget "slug" page)))
             (dst-path (render comment-dst page))
             (comment-params (append params page)))
        (aput "title" (fstr "Comments on ~a" (aget "title" page)) comment-params)
        (aput "post-title" (aget "title" page) comment-params)
        (setf page-comments (enrich-comments page-comments page dst-path params))
        (extend-list enriched-comments page-comments)
        (if page-comments
            (make-comment-list page-comments comment-dst list-layout item-layout comment-params)
            (make-comment-none comment-dst none-layout comment-params))))
    enriched-comments))

(defun make-guestbook (comments page-layout params)
  "Create guestbook page."
  (let ((comments (comments-by-slug comments "guestbook"))
        (list-layout (read-file "layout/guestbook/list.html"))
        (item-layout (read-file "layout/comment/item.html"))
        (page (read-page "content/guestbook/guestbook.aux.html"))
        (dst-path "_site/guestbook.html"))
    (aput "title" "Guestbook" params)
    (aput "post-title" "Guestbook" params)
    (set-nested-template list-layout page-layout)
    (add-page-params dst-path page params)
    (setf comments (enrich-comments comments page dst-path params))
    (make-comment-list comments dst-path list-layout item-layout params)
    (values (list page) comments)))


;;; Tree
;;; ----

(defun validate-required-params (pages)
  "Validate pages to ensure required parameters exist."
  (dolist (page pages)
    (dolist (key '("date" "tag" "key"))
      (when (not (aget key page))
        (error "Missing key ~a for page ~a" key (aget "slug" page))))))

(defun validate-unique-keys (pages)
  (let ((seen-keys (make-hash-table :test #'equal)))
    (dolist (page pages)
      (dolist (key (aget "keys" page))
        (when (gethash key seen-keys)
          (error "Duplicate key ~a in page ~a" key (aget "slug" page)))
        (setf (gethash key seen-keys) t)))))

(defun make-tree-recursively (src dst page-layout post-layout params)
  "Recursively descend into a directory tree and render/copy all files."
  (make-directory dst)
  (let ((pages))
    (dolist (pathname (uiop:directory-files src))
      (let* ((basename (file-namestring pathname))
             (destpath (namestring (merge-pathnames basename dst))))
        (cond ((string-ends-with ".page.html" basename)
               (setf destpath (string-replace ".page.html" ".html" destpath))
               (push (make-page pathname destpath page-layout params) pages))
              ((string-ends-with ".post.html" basename)
               (setf destpath (string-replace ".post.html" ".html" destpath))
               (push (make-page pathname destpath post-layout params) pages))
              ((string-ends-with ".aux.html" basename)
               (write-log "Skipping ~a" basename))
              ((string-ends-with ".html" basename)
               (push (copy-page pathname destpath params) pages))
              (t
               (uiop:copy-file pathname destpath)))))
    (dolist (pathname (uiop:subdirectories src))
      (let* ((basename (directory-basename pathname))
             (destpath (merge-pathnames basename dst)))
        (make-directory destpath)
        (setf pages (append pages (make-tree-recursively pathname destpath
                                                         page-layout post-layout
                                                         params)))))
    pages))

(defun make-tree (src dst page-layout params)
  "Make tree of files and folders from the content tree."
  (let ((post-layout (read-file "layout/tree/post.html")))
    (set-nested-template post-layout page-layout)
    (make-tree-recursively src dst page-layout post-layout params)))


;;; Directory Listing
;;; -----------------

(defun make-directory-index (current-pathname paths-and-sizes
                             dst-filenames page-layout params)
  "Render index pages for the given current directory."
  (let* ((list-layout (read-file "layout/index/list.html"))
         (item-layout (read-file "layout/index/item.html"))
         (fs-path)
         (rendered-rows))
    (set-nested-template list-layout page-layout)
    (setf paths-and-sizes (sort paths-and-sizes
                                (lambda (x y) (string> (car x) (car y)))))
    (dolist (entry paths-and-sizes)
      (let ((item-params params))
        (setf fs-path (car entry))
        (aput "fs-path" fs-path item-params)
        (aput "size" (cdr entry) item-params)
        (when (string-ends-with "/" fs-path)
          (setf fs-path (fstr "~aindex.html" fs-path)))
        (if (string= fs-path "index.html")
            (aput "neat-path" fs-path item-params)
            (aput "neat-path" (neat-path fs-path params) item-params))
        (push (render item-layout item-params) rendered-rows)))
    (aput "rows" (join-strings rendered-rows) params)
    (dolist (dst-path dst-filenames)
      (setf dst-path (enough-namestring (merge-pathnames dst-path current-pathname)))
      (unless (probe-file dst-path)
        (write-page dst-path list-layout params)))))

(defun visit-directory (apex-pathname current-pathname dst-filenames title
                        page-layout params max-render-depth)
  "Make index pages for the given directory and its subdirectories recursively."
  (let ((url-path (enough-namestring current-pathname apex-pathname))
        (total-size 0)
        (paths-and-sizes)
        (size))
    ;; Collect subdirectories.
    (dolist (path (uiop:subdirectories current-pathname))
      (setf size (visit-directory apex-pathname path dst-filenames title
                                  page-layout params (1- max-render-depth)))
      (push (cons (directory-basename path) (format-size size)) paths-and-sizes)
      (incf total-size size))
    ;; Collect files.
    (dolist (path (uiop:directory-files current-pathname))
      (setf size (with-open-file (stream path) (file-length stream)))
      (push (cons (file-namestring path) (format-size size)) paths-and-sizes)
      (incf total-size size))
    ;; Render tree.
    (unless (equal apex-pathname current-pathname)
      (push (cons "../" "-") paths-and-sizes))
    (push (cons "./" (format-size total-size)) paths-and-sizes)
    (aput "url-path" (if (string= url-path "") "/" url-path) params)
    (aput "title" (render title params) params)
    (when (plusp max-render-depth)
      (make-directory-index current-pathname paths-and-sizes dst-filenames
                            page-layout params))
    ;; Return total size of current directory tree to caller.
    total-size))

(defun make-directory-lists (path page-layout &optional params)
  "Make index pages for each site directory and subdirectories recursively."
  (visit-directory (truename "_site/") (truename path)
                   '("index.html" "ls.html") "Index of {{ url-path }}"
                   page-layout params 100))

(defun collect-tree-paths (apex-pathname current-pathname page-layout params)
  "Collect paths from the given directory and its subdirectories recursively."
  (let ((paths))
    ;; Collect subdirectories.
    (dolist (path (uiop:subdirectories current-pathname))
      (push (enough-namestring path apex-pathname) paths)
      (setf paths (append (collect-tree-paths apex-pathname path
                                              page-layout params) paths)))
    ;; Collect files.
    (dolist (path (uiop:directory-files current-pathname))
      (push (enough-namestring path apex-pathname) paths))
    paths))

(defun render-tree-paths (paths dst-path title page-layout params)
  "Render the given list of paths into a page with a flat HTML list."
  (let* ((list-layout (read-file "layout/tree/list.html"))
         (item-layout (read-file "layout/tree/item.html"))
         (rendered-items))
    (set-nested-template list-layout page-layout)
    ;; Directory path (e.g., cc/) and index path (e.g., cc/index.html)
    ;; are duplicates.  Therefore, do not list index path.
    (setf paths (remove-if (lambda (path) (string-ends-with "/index.html" path))
                           paths))
    (dolist (fs-path (sort paths #'string>))
      (let ((item-params params))
        (aput "fs-path" fs-path item-params)
        (when (string-ends-with "/" fs-path)
          (setf fs-path (fstr "~aindex.html" fs-path)))
        (aput "neat-path" (neat-path fs-path params) item-params)
        (push (render item-layout item-params) rendered-items)))
    (aput "title" title params)
    (aput "items" (join-strings rendered-items) params)
    (write-page dst-path list-layout params)))

(defun make-tree-list (path title page-layout params)
  "Generate a flat tree listing of the given directory."
  (let ((paths (collect-tree-paths (truename path) (truename path)
                                   page-layout params))
        (dst-path (namestring (merge-pathnames "TREE.html" path))))
    (render-tree-paths paths dst-path title page-layout params)))


;;; Blog
;;; ----

(defun make-posts (src page-dst list-dst page-layout params)
  "Generate blog post pages for all posts in a blog directory."
  (let ((post-layout (read-file "layout/blog/post.html"))
        (list-layout (read-file "layout/blog/list.html"))
        (item-layout (read-file "layout/blog/item.html"))
        (list-override (render "layout/{{ blog-slug }}/list.html" params))
        (pages))
    ;; Look for overriding layouts.
    (when (probe-file list-override)
      (write-log "Using list layout override ~a ..." list-override)
      (setf list-layout (read-file list-override)))
    ;; Combine layouts to form final layouts.
    (set-nested-template post-layout page-layout)
    (set-nested-template list-layout page-layout)
    ;; Read and render all pages.
    (setf pages (make-pages src page-dst post-layout params))
    ;; Create blog list page.
    (aput "title" (render "{{ nick }}'s {{ blog-name }}" params) params)
    (aput "subtitle" "" params)
    (make-page-list pages list-dst list-layout item-layout params)
    pages))

(defun make-blog (src name page-layout params)
  "Create a complete top-level blog with blog, tags, and list page."
  (aput "blog-name" name params)
  (aput "blog-slug" (string-downcase name) params)
  (let* ((page-dst "_site/{{ slug }}.html")
         (list-dst "_site/{{ blog-slug }}.html")
         (pages))
    (setf pages (make-posts src page-dst list-dst page-layout params))
    pages))


;;; Meets
;;; -----

(defun format-meet-date (date)
  "Format meeting entry date for display in meeting list page."
  (string-replace " " "&nbsp;"
                  (subseq (format-rss-date (parse-content-date date)) 0 22)))

(defun future-p (meet)
  "Whether the given meeting entry is scheduled for future."
  (minusp (getf meet :members)))

(defun find-meet-track (slug slugs)
  (second (first (remove-if-not (lambda (x) (string= (first x) slug)) slugs))))

(defun meet-row-html (meet previous-meet slugs row-id layout params)
  "Create HTML to represent a single row of meeting entry."
  (let ((upcoming-attr (if (and previous-meet (not (future-p previous-meet))
                                (future-p meet)) " id=\"upcoming\"" ""))
        (topic (fstr "~a: ~a" (find-meet-track (getf meet :slug) slugs)
                     (getf meet :topic))))
    (aput "row-id" row-id params)
    (aput "upcoming" upcoming-attr params)
    (aput "class" (if (future-p meet) "future" "past") params)
    (aput "date" (format-meet-date (getf meet :date)) params)
    (aput "duration" (getf meet :duration) params)
    (aput "members" (if (future-p meet) "-" (getf meet :members)) params)
    (aput "topic" topic params)
    (render layout params)))

(defun meet-rows-html (meets slugs layout params)
  "Create HTML to represent all rows of meeting entries"
  (let ((previous-meet nil))
    (join-strings
     (loop for row-id from 1 to (length meets)
           for meet in meets
           collect (meet-row-html meet previous-meet slugs row-id layout params)
           do (setf previous-meet meet)))))

(defun meet-log-path (current-slug other-slug)
  "Return relative path to meeting track log page."
  (let* ((prefix (if current-slug "../" ""))
         (middle (if other-slug (fstr "~a/" other-slug) "")))
    (fstr "~a~alog.html" prefix middle)))

(defun meet-tracks-html (current-slug slugs)
  "Create HTML to list all meeting tracks."
  (join-strings (loop for (slug track) in (reverse slugs)
                      collect (fstr "<li><a href=\"~a\">~a</a></li>~%"
                                    (meet-log-path current-slug slug)
                                    (if track track "All Meetings")))))

(defun select-meets (slug meets)
  "Filter meets to select the ones with the specified slug."
  (remove-if-not (lambda (x) (string= (getf x :slug) slug)) meets))

(defun make-meet-log (meets slug slugs track list-layout item-layout params)
  "Create meeting log page for the given list of meets."
  (setf meets (if slug (select-meets slug meets) meets))
  (let* ((past-meets (loop for m in meets when (not (future-p m)) collect m))
         (past-count (length past-meets))
         (minutes (reduce #'+ (loop for m in past-meets collect (getf m :duration))))
         (members (reduce #'+ (loop for m in past-meets collect (getf m :members))))
         (dst (if slug "_site/cc/{{ slug }}/log.html" "_site/cc/log.html")))
    (aput "head" (fstr "~a, extra.css, meets.css, math.inc"
                       (aget "head" params)) params)
    (aput "title" (fstr "~a Meeting Log" (if slug track "Full")) params)
    (aput "subtitle" "" params)
    (aput "slug" (if slug slug "index") params) ; Needed by next call.
    (aput "track-path"
          (render (if slug "../{{ slug }}/{{ index }}" "../{{ index }}")
                  params) params)
    (aput "track-name" (if track track "club") params)
    (aput "other-title" (if slug "Other Tracks" "Individual Tracks") params)
    (aput "other" (if slug "other" "individual") params)
    (aput "rows" (meet-rows-html meets slugs item-layout params) params)
    (aput "total-count" past-count params)
    (aput "meeting-label" (if (= past-count 1) "meeting" "meetings") params)
    (aput "total-minutes" minutes params)
    (aput "total-hours" (fstr "~,1f" (/ minutes 60)) params)
    (aput "tracks" (meet-tracks-html slug slugs) params)
    ;; Avoid division-by-zero with a fake count.
    (when (zerop past-count)
      (setf past-count 1))
    (aput "average-minutes" (fstr "~,1f" (/ minutes past-count)) params)
    (aput "average-members" (fstr "~,1f" (/ members past-count)) params)
    (write-page dst list-layout params)))

(defun validate-meets-dates (meets)
  "Check that meets are arranged in chronological order."
  (let ((prev-date)
        (curr-date))
    (dolist (meet meets)
      (setf curr-date (getf meet :date))
      (when (and prev-date (string< curr-date prev-date))
        (error "Incorrect order for meet ~a" curr-date))
      (setf prev-date curr-date))))

(defun make-meets (page-layout params)
  "Create meeting log pages for all tracks."
  (let ((meets (read-list "content/lisp/meet.lisp"))
        (slugs (read-list "content/lisp/slug.lisp"))
        (list-layout (read-file "layout/meets/list.html"))
        (item-layout (read-file "layout/meets/item.html")))
    (set-nested-template list-layout page-layout)
    (validate-meets-dates meets)
    (push (list nil nil) slugs)
    (loop for (slug track) in slugs
          do (make-meet-log meets slug slugs track
                            list-layout item-layout params))))


;;; CSS
;;; ---

(defun main-style ()
  "Return primary style and color scheme for the website."
  (list
   ;; HTML elements.
   (cons "font-family" "georgia, serif")
   ;; Light color scheme.
   (cons "light-body-color" "#333")     ; contrast 10.9, 12.6
   (cons "light-link-color" "#00e")     ; contrast  1.3,  9.4, 12.6
   (cons "light-visited-color" "#518")  ; contrast  1.1, 11.5, 12.6
   (cons "light-hover-color" "#03f")    ; contrast  1.8,  7.2, 12.6
   (cons "light-active-color" "#a00")   ; contrast  1.6,  7.8, 12.6
   (cons "light-fill-color" "#eee")     ; contrast  1.2
   (cons "light-shade-color" "#ccc")    ; contrast  1.6
   (cons "light-code-color" "#050")     ; contrast  1.4,  7.9,  9.1
   (cons "light-samp-color" "#730")     ; contrast  1.4,  8.0,  9.3
   (cons "light-hl-color" "#808")       ; contrast  1.4,  7.5,  8.7
   (cons "light-line-color" "#999")     ; contrast  2.8
   (cons "light-success-color" "#060")  ; contrast  7.2
   (cons "light-error-color" "#900")    ; contrast  8.9
   ;; Dark color scheme.
   (cons "dark-background-color" "#111")
   (cons "dark-body-color" "#bbb")      ; contrast 10.9,  9.8
   (cons "dark-link-color" "#9bf")      ; contrast  1.0,  9.8,  9.8
   (cons "dark-visited-color" "#a9f")   ; contrast  1.3,  7.8,  9.8
   (cons "dark-hover-color" "#9cf")     ; contrast  1.1, 11.2,  9.8
   (cons "dark-active-color" "#f99")    ; contrast  1.1,  9.2,  9.8
   (cons "dark-fill-color" "#000")      ; contrast  1.1
   (cons "dark-shade-color" "#333")     ; contrast  1.5
   (cons "dark-code-color" "#9c6")      ; contrast  1.0, 11.2, 10.1
   (cons "dark-samp-color" "#db0")      ; contrast  1.0, 11.2, 10.1
   (cons "dark-hl-color" "#f9c")        ; contrast  1.0, 10.7,  9.6
   (cons "dark-line-color" "#666")      ; contrast  3.3
   (cons "dark-success-color" "#3c6")   ; contrast  9.0
   (cons "dark-error-color" "#f99")))   ; contrast  9.2

(defun make-css ()
  "Generate stylesheets for the main website."
  (make-pages "layout/css/*.css" "_site/css/{{ slug }}.css" "{{ body }}"
              (append (main-style) (list (cons "render" "yes")))))

(defun feed-css ()
  "Return stylesheet for feed as a string."
  (let* ((css (main-style))
         (styles (list (render (read-file "layout/css/main.css") css)
                       (render (read-file "layout/css/extra.css") css))))
    (fstr "~%~a" (indent-lines 10 (join-strings styles)))))

(defun make-xsl ()
  "Generate stylesheet for feed."
  (make-pages "layout/blog/*.xsl" "_site/{{ slug }}.xsl" "{{ body }}"
              (list (cons "css" (feed-css)) (cons "render" "yes"))))


;;; Music
;;; -----

(defun make-music (src page-layout &optional params)
  "Generate music list page."
  (let ((list-layout (read-file "layout/music/list.html"))
        (post-layout (read-file "layout/music/post.html"))
        (item-layout (read-file "layout/music/item.html"))
        (widget-layout (read-file "layout/music/widget.html"))
        (pages))
    ;; Combine layouts to form final layouts.
    (set-nested-template list-layout page-layout)
    (set-nested-template post-layout page-layout)
    ;; Callback function to be passed as a parameter to renderer.
    (defun make-widget-callback (page)
      "Callback function to render music player widget."
      (let* ((widget-params (append page params))
             (rendered-widget (render widget-layout widget-params)))
        (list (cons "widget" rendered-widget))))
    ;; Add parameters for music page rendering.
    (aput "import" "extra.css, music.css" params)
    (aput-list "callbacks" #'make-widget-callback params)
    ;; Render all music pages.
    (setf pages (make-pages src "_site/music/{{ slug }}.html"
                            post-layout params))
    ;; Generate music list page.
    (aput "title" "Music" params)
    (make-page-list pages "_site/music/index.html"
                    list-layout item-layout params)
    pages))


;;; Tags and Feeds
;;; --------------

(defun collect-tags (pages)
  "Group page by tags and return an alist of tag and page list."
  (setf pages (only-listed-items pages))
  (let ((tags))
    (dolist (page pages)
      (dolist (tag (aget "tags" page))
        (aput-list tag page tags)))
    (sort tags (lambda (x y) (< (length (cdr x)) (length (cdr y)))))))

(defun tags-html (tags params)
  "Render tag list as HTML."
  (let ((item-layout (read-file "layout/tag/tag.html"))
        (tag)
        (pages)
        (count)
        (rendered-tags))
    ;; Render each tag-map entry.
    (dolist (tag-entry tags)
      (setf tag (car tag-entry))
      (setf pages (cdr tag-entry))
      (setf count (length pages))
      (aput "tag-slug" (tag-slug tag) params)
      (aput "tag" tag params)
      (aput "count" count params)
      (aput "page-label" (if (= count 1) "page" "pages") params)
      (push (render item-layout params) rendered-tags))
    (join-strings rendered-tags)))

(defun make-tags (pages page-layout params)
  "Generate tag index, tag lists, and tag feeds."
  (let* ((tags-layout (read-file "layout/tag/tags.html"))
         (list-layout (read-file "layout/tag/list.html"))
         (item-layout (read-file "layout/tag/item.html"))
         (feed-xml (read-file "layout/tag/feed.xml"))
         (item-xml (read-file "layout/tag/item.xml"))
         (tags-dst "_site/tag/index.html")
         (list-dst "_site/tag/{{ tag-slug }}.html")
         (mini-feed-dst "_site/tag/{{ tag-slug }}.xml")
         (full-feed-dst "_site/tag/{{ tag-slug }}-full.xml")
         (tags (collect-tags pages))
         (tag)
         (pages))
    (set-nested-template tags-layout page-layout)
    (set-nested-template list-layout page-layout)
    ;; Tag index page.
    (aput "tag-list" (tags-html tags params) params)
    (aput "tag-count" (length tags) params)
    (aput "tag-label" (if (= (length tags) 1) "tag" "tags") params)
    (aput "title" (render "{{ nick }}'s Tags" params) params)
    (aput "subtitle" "" params)
    (write-page tags-dst tags-layout params)
    ;; Tag list page for each tag.
    (dolist (tag-entry tags)
      (setf tag (car tag-entry))
      (setf pages (cdr tag-entry))
      (aput "tag" tag params)
      (aput "tag-slug" (tag-slug tag) params)
      (aput "title" (render "{{ nick }}'s {{ tag }} Pages" params) params)
      (aput "subtitle" "" params)
      (aput "link" (render "{{ site-url }}tag/{{ tag-slug }}.html" params) params)
      (aput "description" (render "Feed for {{ nick }}'s {{ tag }} Pages" params)
            params)
      (make-page-list pages list-dst list-layout item-layout params)
      (make-feed-list pages 20 mini-feed-dst feed-xml item-xml params)
      (make-feed-list pages 10000 full-feed-dst feed-xml item-xml params))))

(defun make-full (pages page-layout params)
  "Generate page list for the full website."
  (let ((list-layout (read-file "layout/full/list.html"))
        (item-layout (read-file "layout/full/item.html")))
    (set-nested-template list-layout page-layout)
    (aput "title" "All Pages" params)
    (make-page-list pages "_site/pages.html" list-layout item-layout params)))

(defun make-full-comments (comments page-layout params)
  "Generate comment list for the full website."
  (let ((list-layout (read-file "layout/comment/list-all.html"))
        (item-layout (read-file "layout/comment/item-all.html")))
    (set-nested-template list-layout page-layout)
    (aput "import" "extra.css, math.inc" params)
    (aput "title" "All Comments" params)
    (setf comments (reverse (number-comments (only-listed-items comments))))
    (make-comment-list comments "_site/comments/index.html"
                       list-layout item-layout params)))

(defun make-short (pages page-layout params)
  "Generate a short links listing page."
  (let ((list-layout (read-file "layout/short/list.html"))
        (item-layout (read-file "layout/short/item.html")))
    (set-nested-template list-layout page-layout)
    (aput "title" "Short Links" params)
    (make-page-list pages "_site/p/index.html" list-layout item-layout params)))

(defun make-feed (pages params)
  "Generate feed for the complete website."
  (let ((feed-xml (read-file "layout/tag/feed.xml"))
        (item-xml (read-file "layout/tag/item.xml")))
    (aput "title" (aget "author" params) params)
    (aput "link" (aget "site-url" params) params)
    (aput "description" (render "{{ nick }}'s Feed" params) params)
    (make-feed-list pages 20 "_site/feed.xml" feed-xml item-xml params)
    (make-feed-list pages 10000 "_site/feed-full.xml" feed-xml item-xml params)))


;;; Home Page
;;; ---------

(defun make-home (pages page-layout params)
  "Generate home page."
  (let ((home-layout (read-file "layout/home/list.html"))
        (item-layout (read-file "layout/blog/item.html")))
    (set-nested-template home-layout page-layout)
    (aput "title" (aget "author" params) params)
    (aput "subtitle" "" params)
    (make-page-list pages "_site/index.html" home-layout item-layout params)))


;;; Complete Website
;;; ----------------

(defvar *params* nil
  "Global parameters that may be provided externally to override any
  default local parameters.")

(defun main ()
  "Generate entire website."
  (remove-directory "_site/")
  (let ((params (list (cons "current-year" (nth-value 5 (get-decoded-time)))
                      (cons "render" "yes")
                      (cons "heads" "")
                      (cons "imports" "")
                      (cons "index" "")
                      (cons "head" "main.css")))
        (page-layout (read-file "layout/page.html"))
        (comments (collect-comments (list "content/comments/*.html"
                                          "content/guestbook/guestbook.html")))
        (all-comments)
        (pages)
        (all-pages))
    ;; If params file exists, merge it with local params.
    (when (probe-file "params.lisp")
      (setf params (append (read-list "params.lisp") params)))
    ;; If *params* exists, merge it with local params.
    (when *params*
      (setf params (append *params* params)))
    ;; Dependencies.
    (copy-directory "_cache/katex/" "_site/js/katex/")
    ;; Stylesheets.
    (make-css)
    (make-xsl)
    ;; Tree.
    (setf all-pages (make-tree "content/tree/" "_site/" page-layout params))
    (make-meets page-layout params)
    ;; Guestbook.
    (multiple-value-bind (pages comments)
        (make-guestbook comments page-layout params)
      (extend-list all-pages pages)
      (extend-list all-comments comments))
    ;; Music
    (extend-list all-pages (make-music "content/music/*.html" page-layout params))
    ;; Maze.
    (setf pages (make-blog "content/maze/*.html" "Maze" page-layout params))
    (extend-list all-comments (make-page-comments pages comments "Maze" page-layout params))
    (extend-list all-pages pages)
    ;; Blog.
    (setf pages (make-blog "content/blog/*.html" "Blog" page-layout params))
    (make-home pages page-layout params)
    (extend-list all-comments (make-page-comments pages comments "Blog" page-layout params))
    (extend-list all-pages pages)
    ;; Aggregates validation.
    (validate-required-params all-pages)
    (validate-unique-keys all-pages)
    ;; Aggregates rendering.
    (make-tags all-pages page-layout params)
    (make-full all-pages page-layout params)
    (make-full-comments all-comments page-layout params)
    (make-short all-pages page-layout params)
    (make-feed all-pages params)
    ;; Directory indices.
    (make-tree-list "_site/" "Tree" page-layout params)
    (make-directory-lists "_site/" page-layout params))
  t)

(when *main-mode*
  (main))
