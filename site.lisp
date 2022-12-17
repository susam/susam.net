;;;; Site Generator
;;;; ==============

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

(defun directory-exists-p (path)
  "Check whether the specified directory exists on the filesystem."
  (uiop:directory-exists-p path))

(defun remove-directory (path)
  "Remove the specified directory tree from the file system."
  (uiop:delete-directory-tree (pathname path) :validate t
                                              :if-does-not-exist :ignore))

(defun directory-name (path)
  "Return only the directory portion of path."
  (namestring (make-pathname :directory (pathname-directory path))))

(defun directory-basename (path)
  "Return the parent directory of the specified pathname."
  (let ((name (car (last (pathname-directory path)))))
    (namestring (make-pathname :directory (list :relative name)))))

(defun copy-file (input output)
  "Copy file from a file path to a file/directory path."
  (when (uiop:directory-pathname-p output)
    (setq output (merge-pathnames (file-namestring input) output)))
  (uiop:copy-file input output))

(defun copy-files (input output)
  "Copy files from a wildcard path to a directory path."
  (dolist (pathname (directory input))
    (copy-file pathname output)))

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

(defun write-file (filename text)
  "Write text to file and close the file."
  (make-directory filename)
  (with-open-file (f filename :direction :output :if-exists :supersede)
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

(defun join-strings (strings)
  "Join strings into a single string."
  (format nil "~{~a~}" strings))

(defun repeat-string (count string)
  "Repeat string count number of times."
  (format nil "~v{~a~:*~}" count (list string)))

(defmacro add-value (key value alist)
  "Add key-value pair to alist."
  `(push (cons ,key ,value) ,alist))

(defmacro add-list-value (key value alist)
  "Add value to a list corresponding to the key in alist."
  `(progn
     (unless (assoc ,key ,alist :test #'string=)
       (push (cons ,key ()) ,alist))
     (push ,value (cdr (assoc ,key ,alist :test #'string=)))))

(defun get-value (key alist)
  "Given a key, return its value found in the list of parameters."
  (cdr (assoc key alist :test #'string=)))

(defun reverse-list-values-in-alist (alist)
  (loop for entry in alist
        collect (cons (car entry) (reverse (cdr entry)))))


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

(defun month-name (month-number)
  "Given a number, return the corresponding month."
  (nth month-number '("X" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defun decode-weekday-name (year month date)
  "Given a date, return the day of week."
  (let* ((encoded-time (encode-universal-time 0 0 0 date month year))
         (decoded-week (nth-value 6 (decode-universal-time encoded-time)))
         (weekday-name (weekday-name decoded-week)))
    weekday-name))

(defun rss-date (date-string)
  "Convert yyyy-mm-dd[ HH:MM[:SS[ TZ]]] to RFC-2822 date."
  (let ((len (length date-string))
        (year (parse-integer (subseq date-string 0 4)))
        (month (parse-integer (subseq date-string 5 7)))
        (date (parse-integer (subseq date-string 8 10)))
        (hour 0)
        (minute 0)
        (second 0)
        (tz "+0000")
        (month-name)
        (weekday-name))
    (when (>= len 16)
      (setf hour (parse-integer (subseq date-string 11 13)))
      (setf minute (parse-integer (subseq date-string 14 16))))
    (when (>= len 19)
      (setf second (parse-integer (subseq date-string 17 19))))
    (when (>= len 21)
      (setf tz (subseq date-string 20)))
    (setf month-name (month-name month))
    (setf weekday-name (decode-weekday-name year month date))
    (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d ~a"
            weekday-name date month-name year hour minute second tz)))

(defun simple-date (date-string &key (sep ""))
  "Convert yyyy-mm-dd[ HH:MM[:SS[ TZ]]] to a human-readable date."
  (let ((len (length date-string))
        (year (parse-integer (subseq date-string 0 4)))
        (month (parse-integer (subseq date-string 5 7)))
        (date (parse-integer (subseq date-string 8 10)))
        (hour 0)
        (minute 0)
        (tz "GMT")
        (month-name)
        (result))
    (setf month-name (month-name month))
    (setf result (format nil "~2,'0d ~a ~4,'0d" date month-name year))
    (when (>= len 16)
      (setf hour (parse-integer (subseq date-string 11 13)))
      (setf minute (parse-integer (subseq date-string 14 16)))
      (setf result (format nil "~a ~a~2,'0d:~2,'0d" result sep hour minute)))
    (when (>= len 21)
      (setf tz (subseq date-string 20))
      (when (string= tz "+0000")
        (setf tz "GMT"))
      (setf result (format nil "~a ~a" result tz)))
    result))

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
               (val (get-value key params)))
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
  (let ((names (uiop:split-string import-header))
        (root (get-value "root" params))
        (snippets))
    (dolist (name names)
      (cond ((string-ends-with ".css" name)
             (push (head-css-html name root) snippets))
            ((string-ends-with ".js" name)
             (push (head-js-html name root) snippets))
            ((string-ends-with ".inc" name)
             (push (head-inc-html name params) snippets))
            (t
             (error (fstr "Unknown import type ~a in ~a" name import-header)))))
    (if snippets (fstr "~{~a~}" (reverse snippets)) "")))

(defun relative-root-path (path)
  "Return relative path to web root from the given rendered file path."
  (repeat-string (count #\/ (string-replace "_site/" "" path)) "../"))

(defun neat-url-path (path)
  "Create canonical path component of the URL for the given rendered file path."
  (string-replace "index.html" "" (string-replace "_site/" "" path)))

(defun neat-url (path params)
  "Create canonical URL for the given rendered file path."
  (fstr "~a~a" (get-value "site-url" params) (neat-url-path path)))

(defmacro add-head-params (dst-path params)
  "Given an output file path, set a canonical URL for that file."
  `(progn
     (add-value "root" (relative-root-path ,dst-path) ,params)
     (add-value "canonical-url" (neat-url ,dst-path ,params) ,params)
     (add-value "heads" (head-html (get-value "head" ,params) ,params) ,params)
     (add-value "imports" (head-html (get-value "import" ,params) ,params) ,params)))

(defmacro invoke-callback (params)
  "Run callback and add the parameters returned by it to params."
  `(let ((callback (get-value "callback" ,params))
         (callback-params))
     (when callback
       (setf callback-params (funcall callback ,params))
       (setf ,params (append callback-params ,params)))))

(defun count-listed-posts (posts)
  "Count the number of posts that are allowed to be listed in a post list."
  (loop for post in posts count (string/= (get-value "unlist" post) "yes")))

(defun extra-markup (text)
  "Add extra markup to the page to create heading anchor links."
  (with-output-to-string (s)
    (let* ((begin-tag-index)
           (end-id-index)
           (end-tag-index)
           (next-index 0))
      (loop
         (setf begin-tag-index (search "<h" text :start2 next-index))
         (unless begin-tag-index
           (return))
         (cond ((and (digit-char-p (char text (+ begin-tag-index 2)))
                     (substring-at "id=\"" text (+ begin-tag-index 4)))

                (setf end-id-index (search "\"" text
                                           :start2 (+ begin-tag-index 8)))
                (setf end-tag-index (search "</h" text
                                            :start2 (+ end-id-index 2)))
                (format s "~a" (subseq text next-index end-tag-index))
                (format s "<a href=\"#~a\"></a></h"
                        (subseq text (+ begin-tag-index 8) end-id-index))
                (setf next-index (+ end-tag-index 3)))
               (t
                (format s "~a" (subseq text next-index (+ begin-tag-index 2)))
                (setf next-index (+ begin-tag-index 2)))))
      (format s "~a" (subseq text next-index)))))

(defun write-page (dst-path layout params)
  "Render given layout with given parameters and write page."
  (setf dst-path (render dst-path params))
  (write-log "Writing ~a ..." dst-path)
  (add-head-params dst-path params)
  (write-file dst-path (extra-markup (render layout params))))

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

(defun first-word (s)
  "Return the first word from the given string."
  (first (uiop:split-string s)))


;;; Posts
;;; -----

(defun read-post (filename)
  "Parse post file."
  (let ((text (read-file filename))
        (post)
        (date))
    (multiple-value-bind (date slug) (date-slug filename)
      (add-value "date" date post)
      (add-value "slug" slug post))
    (multiple-value-bind (headers next-index) (read-headers text 0)
      (setf post (append headers post))
      (add-value "body" (subseq text next-index) post))
    (setf date (get-value "date" post))
    (when date
      (add-value "rss-date" (rss-date date) post)
      (add-value "simple-date" (simple-date date) post))
    post))

(defun make-post (src-path dst layout params)
  "Generate page from post or content file."
  (let* ((post (read-post src-path))
         (body))
    ;; Read post and merge its parameters with call parameters.
    (setf params (append post params))
    (invoke-callback params)
    ;; Render placeholder in post body if requested.
    (when (string= (get-value "render" params) "yes")
      (setf body (render (get-value "body" params) params))
      (add-value "body" body post)      ; make-reading needs this.
      (add-value "body" body params))
    (write-page dst layout params)
    post))

(defun make-posts (src dst layout params)
  "Generate pages from post or content files."
  (let ((posts))
    (dolist (src-path (directory src))
      (push (make-post src-path dst layout params) posts))
    (sort posts (lambda (x y) (string< (get-value "date" x)
                                       (get-value "date" y))))))

(defun make-post-list (posts dst list-layout item-layout params)
  "Generate list page for a list of posts."
  (let ((count (count-listed-posts posts))
        (rendered-posts))
    ;; Render each post.
    (dolist (post posts)
      (unless (string= (get-value "unlist" post) "yes")
        (setf post (append post params))
        (invoke-callback post)
        (push (render item-layout post) rendered-posts)))
    ;; Add list parameters.
    (add-value "body" (join-strings rendered-posts) params)
    (add-value "count" count params)
    (add-value "post-label" (if (= count 1) "post" "posts") params)
    ;; Determine destination path and URL.
    (write-page dst list-layout params)))


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
    (setf commenter (get-value "name" comment))
    (setf url (get-value "url" comment))
    (when url
      (setf commenter (fstr "<a href=\"~a\">~a</a>" url commenter)))
    (add-value "commenter" commenter comment)
    ;; Formatted dates.
    (setf date (get-value "date" comment))
    (add-value "rss-date" (rss-date date) comment)
    (add-value "simple-date" (simple-date date) comment)
    ;; Select content until next header or end-of-text as body.
    (setf next-index (search start-token text :start2 start-index))
    (add-value "body" (subseq text start-index next-index) comment)
    (values comment next-index)))

(defun read-comments (filename)
  "Read all comments from a comment file."
  (let ((text (read-file filename))
        (next-index 0)
        (slug (nth-value 1 (date-slug filename)))
        (comment)
        (comments))
    (loop
     (setf (values comment next-index) (read-comment text next-index))
     ;; Current comment date must be more recent than the previous comment.
     (when (and (consp comments) (string< (get-value "date" comment)
                                          (get-value "date" (car comments))))
       (error (fstr "Incorrect order for comment ~a in ~a"
                    (get-value "date" comment) filename)))
     (push comment comments)
     (unless next-index
       (return)))
    (values comments slug)))

(defun check-comment-dates (comments)
  "Ensure that every comment on this site includes UTC timezone."
  (dolist (comment comments)
    (unless (string-ends-with " +0000" (get-value "date" comment))
      (error (fstr "Time zone missing in comment date ~a in ~a"
                   (get-value "date" comment)
                   (get-value "slug" comment))))))

(defun make-comment-list (post comments dst list-layout item-layout params)
  "Generate comment list page."
  (let* ((post-slug (get-value "slug" post))
         (post-title (get-value "title" post))
         (post-import (get-value "import" post))
         (count (length comments))
         (comment-label (if (= count 1) "comment" "comments"))
         (rendered-comments))
    ;; Add comment item parameters.
    (add-value "slug" post-slug params)
    (add-value "title" (fstr "Comments on ~a" post-title) params)
    (add-value "post-title" (get-value "title" post) params)
    (add-value "count" count params)
    (add-value "comment-label" comment-label params)
    ;; Render each comment.
    (loop for index from count downto 1
          for comment in comments
          do (setf comment (append comment params))
             (add-value "index" index comment)
             (add-value "commenter-type"
                        (if (string= (get-value "name" comment)
                                     (get-value "author" params))
                            "author" "visitor") comment)
             (push (render item-layout comment) rendered-comments))
    ;; Inherit imports from post.
    (if post-import
        (setf post-import (fstr "comment.css ~a" post-import))
        (setf post-import "comment.css"))
    (add-value "import" post-import params)
    ;; Determine destination path and URL.
    (add-value "body" (join-strings rendered-comments) params)
    (write-page dst list-layout params)))

(defun make-comment-none (post dst none-layout params)
  "Generate a comment page with no comments."
  (let* ((post-slug (get-value "slug" post))
         (post-title (get-value "title" post)))
    ;; Set list parameters.
    (add-value "slug" post-slug params)
    (add-value "title" (fstr "Comments on ~a" post-title) params )
    (add-value "post-title" post-title params)
    ;; Determine destination path and URL.
    (write-page dst none-layout params)))


;;; Tags
;;; ----

(defun collect-tags (posts)
  "Group post by tags and return an alist of tag and post list."
  (let ((tags))
    (dolist (post posts)
      (dolist (tag (uiop:split-string (get-value "tag" post)))
        (add-list-value tag post tags)))
    ;; Sort posts in chronological order under each tag.
    (dolist (tag-entry tags)
      (setf (cdr tag-entry)
            (sort (cdr tag-entry) (lambda (x y)
                                    (string< (get-value "date" x)
                                             (get-value "date" y))))))
    ;; Sort tags in ascending order of post count.
    (sort tags (lambda (x y) (< (count-listed-posts (cdr x))
                                (count-listed-posts (cdr y)))))))

(defun tags-html (tags params)
  "Render tag list as HTML."
  (let ((item-layout (read-file "layout/tag/tag.html"))
        (tag)
        (posts)
        (count)
        (rendered-tags))
    ;; Render each tag-map entry.
    (dolist (tag-entry tags)
      (setf tag (car tag-entry))
      (setf posts (cdr tag-entry))
      (setf count (count-listed-posts posts))
      (add-value "tag-slug" (string-downcase tag) params)
      (add-value "tag-title" tag params)
      (add-value "count" count params)
      (add-value "post-label" (if (= count 1) "post" "posts") params)
      (push (render item-layout params) rendered-tags))
    (join-strings rendered-tags)))


;;; Tree
;;; ----

(defun updated-date-callback (post)
  "Create last-updated parameter if post contains updated date."
  (let ((updated (get-value "updated" post))
        (last-updated ""))
    (when updated
      (setf last-updated
            (fstr "&nbsp;&bull;&nbsp;(last updated on ~a)" (simple-date updated))))
    (list (cons "last-updated" last-updated))))

(defun make-tree (src dst page-layout post-layout params)
  "Recursively descend into a directory tree and render/copy all files."
  (make-directory dst)
  (add-value "callback" #'updated-date-callback params)
  (dolist (pathname (uiop:directory-files src))
    (let* ((basename (file-namestring pathname))
           (destpath (namestring (merge-pathnames basename dst))))
      (cond ((string-ends-with ".page.html" basename)
             (setf destpath (string-replace ".page.html" ".html" destpath))
             (make-post pathname destpath page-layout params))
            ((string-ends-with ".post.html" basename)
             (setf destpath (string-replace ".post.html" ".html" destpath))
             (make-post pathname destpath post-layout params))
            (t
             (uiop:copy-file pathname destpath)))))
  (dolist (pathname (uiop:subdirectories src))
    (let* ((basename (directory-basename pathname))
           (destpath (merge-pathnames basename dst)))
      (make-directory destpath)
      (make-tree pathname destpath page-layout post-layout params))))


;;; Directory Listing
;;; -----------------

(defun make-directory-index (current-pathname paths-and-sizes
                             dst-filenames page-layout params)
  "Render index pages for the given current directory."
  (let* ((list-layout (read-file "layout/index/list.html"))
         (item-layout (read-file "layout/index/item.html"))
         (rendered-rows))
    (set-nested-template list-layout page-layout)
    (setf paths-and-sizes (sort paths-and-sizes
                                (lambda (x y) (string> (car x) (car y)))))
    (dolist (entry paths-and-sizes)
      (let ((item-params params))
        (add-value "path" (car entry) item-params)
        (add-value "size" (cdr entry) item-params)
        (if (string-ends-with "/" (car entry))
            (add-value "index" (get-value "index" params) item-params)
            (add-value "index" "" item-params))
        (push (render item-layout item-params) rendered-rows)))
    (add-value "rows" (join-strings rendered-rows) params)
    (dolist (dst-path dst-filenames)
      (setf dst-path (enough-namestring (merge-pathnames dst-path current-pathname)))
      (unless (probe-file dst-path)
        (write-page dst-path list-layout params)))))

(defun visit-directory (apex-pathname current-pathname dst-filenames title
                        page-layout params max-render-depth)
  "Make index pages for the given current directory and its subdirectories."
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
    (add-value "url-path" url-path params)
    (add-value "title" (render title params) params)
    (when (plusp max-render-depth)
      (make-directory-index current-pathname paths-and-sizes dst-filenames
                              page-layout params))
    ;; Return total size of current directory tree to caller.
    total-size))

(defun make-directory-lists (path page-layout &optional params)
  "Make index pages for each site directory."
  (visit-directory (truename "_site/") (truename path)
                   '("index.html" "ls.html") "Index of {{ url-path }}"
                   page-layout params 100))

(defun visit-tree-directory (apex-pathname current-pathname page-layout params)
  "Collect paths from the given directory and its subdirectories."
  (let ((paths))
    ;; Collect subdirectories.
    (dolist (path (uiop:subdirectories current-pathname))
      (push (enough-namestring path apex-pathname) paths)
      (setf paths (append (visit-tree-directory apex-pathname path
                                                page-layout params) paths)))
    ;; Collect files.
    (dolist (path (uiop:directory-files current-pathname))
      (push (enough-namestring path apex-pathname) paths))
    paths))

(defun render-tree-list (paths dst-path page-layout params)
  "Render the given list of paths into a page with a flat HTML list."
  (let* ((list-layout (read-file "layout/tree/list.html"))
         (item-layout (read-file "layout/tree/item.html"))
         (rendered-items))
    (set-nested-template list-layout page-layout)
    (dolist (path (sort paths #'string>))
      (let ((item-params params))
        (add-value "path" path item-params)
        (if (string-ends-with "/" path)
            (add-value "index" (get-value "index" params) item-params)
            (add-value "index" "" item-params))
        (push (render item-layout item-params) rendered-items)))
    (add-value "title" "Maze Tree" params)
    (add-value "items" (join-strings rendered-items) params)
    (write-page dst-path list-layout params)))

(defun make-tree-list (path page-layout params)
  "Generate a flat tree listing of the given directory."
  (let ((paths (visit-tree-directory (truename path) (truename path)
                                     page-layout params))
        (dst-path (namestring (merge-pathnames "TREE.html" path))))
    (render-tree-list paths dst-path page-layout params)))


;;; Zone Components
;;; ---------------

(defun make-zone-blog (src page-layout params)
  "Generate blog."
  (let ((post-layout (read-file "layout/blog/post.html"))
        (list-layout (read-file "layout/blog/list.html"))
        (item-layout (read-file "layout/blog/item.html"))
        (feed-xml (read-file "layout/blog/feed.xml"))
        (item-xml (read-file "layout/blog/item.xml"))
        (post-dst "_site/{{ zone-slug }}/{{ slug }}.html")
        (list-dst "_site/{{ zone-slug }}/index.html")
        (feed-dst "_site/{{ zone-slug }}/feed.xml")
        (posts))
    ;; Combine layouts to form final layouts.
    (set-nested-template post-layout page-layout)
    (set-nested-template list-layout page-layout)
    ;; Read and render all posts.
    (setf posts (make-posts src post-dst post-layout params))
    ;; Create blog list page.
    (add-value "subtitle" "" params)
    ;; If blog list filename is unavailable, choose different filename.
    (when (probe-file (render list-dst params))
      (setf list-dst "_site/{{ zone-slug }}/posts.html"))
    (make-post-list posts list-dst list-layout item-layout params)
    ;; Create RSS feed.
    (make-post-list posts feed-dst feed-xml item-xml params)
    posts))

(defun make-zone-comments (posts src page-layout &optional params)
  "Generate comment list pages or no comments pages for all posts."
  (let ((none-layout (read-file "layout/comment/none.html"))
        (list-layout (read-file "layout/comment/list.html"))
        (item-layout (read-file "layout/comment/item.html"))
        (comment-dst (render "_site/{{ zone-slug }}/comments/{{ slug }}.html"
                             params))
        (comment-map))
    ;; Combine layouts to form final layouts.
    (set-nested-template none-layout page-layout)
    (set-nested-template list-layout page-layout)
    ;; Read all comments.
    (dolist (src-path (directory src))
      (multiple-value-bind (comments slug) (read-comments src-path)
        (check-comment-dates comments)
        (add-value slug comments comment-map)))
    ;; Add parameters for comment list rendering.
    ;; For each post, render its comment list page.
    (dolist (post posts)
      (let* ((slug (get-value "slug" post))
             (comments (get-value slug comment-map)))
        (if (get-value slug comment-map)
            (make-comment-list post comments comment-dst
                               list-layout item-layout params)
            (make-comment-none post comment-dst none-layout params))))))

(defun make-zone-tags (tags dst page-layout params)
  "Generate blog for a specific tag"
  (let ((tags-layout (read-file "layout/tag/tags.html"))
        (list-layout (read-file "layout/tag/list.html"))
        (item-layout (read-file "layout/tag/item.html"))
        (feed-xml (read-file "layout/blog/feed.xml"))
        (item-xml (read-file "layout/blog/item.xml"))
        (tags-dst (namestring (merge-pathnames "index.html" dst)))
        (list-dst (namestring (merge-pathnames "{{ tag-slug }}.html" dst)))
        (feed-dst (namestring (merge-pathnames "{{ tag-slug }}.xml" dst)))
        (tag)
        (posts)
        (title))
    (set-nested-template tags-layout page-layout)
    (set-nested-template list-layout page-layout)
    (add-value "tag-list" (tags-html tags params) params)
    (add-value "tag-count" (length tags) params)
    (add-value "tag-label" (if (= (length tags) 1) "tag" "tags") params)
    (write-page tags-dst tags-layout params)
    (dolist (tag-entry tags)
      (setf tag (car tag-entry))
      (setf posts (cdr tag-entry))
      (add-value "tag-slug" (string-downcase tag) params)
      (add-value "tag-title" tag params)
      (setf title (render "Susam's {{ tag-title }} {{ zone-name }}" params))
      (add-value "title" title params)
      (make-post-list posts list-dst list-layout item-layout params)
      (make-post-list posts feed-dst feed-xml item-xml params))))

(defun make-zone-tree (src page-layout &optional params)
  "Copy and render files found recursively in the given path."
  (let ((post-layout (read-file "layout/tree/post.html"))
        (dst (render "_site/{{ zone-slug }}/" params)))
    (set-nested-template post-layout page-layout)
    (make-tree src dst page-layout post-layout params)))

(defun make-more-list (path page-layout &optional params)
  "Make index pages for each site directory."
  (visit-directory (truename "_site/") (truename path) '("more.html")
                   (render "More from {{ zone-name }}" params)
                   page-layout params 1))


;;; Complete Zone
;;; -------------

(defun zone-title (zone-slug zone-name author-name)
  "Map slug to title of the zone's blog."
  (let* ((author-nick (first-word author-name)))
    (cond ((string= zone-slug "blog")
           author-name)
          ((string= zone-slug "maze")
           (fstr "~a's ~a" author-nick zone-name))
          ((string= zone-slug "club")
           (fstr "~a's Computation ~a" author-nick zone-name)))))

(defun make-zone (zone-slug page-layout params)
  "Create a complete zone with blog, tags, and tree."
  (let* ((author-name (get-value "author" params))
         (zone-name (string-capitalize zone-slug))
         (zone-title (zone-title zone-slug zone-name author-name))
         (dst-dir (fstr "_site/~a/" zone-slug))
         (tags-dst "_site/{{ zone-slug }}/tag/")
         (posts)
         (tags))
    (add-value "zone-slug" zone-slug params)
    (add-value "zone-name" zone-name params)
    (add-value "title" zone-title params)
    (add-value "subtitle" (fstr " - ~a" zone-title) params)
    (make-zone-tree (fstr "content/~a/tree/" zone-slug) page-layout params)
    (make-tree-list dst-dir page-layout params)
    (make-more-list dst-dir page-layout params)
    (setf posts (make-zone-blog (fstr "content/~a/posts/*.html" zone-slug)
                                page-layout params))
    (setf tags (collect-tags posts))
    (make-zone-tags tags tags-dst page-layout params)
    (make-zone-comments posts (fstr "content/~a/comments/*.html" zone-slug)
                        page-layout params)
    (make-directory-lists dst-dir page-layout params)
    posts))


;;; Meets
;;; -----

(defun format-meet-date (date)
  "Format meeting entry date for display in meeting list page."
  (string-replace " " "&nbsp;" (subseq (rss-date date) 0 22)))

(defun future-p (meet)
  "Whether the given meeting entry is scheduled for future."
  (minusp (getf meet :members)))

(defun meet-row-html (meet index layout params)
  "Create HTML to represent a single row of meeting entry."
  (add-value "index" index params)
  (add-value "class" (if (future-p meet) "future" "past") params)
  (add-value "date" (format-meet-date (getf meet :date)) params)
  (add-value "duration" (getf meet :duration) params)
  (add-value "members" (if (future-p meet) "-" (getf meet :members)) params)
  (add-value "topic" (getf meet :topic) params)
  (render layout params))

(defun meet-rows-html (meets layout params)
  "Create HTML to represent all rows of meeting entries"
  (join-strings
   (loop for index from 1 to (length meets)
         for meet in meets
         collect (meet-row-html meet index layout params))))

(defun meet-tracks-html (slug slugs)
  "Create HTML to list all club tracks."
  (setf slugs (remove-if (lambda (x) (or (not (car x))
                                         (string= (car x) slug))) slugs))
  (join-strings
   (loop for (slug track) in (reverse slugs)
         collect (fstr "<li><a href=\"~a.html\">~a</a></li>~%" slug track))))

(defun select-meets (slug meets)
  "Filter meets to select the ones with the specified slug."
  (remove-if-not (lambda (x) (string= (getf x :slug) slug)) meets))

(defun make-meet-log (meets slug slugs track dst list-layout item-layout params)
  "Create meeting log page for the given list of meets."
  (setf meets (if slug (select-meets slug meets) meets))
  (let* ((title (fstr "~a Meeting Log" (if slug track "Club")))
         (head (fstr "~a extra.css meets.inc math.inc" (get-value "head" params)))
         (past-meets (loop for m in meets when (not (future-p m)) collect m))
         (past-count (length past-meets))
         (minutes (reduce #'+ (loop for m in past-meets collect (getf m :duration))))
         (members (reduce #'+ (loop for m in past-meets collect (getf m :members)))))
    (add-value "head" head params)
    (add-value "title" title params)
    (add-value "subtitle" " - Computation Club" params)
    (add-value "zone-slug" "club" params)
    (add-value "zone-name" "Club" params)
    (add-value "other-title" (if slug "Other Tracks" "Individual Tracks") params)
    (add-value "other" (if slug "other" "individual") params)
    (add-value "slug" (if slug slug "index") params)
    (add-value "rows" (meet-rows-html meets item-layout params) params)
    (add-value "total-count" past-count params)
    (add-value "meeting-label" (if (= past-count 1) "meeting" "meetings") params)
    (add-value "total-minutes" minutes params)
    (add-value "total-hours" (fstr "~,1f" (/ minutes 60)) params)
    (add-value "tracks" (meet-tracks-html slug slugs) params)
    ;; Avoid division-by-zero with a fake count.
    (when (zerop past-count)
      (setf past-count 1))
    (add-value "average-minutes" (fstr "~,1f" (/ minutes past-count)) params)
    (add-value "average-members" (fstr "~,1f" (/ members past-count)) params)
    (write-page dst list-layout params)))

(defun check-meets-dates (meets)
  "Check that meets are arranged in chronological order."
  (let ((prev-date)
        (curr-date))
    (dolist (meet meets)
      (setf curr-date (getf meet :date))
      (when (and prev-date (string< curr-date prev-date))
        (error "Incorrect order for meet ~a" curr-date))
      (setf prev-date curr-date))))

(defun make-meets (page-layout params)
  "Create meeting log pages for the club and all its tracks."
  (let ((meets (read-from-string (read-file "content/club/meets.lisp")))
        (slugs (read-from-string (read-file "content/club/slugs.lisp")))
        (list-layout (read-file "layout/meets/list.html"))
        (item-layout (read-file "layout/meets/item.html"))
        (dst "_site/club/meets/{{ slug }}.html"))
    (set-nested-template list-layout page-layout)
    (check-meets-dates meets)
    ;; Add meeting log for entire club.
    (push (list nil nil) slugs)
    (loop for (slug track) in slugs
          do (make-meet-log meets slug slugs track dst
                            list-layout item-layout params))))

(defun make-club (page-layout params)
  "Make the club section of the website."
  (make-posts "content/club/*.html" "_site/club/{{ slug }}.html" page-layout params)
  (make-meets page-layout params))


;;; CSS
;;; ---

(defun main-style ()
  "Return primary style and color scheme for the website."
  (list
   ;; HTML elements.
   (cons "font-family" "georgia, serif")
   ;; Light color scheme.
   (cons "light-body-color" "#333")
   (cons "light-link-color" "#00e")
   (cons "light-visited-color" "#518")
   (cons "light-hover-color" "#03f")
   (cons "light-active-color" "#e00")
   (cons "light-fill-color" "#eee")
   (cons "light-shade-color" "#ccc")
   (cons "light-code-color" "#050")
   (cons "light-samp-color" "#730")
   (cons "light-hl-color" "#800")
   (cons "light-line-color" "#999")
   (cons "light-success-color" "#060")
   (cons "light-error-color" "#900")
   ;; Dark color scheme.
   (cons "dark-background-color" "#111")
   (cons "dark-body-color" "#bbb")
   (cons "dark-link-color" "#9bf")
   (cons "dark-visited-color" "#a9f")
   (cons "dark-hover-color" "#9cf")
   (cons "dark-active-color" "#f99")
   (cons "dark-fill-color" "#000")
   (cons "dark-shade-color" "#333")
   (cons "dark-code-color" "#9c6")
   (cons "dark-samp-color" "#db0")
   (cons "dark-hl-color" "#fa6")
   (cons "dark-line-color" "#666")
   (cons "dark-success-color" "#3c6")
   (cons "dark-error-color" "#f99")))

(defun make-css ()
  "Generate stylesheets for the main website."
  (dolist (filename (list "comment.css" "extra.css" "form.css"
                          "main.css" "music.css" "reading.css"))
    (let ((css-layout (read-file (fstr "layout/css/~a" filename)))
          (css-path (fstr "_site/css/~a" filename)))
      (write-file css-path (render css-layout (main-style))))))


;;; Reading
;;; -------

(defun links-html (post)
  "Generate HTML for reference links in reading post."
  (let ((key-attrs (list (cons "pdf" "PDF") (cons "url" "url")))
        (result))
    (dolist (entry key-attrs)
      (let* ((key (car entry))
             (label (cdr entry))
             (link (get-value key post)))
        (when link
          (push (fstr " [<a href=\"~a\" class=\"basic\">~a</a>]"
                      link label) result))))
    (join-strings result)))

(defun read-sections (body)
  "Read sections within post body."
  (let* ((start-token "<!-- ")
         (end-token (format nil " -->~%"))
         (start-len (length start-token))
         (end-len (length end-token))
         (start-index 0)   ; Index of start-token.
         (end-index)       ; Index of end-token.
         (section-name)    ; Text between start-token and end-token.
         (section-body)    ; Text between end-token and next start-token/EOT.
         (result))
    (loop
      ;; Extract section name between start-token and end-token.
      (unless (setf start-index (search start-token body :start2 start-index))
        (return))
      (incf start-index start-len)
      (unless (setf end-index (search end-token body :start2 start-index))
        (return))
      (setf section-name (subseq body start-index end-index))
      ;; Extract section body between end-token and next start-token/EOT.
      (setf start-index (+ end-index end-len))
      (setf end-index (search start-token body :start2 start-index))
      (setf section-body (subseq body start-index end-index))
      ;; Collect section name and section body.
      (add-list-value section-name section-body result)
      (unless end-index
        (return)))
    ;; Order the section bodies in the order they were read.
    (reverse-list-values-in-alist result)))

(defun make-reading (src page-layout params)
  "Generate reading list page."
  (let ((tag-defs '(("technical" "Technical" "book" "books")
                    ("textbook" "Textbooks" "book" "books")
                    ("paper" "Papers" "paper" "papers")
                    ("non-fiction" "Non-Fiction" "book" "books")
                    ("fiction" "Fiction" "book" "books")))
        (read-layout (read-file "layout/reading/read.html"))
        (tagl-layout (read-file "layout/reading/tagl.html"))
        (item-layout (read-file "layout/reading/tagi.html"))
        (tocl-layout (read-file "layout/reading/tocl.html"))
        (toci-layout (read-file "layout/reading/toci.html"))
        (dst-path "_site/reading.html")
        (tag-map)
        (toc-list)
        (tag-list))
    ;; Combine layouts to form final layouts.
    (set-nested-template read-layout page-layout)
    ;; Read all reading posts and insert them into tag-map by tags
    ;; such that each entry in tag-map maps a tag to a list of posts.
    (dolist (src-path (directory src))
      (let* ((post (read-post src-path))
             (tag (get-value "tag" post)))
        (setf post (append (read-sections (get-value "body" post)) post))
        (add-value "extra" (links-html post) post)
        (unless (get-value tag tag-defs)
          (error (fstr  "Unknown tag ~a in ~a" tag src-path)))
        (add-list-value tag post tag-map)))
    ;; Render posts under each tag to form tag lists for each tag.
    (dolist (tag-def tag-defs)
      (let* ((tag-name (car tag-def))
             (posts (get-value tag-name tag-map))
             (count (length posts))
             (tag-title (first (cdr tag-def)))
             (singular (second (cdr tag-def)))
             (plural (third (cdr tag-def)))
             (tag-params params)
             (toc-items)
             (tag-items))
        ;; Sort posts under current tag in reverse chronological order.
        (setf posts (sort posts (lambda (x y) (string< (get-value "date" x)
                                                       (get-value "date" y)))))
        ;; Ensure the post list under the current tag is not empty.
        (unless (zerop count)
          ;; Add parameters for rendering tag list for current tag.
          (add-value "tag" tag-name tag-params)
          (add-value "tag-title" (string-capitalize tag-title) tag-params)
          (add-value "count" count tag-params)
          (add-value "tag-label" (if (= count 1) singular plural) tag-params)
          ;; Render posts under current tag to form tag list for current tag.
          (dolist (post posts)
            (let ((post-params (append post params))
                  (quotes (loop for q in (get-value "quote" post)
                                collect
                                (fstr "<blockquote>~%~a</blockquote>" q))))
              (add-value "quotes" (join-strings quotes) post-params)
              (add-value "notes" (join-strings (get-value "note" post)) post-params)
              (add-value "quote-title" (if (= (length quotes) 1)
                                           "An Excerpt"
                                           "Some Excerpts") post-params)
              (push (render item-layout post-params) tag-items)
              (push (render toci-layout post-params) toc-items)))
          ;; Render table of contents for current tag.
          (add-value "body" (join-strings toc-items) tag-params)
          (push (render tocl-layout tag-params) toc-list)
          ;; Render tag list for current tag.
          (add-value "body" (join-strings tag-items) tag-params)
          (push (render tagl-layout tag-params) tag-list))))
    ;; Add parameters for reading list page.
    (add-value "body" (join-strings tag-list) params)
    (add-value "toc" (join-strings toc-list) params)
    (add-value "title" "My Reading List" params)
    (add-value "import" "reading.css math.inc" params)
    (write-page dst-path read-layout params)))


;;; Music
;;; -----

(defun make-music (src page-layout &optional params)
  "Generate music list page."
  (let ((list-layout (read-file "layout/music/list.html"))
        (post-layout (read-file "layout/music/post.html"))
        (item-layout (read-file "layout/music/item.html"))
        (widget-layout (read-file "layout/music/widget.html"))
        (posts))
    ;; Combine layouts to form final layouts.
    (set-nested-template list-layout page-layout)
    (set-nested-template post-layout page-layout)
    ;; Callback function to be passed as a parameter to renderer.
    (defun make-widget-callback (post)
      "Callback function to render music player widget."
      (let* ((widget-params (append post params))
             (rendered-widget (render widget-layout widget-params)))
        (list (cons "widget" rendered-widget))))
    ;; Add parameters for music post rendering.
    (add-value "import" "extra.css music.css" params)
    (add-value "callback" #'make-widget-callback params)
    ;; Render all music posts.
    (setf posts (make-posts src "_site/music/{{ slug }}.html"
                            post-layout params))
    ;; Generate music list page.
    (add-value "title" "Music" params)
    (make-post-list posts "_site/music/index.html"
                    list-layout item-layout params)))


;;; Home Page
;;; ---------

(defun make-home (posts page-layout params)
  "Generate home page."
  (let ((home-layout (read-file "layout/home/list.html"))
        (item-layout (read-file "layout/home/item.html")))
    (set-nested-template home-layout page-layout)
    (add-value "zone-name" "Blog" params)
    (add-value "zone-slug" "blog" params)
    (add-value "title" "Susam Pal" params)
    (add-value "subtitle" "" params)
    (make-post-list posts "_site/index.html" home-layout item-layout params)))


;;; Complete Website
;;; ----------------

(defvar *params* nil
  "Global parameters that may be provided externally to override any
  default local parameters.")

(defun main ()
  "Generate entire website."
  (remove-directory "_site/")
  (let ((params (list (cons "author" "Susam Pal")
                      (cons "subtitle" " - Susam Pal")
                      (cons "zone-slug" "blog")
                      (cons "zone-name" "Blog")
                      (cons "site-url" "https://susam.net/")
                      (cons "initial-year" 2001)
                      (cons "current-year" (nth-value 5 (get-decoded-time)))
                      (cons "render" "yes")
                      (cons "heads" "")
                      (cons "imports" "")
                      (cons "index" "")))
        (page-layout (read-file "layout/page.html"))
        (posts))
    ;; If *params* exists, merge it with local params.
    (when *params*
      (setf params (append *params* params)))
    ;; Static files.
    (copy-directory "static/" "_site/")
    (copy-directory "_cache/mathjax/" "_site/js/mathjax/")
    ;; Stylesheet.
    (make-css)
    (add-value "head" "main.css" params)
    (make-zone "maze" page-layout params)
    (make-zone "club" page-layout params)
    (make-meets page-layout params)
    (setf posts (make-zone "blog" page-layout params))
    (make-home posts page-layout params)
    (make-music "content/music/*.html" page-layout params)
    (make-reading "content/reading/*.html" page-layout params)
    (make-posts "content/*.html" "_site/{{ slug }}.html" page-layout params)
    (make-directory-lists "_site/" page-layout params))
  t)

(when *main-mode*
  (main))
