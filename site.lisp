;;;; Site Generator
;;;; ==============

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

(defun simple-date (date-string)
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
      (setf result (format nil "~a ~2,'0d:~2,'0d" result hour minute)))
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
      (format s (subseq text next-index)))))


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

(defun head-html (import-header root)
  "Given the value of an import header, return HTML code for it."
  (let ((tokens (uiop:split-string import-header))
        (fstr)
        (result ""))
    (dolist (token tokens)
      (cond ((string-ends-with ".js" token)
             (setf fstr "~a  <script src=\"~ajs/~a\"></script>~%"))
            ((string-ends-with ".css" token)
             (setf fstr "~a  <link rel=\"stylesheet\" href=\"~acss/~a\">~%"))
            (t
             (setf fstr "Unknown import type ~s in ~s")
             (error (format nil fstr token import-header))))
      (setf result (format nil fstr result root token)))
    result))

(defmacro add-imports (params)
  "Add head element imports to params if an import is specified."
  `(let* ((import (get-value "import" ,params))
          (root (get-value "root" ,params)))
     (add-value "imports" (head-html import root) ,params)))

(defmacro add-canonical-url (dst-path params)
  "Given an output file path, set a canonical URL for that file."
  `(let ((neat-url ,dst-path)
         (site-url (get-value "site-url" ,params)))
     (setf neat-url (string-replace "_site/" "" neat-url))
     (setf neat-url (string-replace "index.html" "" neat-url))
     (setf neat-url (format nil "~a~a" site-url neat-url))
     (add-value "canonical-url" neat-url ,params)))

(defmacro invoke-callback (params)
  "Run callback and add the parameters returned by it to params."
  `(let ((callback (get-value "callback" ,params))
         (callback-params))
     (when callback
       (setf callback-params (funcall callback ,params))
       (setf ,params (append callback-params ,params)))))

(defun make-posts (src dst layout &optional params)
  "Generate pages from post files."
  (let ((post)           ; Parameters read from post.
        (page)           ; Parameters for current page.
        (body)           ; Post body.
        (posts)          ; List of post parameters.
        (dst-path)       ; Destination path to write rendered page to.
        (render))        ; Render flag in post parameters.
    (dolist (src-path (directory src))
      ;; Read post and merge its parameters with call parameters.
      (setf post (read-post src-path))
      (setf page (append post params post))
      (invoke-callback page)
      (add-imports page)
      ;; Render placeholder in post body if requested.
      (setf render (get-value "render" page))
      (when (string= render "yes")
        (setf body (render (get-value "body" page) page))
        (add-value "body" body post)
        (add-value "body" body page))
      (push post posts)
      ;; Determine destination path and URL.
      (setf dst-path (render dst page))
      (add-canonical-url dst-path page)
      ;; Render the post using the layout.
      (write-log "Rendering ~a => ~a ..." (get-value "slug" page) dst-path)
      (write-file dst-path (extra-markup (render layout page))))
    ;; Sort the posts in chronological order.
    (sort posts #'(lambda (x y) (string< (get-value "date" x)
                                         (get-value "date" y))))))

(defun make-post-list (posts dst list-layout item-layout
                       &optional params)
  "Generate list page for a list of posts."
  (let ((count (length posts))
        (rendered-posts)
        (dst-path))
    ;; Render each post.
    (dolist (post posts)
      (setf post (append post params))
      (invoke-callback post)
      (push (render item-layout post) rendered-posts))
    ;; Add list parameters.
    (add-value "body" (join-strings rendered-posts) params)
    (add-value "count" count params)
    (add-value "post-label" (if (= count 1) "post" "posts") params)
    (add-imports params)
    ;; Determine destination path and URL.
    (setf dst-path (render dst params))
    (add-canonical-url dst-path params)
    ;; Render the post using list layout.
    (write-log "Rendering list => ~a ..." dst-path)
    (write-file dst-path (render list-layout params))))


;;; Comments
;;; --------

(defun read-comment (text start-index)
  "Read a single comment from a comment file."
  (let ((start-token "<!-- ") ; Header prefix.
        (commenter)           ; Rendered commenter display name.
        (url)                 ; URL of commenter.
        (comment)             ; Parsed comment parameters.
        (next-index))         ; Index at which to search next comment.
    (setf (values comment start-index) (read-headers text start-index))
    ;; Determine commenter's display name.
    (setf commenter (get-value "name" comment))
    (setf url (get-value "url" comment))
    (when url
      (setf commenter (format nil "<a href=\"~a\">~a</a>" url commenter)))
    (add-value "commenter" commenter comment)
    ;; Human-readable date.
    (add-value "simple-date" (simple-date (get-value "date" comment)) comment)
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
      (push comment comments)
      (unless next-index
        (return)))
    (setf comments (sort comments #'(lambda (x y)
                                      (string> (get-value "date" x)
                                               (get-value "date" y)))))
    (values slug comments)))

(defun make-comment-list (post comments dst list-layout item-layout
                          &optional params)
  "Generate comment list page."
  (let* ((post-slug (get-value "slug" post))
         (post-title (get-value "title" post))
         (post-import (get-value "import" post))
         (count (length comments))
         (comment-label (if (= count 1) "comment" "comments"))
         (rendered-comments)
         (dst-path))
    ;; Add comment item parameters.
    (add-value "slug" post-slug params)
    (add-value "title" (format nil "Comments on ~a" post-title) params)
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
        (setf post-import (format nil "comment.css ~a" post-import))
        (setf post-import "comment.css"))
    (add-value "import" post-import params)
    (add-imports params)
    ;; Determine destination path and URL.
    (add-value "body" (join-strings rendered-comments) params)
    (setf dst-path (render dst params))
    (add-canonical-url dst-path params)
    ;; Render comment list.
    (write-log "Rendering ~a => ~a ..." post-slug dst-path)
    (write-file dst-path (render list-layout params))))

(defun make-comment-none (post dst none-layout &optional params)
  "Generate a comment page with no comments."
  (let* ((post-slug (get-value "slug" post))
         (post-title (get-value "title" post))
         (dst-path))
    ;; Set list parameters.
    (add-value "slug" post-slug params)
    (add-value "title" (format nil "Comments on ~a" post-title) params )
    (add-value "post-title" post-title params)
    ;; Determine destination path and URL.
    (setf dst-path (render dst params))
    (add-canonical-url dst-path params)
    ;; Render comment list.
    (write-log "Rendering ~a => ~a ..." post-slug dst-path)
    (write-file dst-path (render none-layout params))))

(defun links-html (post)
  "Generate HTML for reference links in reading post."
  (let ((key-attrs (list (cons "pdf" "PDF") (cons "url" "url")))
        (result))
    (dolist (entry key-attrs)
      (let* ((key (car entry))
             (label (cdr entry))
             (link (get-value key post)))
        (when link
          (push (format nil " [<a href=\"~a\" class=\"basic\">~a</a>]"
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


;;; Reading Log
;;; -----------

(defun make-reading (src page-layout &optional params)
  "Generate reading list page."
  (let ((tag-defs '(("non-fiction" . ("Non-Fiction" "book" "books"))
                    ("technical" . ("Technical" "book" "books"))
                    ("textbook" . ("Textbooks" "book" "books"))
                    ("paper" . ("Papers" "paper" "papers"))
                    ("fiction" . ("Fiction" "book" "books"))))
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
    (setf read-layout (render page-layout (list (cons "body" read-layout))))
    ;; Read all reading posts and insert them into tag-map by tags
    ;; such that each entry in tag-map maps a tag to a list of posts.
    (dolist (src-path (directory src))
      (let* ((post (read-post src-path))
             (tag (get-value "tag" post)))
        (setf post (append (read-sections (get-value "body" post)) post))
        (add-value "extra" (links-html post) post)
        (unless (get-value tag tag-defs)
          (error (format nil "Unknown tag ~a in ~a" tag src-path)))
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
        (setf posts (sort posts #'(lambda (x y)
                                    (string< (get-value "date" x)
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
                                (format nil "<blockquote>~%~a</blockquote>" q))))
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
    (add-value "root" "" params)
    (add-value "body" (join-strings tag-list) params)
    (add-value "toc" (join-strings toc-list) params)
    (add-value "title" "My Reading List" params)
    (add-value "import" "reading.css tex.js" params)
    (add-imports params)
    (add-canonical-url dst-path params)
    ;; Render reading list.
    (write-file dst-path (extra-markup (render read-layout params)))))


;;; Text Files
;;; ----------

(defun read-text-files (src)
  "Read all posts into a list."
  (let* ((posts))
    (dolist (src-path (directory src))
      (let* ((post (read-post src-path))
             (body (get-value "body" post))
             (newline-index (position #\newline body)))
        (add-value "basename" (file-namestring src-path) post)
        (add-value "title" (subseq body 0 newline-index) post)
        (push post posts)))
    (sort posts #'(lambda (x y) (string< (get-value "basename" x)
                                         (get-value "basename" y))))))


;;; Site
;;; ----

(defun make-tags (posts dst tags-layout tagh-layout tagl-layout
                  item-layout params)
  (let ((tag-map)
        (tag)
        (rendered-item)
        (rendered-header)
        (rendered-body)
        (dst-path)
        (count))
    ;; Render each post using item-layout and collect them in tag-map.
    (dolist (post posts)
      (setf post (append post params))
      (setf rendered-item (render item-layout post))
      (setf tag (get-value "tag" post))
      (unless (get-value tag tag-map)
        (add-value tag () tag-map))
      (add-list-value tag rendered-item tag-map))
    ;; Sort tag-map entries by ascending order of post count.
    (setf tag-map (sort tag-map #'(lambda (x y)
                                    (< (length (cdr x)) (length (cdr y))))))
    ;; Render each tag-map entry with tagh-layout and tagl-layout.
    (dolist (tag-entry tag-map)
      (setf count (length (cdr tag-entry)))
      (add-value "body" (join-strings (cdr tag-entry)) params)
      (add-value "tag" tag params)
      (add-value "count" count params)
      (add-value "post-label" (if (= count 1) "post" "posts") params)
      (add-value "tag-title" (string-capitalize (car tag-entry))
                 params)
      (push (render tagh-layout params) rendered-header)
      (push (render tagl-layout params) rendered-body))
    ;; Add tag list page parameters.
    (add-value "header" (join-strings rendered-header) params)
    (add-value "body" (join-strings rendered-body) params)
    (add-value "import" "tags.css" params)
    (add-imports params)
    ;; Determine destination path and URL.
    (setf dst-path (render dst params))
    (add-canonical-url dst-path params)
    ;; Render tag list.
    (write-log "Rendering list => ~a ..." dst-path)
    (write-file dst-path (extra-markup (render tags-layout params)))))

(defun make-blog (src page-layout &optional params)
  "Generate blog."
  (let* ((post-layout (read-file "layout/blog/post.html"))
         (list-layout (read-file "layout/blog/list.html"))
         (tags-layout (read-file "layout/blog/tags.html"))
         (tagh-layout (read-file "layout/blog/tagh.html"))
         (tagl-layout (read-file "layout/blog/tagl.html"))
         (item-layout (read-file "layout/blog/item.html"))
         (feed-xml (read-file "layout/blog/feed.xml"))
         (item-xml (read-file "layout/blog/item.xml"))
         (posts)
         (listed))
    ;; Combine layouts to form final layouts.
    (setf post-layout (render page-layout (list (cons "body" post-layout))))
    (setf list-layout (render page-layout (list (cons "body" list-layout))))
    (setf tags-layout (render page-layout (list (cons "body" tags-layout))))
    ;; Add parameters for blog rendering.
    (add-value "root" "../" params)
    (add-value "blog" "blog" params)
    (add-value "render" "yes" params)
    ;; Read and render all posts.
    (setf posts (make-posts src "_site/blog/{{ slug }}.html"
                            post-layout params))
    (setf listed (remove-if #'(lambda (p)
                                (string= (get-value "list" p) "no")) posts))
    ;; Create blog list page as the site home page.
    (add-value "root" "./" params)
    (add-value "blog" "blog" params)
    (add-value "title" "Susam Pal" params)
    (add-value "subtitle" "" params)
    (make-post-list listed "_site/index.html" list-layout item-layout params)
    ;; Create tag list page as the blog page.
    (add-value "root" "../" params)
    (add-value "title" "Susam's Blog" params)
    (make-tags listed "_site/blog/index.html"
               tags-layout tagh-layout tagl-layout item-layout params)
    ;; Create RSS feed.
    (make-post-list listed "_site/blog/rss.xml" feed-xml item-xml params)
    posts))

(defun make-comments (posts src page-layout &optional params)
  "Generate comment list pages or no comments pages for all posts."
  (let ((none-layout (read-file "layout/comments/none.html"))
        (list-layout (read-file "layout/comments/list.html"))
        (item-layout (read-file "layout/comments/item.html"))
        (dst "_site/{{ blog }}/comments/{{ slug }}.html")
        (comment-map))
    ;; Combine layouts to form final layouts.
    (setf none-layout (render page-layout (list (cons "body" none-layout))))
    (setf list-layout (render page-layout (list (cons "body" list-layout))))
    ;; Read all comments.
    (dolist (src-path (directory src))
      (multiple-value-bind (slug comments) (read-comments src-path)
        (add-value slug comments comment-map)))
    ;; Add parameters for comment list rendering.
    (add-value "root" "../../" params)
    (add-value "blog" "blog" params)
    ;; For each post, render its comment list page.
    (dolist (post posts)
      (let* ((slug (get-value "slug" post))
             (comments (get-value slug comment-map)))
        (if (get-value slug comment-map)
            (make-comment-list post comments dst
                               list-layout item-layout params)
            (make-comment-none post dst none-layout params))))))

(defun make-text-directory (src page-layout &optional params)
  "Generate index for a directory of text files."
  (let* ((dirname (directory-basename src))
         (topic (car (last (pathname-directory dirname))))
         (title (format nil "~a Files" (string-capitalize topic)))
         (list-layout (read-file "layout/textdir/list.html"))
         (item-layout (read-file "layout/textdir/item.html"))
         (dst-path)
         (posts))
    (setf list-layout (render page-layout(list (cons "body" list-layout))))
    (add-value "root" "../" params)
    (add-value "title" title params)
    (add-value "dirname" dirname params)
    (add-value "import" "extra.css" params)
    (setf posts (read-text-files src))
    (setf dst-path (format nil "_site/~a/index.html" topic))
    (make-post-list posts dst-path list-layout item-layout params)))

(defun make-music (src page-layout &optional params)
  "Generate music list page."
  (let ((list-layout (read-file "layout/music/list.html"))
        (post-layout (read-file "layout/music/post.html"))
        (item-layout (read-file "layout/music/item.html"))
        (widget-layout (read-file "layout/music/widget.html"))
        (posts))
    ;; Combine layouts to form final layouts.
    (setf list-layout (render page-layout (list (cons "body" list-layout))))
    (setf post-layout (render page-layout (list (cons "body" post-layout))))
    ;; Callback function to be passed as a parameter to renderer.
    (defun make-widget (post)
      "Callback function to render music player widget."
      (let* ((widget-params (append post params))
             (rendered-widget (render widget-layout widget-params)))
        (list (cons "widget" rendered-widget))))
    ;; Add parameters for music post rendering.
    (add-value "import" "extra.css music.css" params)
    (add-value "root" "../" params)
    (add-value "blog" "music" params)
    (add-value "render" "yes" params)
    (add-value "callback" #'make-widget params)
    ;; Render all music posts.
    (setf posts (make-posts src "_site/music/{{ slug }}.html"
                            post-layout params))
    ;; Generate music list page.
    (add-value "root" "../" params)
    (add-value "title" "Music" params)
    (make-post-list posts "_site/music/index.html"
                    list-layout item-layout params)))

(defvar *params* nil
  "Global parameters that may be provided externally to override any
  default local parameters.")

(defun main ()
  ;; Create a new site directory from scratch.
  (remove-directory "_site/")
  (copy-directory "static/" "_site/")
  (let ((params (list (cons "root" "")
                      (cons "subtitle" " - Susam Pal")
                      (cons "author" "Susam Pal")
                      (cons "site-url" "https://susam.in/")
                      (cons "current-year"
                            (nth-value 5 (get-decoded-time)))
                      (cons "imports" "")
                      (cons "index" "")
                      (cons "maze" "/maze/")))
        (page-layout (read-file "layout/page.html"))
        (posts))
    ;; If *params* exists, merge it with local params.
    (when *params*
      (setf params (append *params* params)))
    ;; Top-level pages.
    (add-value "render" "yes" params)
    (make-posts "content/*.html" "_site/{{ slug }}.html" page-layout params)
    ;; Blog, comments, music, reading, and text directories.
    (setf posts (make-blog "content/blog/*.html" page-layout params))
    (make-comments posts "content/comments/*.html" page-layout params)
    (make-music "content/music/*.html" page-layout params)
    (make-reading "content/reading/*.html" page-layout params)
    (make-text-directory "static/security/*.txt" page-layout params))
  t)

(when *main-mode*
  (main))
