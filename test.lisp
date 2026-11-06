;;;; Tests
;;;; =====

;;;; Copyright (c) 2021-2025 Susam Pal
;;;;
;;;; You can use, copy, modify, merge, publish, distribute,
;;;; sublicense, and/or sell copies of it, under the terms of the MIT
;;;; License. See COPYRIGHT.md for complete details.
;;;;
;;;; This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; express or implied. See COPYRIGHT.md for complete details.

(require "uiop")


;;; Test Definitions
;;; ----------------

(defparameter *pass* 0)
(defparameter *fail* 0)
(defvar *quit* nil)

(defun remove-directory (path)
  "Remove the specified directory tree from the file system."
  (uiop:delete-directory-tree (pathname path) :validate t
                                              :if-does-not-exist :ignore))

(defun set-dev-path ()
  "Utility function to be run manually in development environment."
  (setf *default-pathname-defaults* (truename "~/src/susam.net/")))

(defmacro test-case (name &body body)
  "Execute a test case and print pass or fail status."
  `(progn
     (remove-directory #p"test-tmp/")
     (ensure-directories-exist #p"test-tmp/")
     (let ((test-name (string-downcase ',name)))
       (format t "~&~a: " test-name)
       (handler-case (progn ,@body)
         (:no-error (c)
           (declare (ignore c))
           (remove-directory #p"test-tmp/")
           (incf *pass*)
           (format t "pass~%")
           t)
         (error (c)
           (remove-directory #p"test-tmp/")
           (incf *fail*)
           (format t "FAIL~%")
           (format t "~&  ~a: error: ~a~%" test-name c))))))

(defmacro test-case! (name &body body)
  "Execute a test case and error out on failure. Useful for debugging."
  `(progn
     (remove-directory #p"test-tmp/")
     (ensure-directories-exist #p"test-tmp/")
     (let ((test-name (string-downcase ',name)))
       (format t "~&~a: " test-name)
       ,@body
       (incf *pass*)
       (format t "pass!~%")
       (remove-directory #p"test-tmp/"))))

(defun test-done ()
  "Print test statistics."
  (format t "~&~%PASS: ~a~%" *pass*)
  (when (plusp *fail*)
    (format t "~&FAIL: ~a~%" *fail*))
  (when *quit*
    (format t "~&~%DONE~%~%")
    (uiop:quit (if (zerop *fail*) 0 1)))
  (zerop *fail*))


;;; Begin Test Cases
;;; ----------------

(defvar *log-mode* nil)
(defvar *site-mode* nil)

(load "site.lisp")


;;; Test Cases for Reusable Definitions
;;; -----------------------------------

(test-case make-directory
  (make-directory "test-tmp/foo/bar/")
  (assert (uiop:directory-exists-p "test-tmp/foo/bar/")))

(test-case remove-directory
  (make-directory "test-tmp/foo/bar/")
  (assert (uiop:directory-exists-p "test-tmp/foo/bar/"))
  (remove-directory "test-tmp/foo/")
  (assert (not (uiop:directory-exists-p "test-tmp/foo/"))))

(test-case directory-basename
  (assert (string= (directory-basename "foo/") "foo/"))
  (assert (string= (directory-basename "foo/bar.txt") "foo/"))
  (assert (string= (directory-basename "foo/bar/") "bar/"))
  (assert (string= (directory-basename "foo/bar/baz.txt") "bar/"))
  (assert (string= (directory-basename "/foo/") "foo/"))
  (assert (string= (directory-basename "/foo/bar.txt") "foo/"))
  (assert (string= (directory-basename "/foo/bar/") "bar/"))
  (assert (string= (directory-basename "/foo/bar/baz.txt") "bar/")))

(test-case copy-directory
  (write-file "test-tmp/foo/foo.txt" "foo")
  (write-file "test-tmp/foo/bar/foo.txt" "foo")
  (write-file "test-tmp/foo/bar/bar.txt" "bar")
  (write-file "test-tmp/foo/bar/baz.txt" "baz")
  (write-file "test-tmp/foo/baz/baz.txt" "baz")
  (write-file "test-tmp/foo/baz/qux/qux.txt" "qux")
  (copy-directory "test-tmp/foo/" "test-tmp/bar/")
  (assert (string= (read-file "test-tmp/bar/foo.txt") "foo"))
  (assert (string= (read-file "test-tmp/bar/bar/foo.txt") "foo"))
  (assert (string= (read-file "test-tmp/bar/bar/bar.txt") "bar"))
  (assert (string= (read-file "test-tmp/bar/bar/baz.txt") "baz"))
  (assert (string= (read-file "test-tmp/bar/baz/baz.txt") "baz"))
  (assert (string= (read-file "test-tmp/bar/baz/qux/qux.txt") "qux")))

(test-case copy-directory-to-existing-directory
  (write-file "test-tmp/foo/foo.txt" "foo")
  (write-file "test-tmp/foo/bar/foo.txt" "foo")
  (write-file "test-tmp/foo/bar/bar.txt" "bar")
  (write-file "test-tmp/foo/bar/baz.txt" "baz")
  (write-file "test-tmp/foo/baz/baz.txt" "baz")
  (write-file "test-tmp/foo/baz/qux/qux.txt" "qux")
  (make-directory "test-tmp/bar/")
  (copy-directory "test-tmp/foo/" "test-tmp/bar/")
  (assert (string= (read-file "test-tmp/bar/foo.txt") "foo"))
  (assert (string= (read-file "test-tmp/bar/bar/foo.txt") "foo"))
  (assert (string= (read-file "test-tmp/bar/bar/bar.txt") "bar"))
  (assert (string= (read-file "test-tmp/bar/bar/baz.txt") "baz"))
  (assert (string= (read-file "test-tmp/bar/baz/baz.txt") "baz"))
  (assert (string= (read-file "test-tmp/bar/baz/qux/qux.txt") "qux")))

(test-case read-write-file-single-line
  (let ((text "foo"))
    (write-file "test-tmp/foo.txt" text)
    (assert (string= (read-file "test-tmp/foo.txt") text))))

(test-case read-write-file-multiple-lines
  (let ((text (fstr "foo~%bar~%baz~%")))
    (write-file "test-tmp/foo.txt" text)
    (assert (string= (read-file "test-tmp/foo.txt") text))))

(test-case read-write-file-nested-directories
  (write-file "test-tmp/foo/bar/baz/qux.txt" "foo")
  (assert (string= (read-file "test-tmp/foo/bar/baz/qux.txt") "foo")))

(test-case write-log
  (write-log "~a, ~a" "hello" "world"))

(test-case string-starts-with
  (assert (eq (string-starts-with "" "") t))
  (assert (eq (string-starts-with "foo" "foo") t))
  (assert (eq (string-starts-with "foo" "foobar") t))
  (assert (eq (string-starts-with "foo" "bazfoobar") nil))
  (assert (eq (string-starts-with "foo" "fo") nil))
  (assert (eq (string-starts-with "foo" "fox") nil))
  (assert (eq (string-starts-with "foo" "foO") nil)))

(test-case string-ends-with
  (assert (eq (string-ends-with "" "") t))
  (assert (eq (string-ends-with "foo" "foo") t))
  (assert (eq (string-ends-with "foo" "barfoo") t))
  (assert (eq (string-ends-with "foo" "bazfoobar") nil))
  (assert (eq (string-ends-with "foo" "oo") nil))
  (assert (eq (string-ends-with "foo" "xoo") nil))
  (assert (eq (string-ends-with "foo" "Foo") nil)))

(test-case substring-at
  (assert (eq (substring-at "" "" 0) t))
  (assert (eq (substring-at "foo" "foo" 0) t))
  (assert (eq (substring-at "foo" "foobar" 0) t))
  (assert (eq (substring-at "foo" "bazfoobar" 0) nil))
  (assert (eq (substring-at "foo" "fo" 0) nil))
  (assert (eq (substring-at "foo" "fox" 0) nil))
  (assert (eq (substring-at "foo" "foO" 0) nil))
  (assert (eq (substring-at "foo" "bazfoobar" 3) t))
  (assert (eq (substring-at "foo" "foo" 1) nil))
  (assert (eq (substring-at "foo" "bazfoobar" 2) nil)))

(test-case string-replace-empty
  (assert (string= (string-replace "" "" "") ""))
  (assert (string= (string-replace "" "x" "") "x"))
  (assert (string= (string-replace "" "bar" "") "bar"))
  (assert (string= (string-replace "" "-" "foo") "-f-o-o-"))
  (assert (string= (string-replace "" "-~" "foo") "-~f-~o-~o-~")))

(test-case string-replace-single
  (assert (string= (string-replace "foo" "foo" "foo") "foo"))
  (assert (string= (string-replace "foo" "bar" "") ""))
  (assert (string= (string-replace "foo" "bar" "foo") "bar"))
  (assert (string= (string-replace "foo" "bar" "foofoo") "barbar"))
  (assert (string= (string-replace "foo" "bar" "foo foo") "bar bar")))

(test-case string-replace-multiple
  (assert (string= (string-replace "foo" "x" "foo:foo") "x:x"))
  (assert (string= (string-replace "foo" "x" "foo:foo:") "x:x:")))

(test-case string-split
  (assert (not (string-split "" ":")))
  (assert (not (string-split "" ": ")))
  (assert (equal (string-split "foo" ":") '("foo")))
  (assert (equal (string-split "foo:" ":") '("foo")))
  (assert (equal (string-split "foo:bar" ":") '("foo" "bar")))
  (assert (equal (string-split "foo:bar" ":") '("foo" "bar")))
  (assert (equal (string-split "foo" ": ") '("foo")))
  (assert (equal (string-split "foo:" ": ") '("foo:")))
  (assert (equal (string-split "foo: " ": ") '("foo")))
  (assert (equal (string-split "foo:bar" ": ") '("foo:bar")))
  (assert (equal (string-split "foo: bar" ": ") '("foo" "bar")))
  (assert (equal (string-split "foo: bar: baz" ": ") '("foo" "bar" "baz")))
  (assert (equal (string-split "foo: bar: baz: " ": ") '("foo" "bar" "baz")))
  (assert (equal (string-split "foo: bar: : baz: " ": ") '("foo" "bar" "" "baz")))
  (assert (equal (string-split "foo::bar:::baz:" ":") '("foo" ""  "bar" "" "" "baz"))))

(test-case string-split-empty-fields
  (assert (not (string-split "" ":")))
  (assert (not (string-split "" ": ")))
  (assert (equal (string-split "foo" ":") '("foo")))
  (assert (equal (string-split "foo:" ":") '("foo")))
  (assert (equal (string-split "foo:bar" ":") '("foo" "bar")))
  (assert (equal (string-split "foo:bar" ":") '("foo" "bar")))
  (assert (equal (string-split "foo" ": ") '("foo")))
  (assert (equal (string-split "foo:" ": ") '("foo:")))
  (assert (equal (string-split "foo: " ": ") '("foo")))
  (assert (equal (string-split "foo:bar" ": ") '("foo:bar")))
  (assert (equal (string-split "foo: bar" ": ") '("foo" "bar")))
  (assert (equal (string-split "foo: bar: baz" ": ") '("foo" "bar" "baz")))
  (assert (equal (string-split "foo: bar: baz: " ": ") '("foo" "bar" "baz")))
  (assert (equal (string-split "foo: bar: : baz: " ": ") '("foo" "bar" "" "baz")))
  (assert (equal (string-split "foo::bar:::baz:" ":") '("foo" "" "bar" "" "" "baz"))))

(test-case join-string-list
  (assert (string= (join-string-list '()) ""))
  (assert (string= (join-string-list '("")) ""))
  (assert (string= (join-string-list '("" "")) ""))
  (assert (string= (join-string-list '("foo")) "foo"))
  (assert (string= (join-string-list '("foo" "")) "foo"))
  (assert (string= (join-string-list '("foo" "bar")) "foobar"))
  (assert (string= (join-string-list '("foo" "bar" "baz")) "foobarbaz")))

(test-case repeat-string
  (assert (string= (repeat-string 0 "") ""))
  (assert (string= (repeat-string 1 "") ""))
  (assert (string= (repeat-string 2 "") ""))
  (assert (string= (repeat-string 3 "") ""))
  (assert (string= (repeat-string 0 "foo") ""))
  (assert (string= (repeat-string 1 "foo") "foo"))
  (assert (string= (repeat-string 2 "foo") "foofoo"))
  (assert (string= (repeat-string 3 "foo") "foofoofoo")))

(test-case indent-lines
  (assert (string= (indent-lines 0 "") ""))
  (assert (string= (indent-lines 4 "") ""))
  (assert (string= (indent-lines 0 "x") (fstr "x~%")))
  (assert (string= (indent-lines 1 "x") (fstr " x~%")))
  (assert (string= (indent-lines 2 "x") (fstr "  x~%")))
  (assert (string= (indent-lines 4 "x") (fstr "    x~%")))
  (assert (string= (indent-lines 4 (fstr "x~%")) (fstr "    x~%")))
  (assert (string= (indent-lines 4 (fstr "x~%y~%")) (fstr "    x~%    y~%"))))

(test-case xml-escape
  (assert (string= (xml-escape "<foo>") "&lt;foo&gt;"))
  (assert (string= (xml-escape "\"foo'") "&quot;foo&apos;"))
  (assert (string= (xml-escape "foo & bar") "foo &amp; bar"))
  (assert (string= (xml-escape "foo & bar") "foo &amp; bar")))

(test-case parse-tz
  (assert (= (parse-tz "+0000") 0))
  (assert (= (parse-tz "+0100") -1))
  (assert (= (parse-tz "+0530") -11/2))
  (assert (= (parse-tz "-0500") 5)))

(test-case iso-date
  (assert (string= (iso-date "2020-06-01 09:00:10 +0000") "2020-06-01T09:00:10Z"))
  (assert (string= (iso-date "2020-06-01 10:00:10 +0100") "2020-06-01T09:00:10Z"))
  (assert (string= (iso-date "2020-06-01 14:30:10 +0530") "2020-06-01T09:00:10Z"))
  (assert (string= (iso-date "2020-06-01 04:00:10 -0500") "2020-06-01T09:00:10Z")))


;;; Test Cases for Tool Definitions
;;; -------------------------------

(test-case read-header-line
  (let ((text (fstr "<!-- k1: v1 -->~%")))
    (assert (equal (multiple-value-list (read-header-line text 0)) '("k1" "v1" 16)))
    (assert (equal (multiple-value-list (read-header-line text 16)) '(nil nil 16)))))

(test-case read-header-line-empty-value
  (let ((text (fstr "<!-- k1:  -->~%")))
    (assert (equal (multiple-value-list (read-header-line text 0)) '("k1" "" 14)))
    (assert (equal (multiple-value-list (read-header-line text 14)) '(nil nil 14)))))

(test-case read-header-lines
  (let ((text (fstr "<!-- k1: v1 -->~%<!-- k2: v2 -->~%body")))
    (assert (equal (multiple-value-list (read-header-line text 0)) '("k1" "v1" 16)))
    (assert (equal (multiple-value-list (read-header-line text 16)) '("k2" "v2" 32)))
    (assert (equal (multiple-value-list (read-header-line text 32)) '(nil nil 32)))))

(test-case read-headers
  (let ((text (fstr "<!-- k1: v1 -->~%<!-- k2: v2 -->~%body")))
    (assert (equal (multiple-value-list (read-headers text 0))
                   '((("k2" . "v2") ("k1" . "v1")) 32))))
  (let ((text (fstr "<!-- k1: v1 -->~%<!-- k2: v2 -->~%body")))
    (assert (equal (multiple-value-list (read-headers text 16))
                   '((("k2" . "v2")) 32))))
  (assert (equal (multiple-value-list (read-headers "body" 0)) '(nil 0))))

(test-case read-multi-content
  (let* ((nl (fstr "~%"))
         (text (join-strings "<!-- title: Discussion -->" nl
                             "<p>Intro</p>" nl
                             "<!-- date: 2020-06-01 09:00:00 +0000 -->" nl
                             "<!-- name: Alice -->" nl "first" nl
                             "<!-- date: 2020-06-02 09:00:00 +0000 -->" nl
                             "<!-- name: Bob -->" nl "second"))
         (doc (read-multi-content text))
         (blocks (aget "blocks" doc)))
    (assert (string= (aget "title" doc) "Discussion"))
    (assert (string= (aget "body" doc) (fstr "<p>Intro</p>~%")))
    (assert (string= (aget "date" doc) "2020-06-01 09:00:00 +0000"))
    (assert (equal (mapcar (lambda (blk) (aget "name" blk)) blocks)
                   '("Alice" "Bob")))))

(test-case weekday-name
  (assert (string= (weekday-name 0) "Mon"))
  (assert (string= (weekday-name 1) "Tue"))
  (assert (string= (weekday-name 2) "Wed"))
  (assert (string= (weekday-name 3) "Thu"))
  (assert (string= (weekday-name 4) "Fri"))
  (assert (string= (weekday-name 5) "Sat"))
  (assert (string= (weekday-name 6) "Sun")))

(test-case month-name
  (assert (string= (month-name 1) "Jan"))
  (assert (string= (month-name 2) "Feb"))
  (assert (string= (month-name 3) "Mar"))
  (assert (string= (month-name 4) "Apr"))
  (assert (string= (month-name 5) "May"))
  (assert (string= (month-name 6) "Jun"))
  (assert (string= (month-name 7) "Jul"))
  (assert (string= (month-name 8) "Aug"))
  (assert (string= (month-name 9) "Sep"))
  (assert (string= (month-name 10) "Oct"))
  (assert (string= (month-name 11) "Nov"))
  (assert (string= (month-name 12) "Dec")))

(test-case format-iso-date
  (flet ((f (x) (format-iso-date (parse-content-date x))))
    (assert (string= (f "2020-06-01") "2020-06-01T00:00:00Z"))
    (assert (string= (f "2020-06-01 09:00") "2020-06-01T09:00:00Z"))
    (assert (string= (f "2020-06-01 09:00:10") "2020-06-01T09:00:10Z"))
    (assert (string= (f "2020-06-01 14:30:10 +0530") "2020-06-01T09:00:10Z"))
    (assert (string= (f "2020-06-01 04:30:10 +0530") "2020-05-31T23:00:10Z"))))

(test-case format-short-date
  (flet ((f (x) (format-short-date (parse-content-date x))))
    (assert (string= (f "2020-06-01") "01 Jun 2020"))
    (assert (string= (f "2020-06-01 09:00") "01 Jun 2020"))
    (assert (string= (f "2020-06-01 14:30:10 +0530") "01 Jun 2020"))
    (assert (string= (f "2020-06-01 04:30:10 +0530") "31 May 2020"))))

(test-case format-long-date
  (flet ((f (x) (format-long-date (parse-content-date x))))
    (assert (string= (f "2020-06-01") "01 Jun 2020 00:00 UTC"))
    (assert (string= (f "2020-06-01 14:30:10 +0530") "01 Jun 2020 09:00 UTC"))
    (assert (string= (f "2020-06-01 04:30:10 +0530") "31 May 2020 23:00 UTC"))))

(test-case format-long-date-sep
  (flet ((f (x) (format-long-date (parse-content-date x) " at ")))
    (assert (string= (f "2020-06-01") "01 Jun 2020 at 00:00 UTC"))
    (assert (string= (f "2020-06-01 14:30:10 +0530") "01 Jun 2020 at 09:00 UTC"))
    (assert (string= (f "2020-06-01 04:30:10 +0530") "31 May 2020 at 23:00 UTC"))))

(test-case doc-path
  (assert (string= (doc-path "foo/bar/baz.page.html" "foo/bar/") "baz.html"))
  (assert (string= (doc-path "foo/bar/baz.qux.page.html" "foo/bar/") "baz.qux.html"))
  (assert (string= (doc-path "foo/bar/baz/qux.page.html" "foo/") "bar/baz/qux.html"))
  (assert (string= (doc-path "foo/bar/baz/qux.page.html" "foo/bar/") "baz/qux.html"))
  (assert (string= (doc-path "foo/bar/baz/qux.page.html" "foo/bar/baz/") "qux.html")))

(test-case aput
  (let ((alist))
    (aput "a" "apple" alist)
    (string= (aget "a" alist) "apple")
    (assert (not (aget "b" alist)))))

(test-case aput-multiple
  (let ((alist))
    (aput "a" "apple" alist)
    (aput "b" "ball" alist)
    (aput "c" "cat" alist)
    (assert (string= (aget "a" alist) "apple"))
    (assert (string= (aget "b" alist) "ball"))
    (assert (string= (aget "c" alist) "cat"))))

(test-case aput-new-overrides-old
  (let ((alist))
    (aput "a" "apple" alist)
    (aput "a" "ant" alist)
    (assert (string= (aget "a" alist) "ant"))))

(test-case aput-list
  (let ((alist))
    (aput-list "a" "apple" alist)
    (aput-list "a" "axe" alist)
    (aput-list "b" "ball" alist)
    (aput-list "c" "cat" alist)
    (aput-list "a" "ant" alist)
    (aput-list "b" "bag" alist)
    (assert (equal (aget "a" alist) '("ant" "axe" "apple")))
    (assert (equal (aget "b" alist) '("bag" "ball")))
    (assert (equal (aget "c" alist) '("cat")))))

(test-case last-n
  (let ((seq '(10 20 30 40 50)))
    (assert (not (last-n 0 seq)))
    (assert (equal (last-n 1 seq) '(50)))
    (assert (equal (last-n 2 seq) '(40 50)))
    (assert (equal (last-n 3 seq) '(30 40 50)))
    (assert (equal (last-n 4 seq) '(20 30 40 50)))
    (assert (equal (last-n 5 seq) '(10 20 30 40 50)))
    (assert (equal (last-n 6 seq) '(10 20 30 40 50)))
    (assert (equal (last-n 10 seq) '(10 20 30 40 50)))))

(test-case refine-headings
  (assert (string= (refine-headings "") ""))
  (assert (string= (refine-headings "foo") "foo"))
  (assert (string= (refine-headings "<h1>Foo</h1>") "<h1>Foo</h1>"))
  (assert (string= (refine-headings "<hx id=\"foo\">Foo</hx>")
                   "<hx id=\"foo\">Foo</hx>"))
  (assert (string= (refine-headings "<h1 id=\"foo\">Foo</h1>")
                   "<h1 id=\"foo\">Foo<a href=\"#foo\"></a></h1>"))
  (assert (string= (refine-headings "begin<h1 id=\"foo\">Foo</h1>end")
                   "begin<h1 id=\"foo\">Foo<a href=\"#foo\"></a></h1>end"))
  (assert (string= (refine-headings "Hello
<h1 id=\"foo\">Foo</h1>
<h2 id=\"bar\">Bar</h2>")
                   "Hello
<h1 id=\"foo\">Foo<a href=\"#foo\"></a></h1>
<h2 id=\"bar\">Bar<a href=\"#bar\"></a></h2>")))

(test-case refine-headings-format-control-in-text
  (assert (string= (refine-headings "~a") "~a")))

(test-case toc-html-none
  (let ((toc (fstr "<h2 id=\"contents\">Contents</h2>~%")))
    (assert (string= (toc-html "" nil) toc))
    (assert (string= (toc-html "foo bar" nil) toc))
    (assert (string= (toc-html "<h2>H2</h2>" nil) toc))))

(test-case tocn-html-none
  (let ((toc (fstr "<h2 id=\"contents\">Contents</h2>~%")))
    (assert (string= (toc-html "" t) toc))
    (assert (string= (toc-html "foo bar" t) toc))
    (assert (string= (toc-html "<h2>H2</h2>" t) toc))))

(test-case toc-html-single
  (let ((body "<h2 id=\"h2\">H2</h2>")
        (toc "<h2 id=\"contents\">Contents</h2>
<ul>
  <li><a href=\"#h2\">H2</a></li>
</ul>"))
    (assert (string= (toc-html body nil) toc))))

(test-case tocn-html-single
  (let ((body "<h2 id=\"h2\">H2</h2>")
        (toc "<h2 id=\"contents\">Contents</h2>
<ol>
  <li><a href=\"#h2\">H2</a></li>
</ol>"))
    (assert (string= (toc-html body t) toc))))

(test-case toc-html-multiple
  (let ((body "<h2 id=\"h2\">H2</h2>

Foo

<h2 id=\"h2\">H2</h2>
<h3 id=\"h3\">H3</h3>
<h3 id=\"h3\">H3</h3>
<h4 id=\"h4\">H4</h4>
<h5 id=\"h5\">H5</h5>
<h2 id=\"h2\">H2</h2>")
        (toc "<h2 id=\"contents\">Contents</h2>
<ul>
  <li><a href=\"#h2\">H2</a></li>
  <li><a href=\"#h2\">H2</a>
    <ul>
      <li><a href=\"#h3\">H3</a></li>
      <li><a href=\"#h3\">H3</a>
        <ul>
          <li><a href=\"#h4\">H4</a>
            <ul>
              <li><a href=\"#h5\">H5</a></li>
            </ul>
          </li>
        </ul>
      </li>
    </ul>
  </li>
  <li><a href=\"#h2\">H2</a></li>
</ul>"))
    (assert (string= (toc-html body nil) toc))))

(test-case tocn-html-multiple
  (let ((body "<h2 id=\"h2\">H2</h2>

Foo

<h2 id=\"h2\">H2</h2>
<h3 id=\"h3\">H3</h3>
<h3 id=\"h3\">H3</h3>
<h4 id=\"h4\">H4</h4>
<h5 id=\"h5\">H5</h5>
<h2 id=\"h2\">H2</h2>")
        (toc "<h2 id=\"contents\">Contents</h2>
<ol>
  <li><a href=\"#h2\">H2</a></li>
  <li><a href=\"#h2\">H2</a>
    <ol type=\"a\">
      <li><a href=\"#h3\">H3</a></li>
      <li><a href=\"#h3\">H3</a>
        <ol type=\"i\">
          <li><a href=\"#h4\">H4</a>
            <ol type=\"a\">
              <li><a href=\"#h5\">H5</a></li>
            </ol>
          </li>
        </ol>
      </li>
    </ol>
  </li>
  <li><a href=\"#h2\">H2</a></li>
</ol>"))
    (assert (string= (toc-html body t) toc))))

(test-case toc-html-deep-end
  (let ((body "<h2 id=\"h2\">H2</h2>
<h3 id=\"h3\">H3</h3>
<h4 id=\"h4\">H4</h4>")
        (toc "<h2 id=\"contents\">Contents</h2>
<ul>
  <li><a href=\"#h2\">H2</a>
    <ul>
      <li><a href=\"#h3\">H3</a>
        <ul>
          <li><a href=\"#h4\">H4</a></li>
        </ul>
      </li>
    </ul>
  </li>
</ul>"))
    (assert (string= (toc-html body nil) toc))))

(test-case tocn-html-deep-end
  (let ((body "<h2 id=\"h2\">H2</h2>
<h3 id=\"h3\">H3</h3>
<h4 id=\"h4\">H4</h4>")
        (toc "<h2 id=\"contents\">Contents</h2>
<ol>
  <li><a href=\"#h2\">H2</a>
    <ol type=\"a\">
      <li><a href=\"#h3\">H3</a>
        <ol type=\"i\">
          <li><a href=\"#h4\">H4</a></li>
        </ol>
      </li>
    </ol>
  </li>
</ol>"))
    (assert (string= (toc-html body t) toc))))

(test-case render
  (let* ((template "Foo {{ var-x }} Baz {{ var-y }} Quux")
         (params '(("var-x" . "Bar") ("var-y" . "Qux")))
         (result (render template params)))
    (assert (string= result "Foo Bar Baz Qux Quux"))))

(test-case render-no-param
  (assert (string= (render "Foo" nil) "Foo")))

(test-case render-one-param
  (let* ((template "{{ var-x }}")
         (params '(("var-x" . "Bar"))))
    (assert (string= (render template params) "Bar"))))

(test-case render-trailing-param
  (let* ((template "Foo {{ var-x }}")
         (params '(("var-x" . "Bar"))))
    (assert (string= (render template params) "Foo Bar"))))

(test-case render-missing-param-intact
  (let* ((template "Foo {{ var-x }}"))
    (assert (string= (render template nil) "Foo {{ var-x }}"))))

(test-case render-good-param-and-missing-param
  (let* ((template "Foo {{ var-x }} {{ var-y }}")
         (params '(("var-x" . "Bar"))))
    (assert (string= (render template params) "Foo Bar {{ var-y }}"))))

(test-case render-extra-params-ignored
  (let* ((template "Foo {{ var-x }}")
         (params '(("var-x" . "Bar") ("var-y" . "Baz"))))
    (assert (string= (render template params) "Foo Bar"))))

(test-case render-head-html-css
  (let ((s "  <link rel=\"stylesheet\" href=\"css/foo.css\">~%"))
    (assert (string= (head-html '("foo.css") "" nil "no")
                     (fstr s)))))

(test-case render-head-html-js
  (assert (string= (head-html '("foo.js") "" nil "no")
                   (fstr "  <script src=\"js/foo.js\"></script>~%"))))

(test-case render-head-html-css-root
  (let ((s "  <link rel=\"stylesheet\" href=\"../css/foo.css\">~%"))
    (assert (string= (head-html '("foo.css") "../" nil "no")
                     (fstr s)))))

(test-case render-head-html-js-root
  (let ((s "  <script src=\"../js/foo.js\"></script>~%"))
    (assert (string= (head-html '("foo.js") "../" nil "no")
                     (fstr s)))))

(test-case render-head-html-js-css
  (assert (string=
           (head-html '("foo.css" "bar.js" "baz.css" "qux.js") "" nil "no")
           "  <link rel=\"stylesheet\" href=\"css/foo.css\">
  <script src=\"js/bar.js\"></script>
  <link rel=\"stylesheet\" href=\"css/baz.css\">
  <script src=\"js/qux.js\"></script>
")))

(test-case fill-layout-simple
  (let* ((layouts '(("page" . (("body" . "begin-page {{ body }} end-page")))
                    ("post" . (("body" . "begin-post {{ body }} end-post")
                               ("layout" . "page")))))
         (post (assoc "post" layouts :test #'string=))
         (filled (fill-layout post layouts)))
    (assert (equal filled
                   (cons "post" "begin-page begin-post {{ body }} end-post end-page")))))

(test-case fill-layout-nested
  (let* ((layouts '(("page" . (("body" . "begin-page {{ body }} end-page")))
                    ("post" . (("body" . "begin-post {{ body }} end-post")
                               ("layout" . "page")))
                    ("nest" . (("body" . "begin-nest {{ body }} end-nest")
                               ("layout" . "post")))))
         (nest (assoc "nest" layouts :test #'string=))
         (filled (fill-layout nest layouts)))
    (assert (equal filled
                   (cons "nest"
                         (join-strings "begin-page begin-post begin-nest"
                                       " {{ body }} "
                                       "end-nest end-post end-page"))))))

(test-case root-path
  (assert (string= (root-path "") "./"))
  (assert (string= (root-path "index.html") "./"))
  (assert (string= (root-path "foo") "./"))
  (assert (string= (root-path "foo/") "../"))
  (assert (string= (root-path "foo/index.html") "../"))
  (assert (string= (root-path "foo/bar") "../"))
  (assert (string= (root-path "foo/bar/") "../../"))
  (assert (string= (root-path "foo/bar/index.html") "../../")))

;; End test cases.

(test-done)
