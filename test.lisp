;;;; Tests
;;;; =====

;;;; Copyright (c) 2021-2022 Susam Pal
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
           (incf *pass*)
           (format t "pass~%"))
         (error (c)
           (incf *fail*)
           (format t "FAIL~%")
           (format t "~&  ~a: error: ~a~%" test-name c)))
       (remove-directory #p"test-tmp/"))))

(defmacro test-case! (name &body body)
  "Execute a test case and error out on failure."
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
    (format t "~&~%quitting ...~%~%")
    (uiop:quit (if (zerop *fail*) 0 1))))


;;; Begin Test Cases
;;; ----------------

(defvar *log-mode* nil)
(defvar *main-mode* nil)
(load "site.lisp")


;;; Test Cases for Reusable Definitions
;;; -----------------------------------

(test-case make-directory
  (make-directory "test-tmp/foo/bar/")
  (assert (directory-exists-p "test-tmp/foo/bar/")))

(test-case remove-directory
  (make-directory "test-tmp/foo/bar/")
  (assert (directory-exists-p "test-tmp/foo/bar/"))
  (remove-directory "test-tmp/foo/")
  (assert (not (directory-exists-p "test-tmp/foo/"))))

(test-case directory-name
  (assert (string= (directory-name "") ""))
  (assert (string= (directory-name "foo") ""))
  (assert (string= (directory-name "foo/") "foo/"))
  (assert (string= (directory-name "foo/bar.txt") "foo/"))
  (assert (string= (directory-name "foo/bar/") "foo/bar/"))
  (assert (string= (directory-name "foo/bar/baz.txt") "foo/bar/"))
  (assert (string= (directory-name "/") "/"))
  (assert (string= (directory-name "/foo") "/"))
  (assert (string= (directory-name "/foo/") "/foo/"))
  (assert (string= (directory-name "/foo/bar.txt") "/foo/"))
  (assert (string= (directory-name "/foo/bar/") "/foo/bar/"))
  (assert (string= (directory-name "/foo/bar/baz.txt") "/foo/bar/")))

(test-case directory-basename
  (assert (string= (directory-basename "foo/") "foo/"))
  (assert (string= (directory-basename "foo/bar.txt") "foo/"))
  (assert (string= (directory-basename "foo/bar/") "bar/"))
  (assert (string= (directory-basename "foo/bar/baz.txt") "bar/"))
  (assert (string= (directory-basename "/foo/") "foo/"))
  (assert (string= (directory-basename "/foo/bar.txt") "foo/"))
  (assert (string= (directory-basename "/foo/bar/") "bar/"))
  (assert (string= (directory-basename "/foo/bar/baz.txt") "bar/")))

(test-case copy-file-to-file
  (write-file "test-tmp/foo.txt" "foo")
  (copy-file "test-tmp/foo.txt" "test-tmp/bar.txt")
  (assert (string= (read-file "test-tmp/bar.txt") "foo")))

(test-case copy-file-to-dir
  (write-file "test-tmp/foo/foo.txt" "foo")
  (make-directory "test-tmp/bar/")
  (copy-file "test-tmp/foo/foo.txt" "test-tmp/bar/")
  (assert (string= (read-file "test-tmp/bar/foo.txt") "foo")))

(test-case copy-files-to-dir
  (write-file "test-tmp/foo/foo.txt" "foo")
  (write-file "test-tmp/foo/bar.txt" "bar")
  (write-file "test-tmp/foo/baz.txt" "baz")
  (make-directory "test-tmp/bar/")
  (copy-files "test-tmp/foo/*.txt" "test-tmp/bar/")
  (assert (string= (read-file "test-tmp/bar/foo.txt") "foo"))
  (assert (string= (read-file "test-tmp/bar/bar.txt") "bar"))
  (assert (string= (read-file "test-tmp/bar/baz.txt") "baz")))

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
  (let ((text (format nil "foo~%bar~%baz~%")))
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

(test-case join-strings
  (assert (string= (join-strings '()) ""))
  (assert (string= (join-strings '("")) ""))
  (assert (string= (join-strings '("" "")) ""))
  (assert (string= (join-strings '("foo")) "foo"))
  (assert (string= (join-strings '("foo" "")) "foo"))
  (assert (string= (join-strings '("foo" "bar")) "foobar"))
  (assert (string= (join-strings '("foo" "bar" "baz")) "foobarbaz")))

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


;;; Test Cases for Tool Definitions
;;; -------------------------------

(test-case read-header-line
  (let ((text (format nil "<!-- k1: v1 -->~%")))
    (multiple-value-bind (k v next-index) (read-header-line text 0)
      (assert (string= k "k1"))
      (assert (string= v "v1"))
      (assert (= next-index 16)))
    (multiple-value-bind (k v next-index) (read-header-line text 16)
      (assert (not k))
      (assert (not v))
      (assert (= next-index 16)))))

(test-case read-header-line-empty-value
  (let ((text (format nil "<!-- k1:  -->~%")))
    (multiple-value-bind (k v next-index) (read-header-line text 0)
      (assert (string= k "k1"))
      (assert (string= v ""))
      (assert (= next-index 14)))
    (multiple-value-bind (k v next-index) (read-header-line text 14)
      (assert (not k))
      (assert (not v))
      (assert (= next-index 14)))))

(test-case read-header-lines
  (let ((text (format nil "<!-- k1: v1 -->~%<!-- k2: v2 -->~%body")))
    (multiple-value-bind (k v next-index) (read-header-line text 0)
      (assert (string= k "k1"))
      (assert (string= v "v1"))
      (assert (= next-index 16)))
    (multiple-value-bind (k v next-index) (read-header-line text 16)
      (assert (string= k "k2"))
      (assert (string= v "v2"))
      (assert (= next-index 32)))
    (multiple-value-bind (k v next-index) (read-header-line text 32)
      (assert (not k))
      (assert (not v))
      (assert (= next-index 32)))))

(test-case read-headers
  (let ((text (format nil "<!-- k1: v1 -->~%<!-- k2: v2 -->~%body")))
    (multiple-value-bind (headers next-index) (read-headers text 0)
      (assert (equal headers (list (cons "k2" "v2") (cons "k1" "v1"))))
      (assert (= next-index 32))))
  (let ((text (format nil "<!-- k1: v1 -->~%<!-- k2: v2 -->~%body")))
    (multiple-value-bind (headers next-index) (read-headers text 16)
      (assert (equal headers (list (cons "k2" "v2"))))
      (assert (= next-index 32))))
  (let ((text "body"))
    (multiple-value-bind (headers next-index) (read-headers text 0)
      (assert (equal headers nil))
      (assert (= next-index 0)))))

(test-case read-sections-empty-string
  (assert (not (read-sections ""))))

(test-case read-sections-none
  (assert (not (read-sections "Foo"))))

(test-case read-sections-single
  (let* ((body (format nil "<!-- foo -->~%Foo"))
         (result (read-sections body)))
    (assert (equal (aget "foo" result) (list "Foo")))))

(test-case read-sections-multiple
  (let* ((body (format nil "<!-- foo -->~%Foo<!-- bar -->~%Bar"))
         (result (read-sections body)))
    (assert (equal (aget "foo" result) (list "Foo")))
    (assert (equal (aget "bar" result) (list "Bar")))))

(test-case read-sections-repeating-section-names
  (let* ((body (format nil "<!-- a -->~%A<!-- b -->~%B<!-- a -->~%C"))
         (result (read-sections body)))
    (assert (equal (aget "a" result) (list "A" "C")))
    (assert (equal (aget "b" result) (list "B")))))

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

(test-case decode-weekday-name
  (assert (string= (decode-weekday-name 2019 01 07) "Mon"))
  (assert (string= (decode-weekday-name 2019 03 05) "Tue"))
  (assert (string= (decode-weekday-name 2020 01 01) "Wed"))
  (assert (string= (decode-weekday-name 2020 02 27) "Thu"))
  (assert (string= (decode-weekday-name 2020 02 28) "Fri"))
  (assert (string= (decode-weekday-name 2020 02 29) "Sat"))
  (assert (string= (decode-weekday-name 2020 03 01) "Sun")))

(test-case rss-date
  (assert (string= (rss-date "2020-06-01")
                   "Mon, 01 Jun 2020 00:00:00 +0000"))
  (assert (string= (rss-date "2020-06-01 17:30")
                   "Mon, 01 Jun 2020 17:30:00 +0000"))
  (assert (string= (rss-date "2020-06-01 17:30:10")
                   "Mon, 01 Jun 2020 17:30:10 +0000"))
  (assert (string= (rss-date "2020-06-01 17:30:10 +0530")
                   "Mon, 01 Jun 2020 17:30:10 +0530"))
  (assert (string= (rss-date "2020-06-01 17:30:10 IST")
                   "Mon, 01 Jun 2020 17:30:10 IST")))

(test-case simple-date
  (assert (string= (simple-date "2020-06-01")
                   "01 Jun 2020"))
  (assert (string= (simple-date "2020-06-01 17:30")
                   "01 Jun 2020 17:30"))
  (assert (string= (simple-date "2020-06-01 17:30:10 +0000")
                   "01 Jun 2020 17:30 GMT"))
  (assert (string= (simple-date "2020-06-01 17:30:10 GMT")
                   "01 Jun 2020 17:30 GMT"))
  (assert (string= (simple-date "2020-06-01 17:30:10 +0530")
                   "01 Jun 2020 17:30 +0530"))
  (assert (string= (simple-date "2020-06-01 17:30:10 IST")
                   "01 Jun 2020 17:30 IST"))
  (assert (string= (simple-date "2020-06-01 17:30:10 IST" :sep "at ")
                   "01 Jun 2020 at 17:30 IST")))

(test-case date-slug
  (multiple-value-bind (date slug) (date-slug "foo")
    (assert (not date))
    (assert (string= slug "foo")))
  (multiple-value-bind (date slug) (date-slug "foo-bar.html")
    (assert (not date))
    (assert (string= slug "foo-bar")))
  (multiple-value-bind (date slug) (date-slug "/foo-bar.html")
    (assert (not date))
    (assert (string= slug "foo-bar")))
  (multiple-value-bind (date slug) (date-slug "2020-06-01-foo.html")
    (assert (string= date "2020-06-01"))
    (assert (string= slug "foo")))
  (multiple-value-bind (date slug) (date-slug "/2020-06-01-foo.html")
    (assert (string= date "2020-06-01"))
    (assert (string= slug "foo"))))

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
    (assert (equal (aget "a" alist) (list "ant" "axe" "apple")))
    (assert (equal (aget "b" alist) (list "bag" "ball")))
    (assert (equal (aget "c" alist) (list "cat")))))

(test-case reverse-list-values-in-alist-nil
  (assert (not (reverse-list-values-in-alist nil))))

(test-case reverse-list-values-in-alist
  (assert (equal
           (reverse-list-values-in-alist
            (list (cons "a" (list "val1" "val2" "val3"))
                  (cons "b" (list "val1" "val2"))
                  (cons "c" (list "val1"))
                  (cons "d" '())))
           (list (cons "a" (list "val3" "val2" "val1"))
                 (cons "b" (list "val2" "val1"))
                 (cons "c" (list "val1"))
                 (cons "d" '())))))

(test-case extra-markup
  (assert (string= (extra-markup "") ""))
  (assert (string= (extra-markup "foo") "foo"))
  (assert (string= (extra-markup "<h1>Foo</h1>") "<h1>Foo</h1>"))
  (assert (string= (extra-markup "<hx id=\"foo\">Foo</hx>")
                   "<hx id=\"foo\">Foo</hx>"))
  (assert (string= (extra-markup "<h1 id=\"foo\">Foo</h1>")
                   "<h1 id=\"foo\">Foo<a href=\"#foo\"></a></h1>"))
  (assert (string= (extra-markup "begin<h1 id=\"foo\">Foo</h1>end")
                   "begin<h1 id=\"foo\">Foo<a href=\"#foo\"></a></h1>end"))
  (assert (string= (extra-markup "Hello
<h1 id=\"foo\">Foo</h1>
<h2 id=\"bar\">Bar</h2>")
                   "Hello
<h1 id=\"foo\">Foo<a href=\"#foo\"></a></h1>
<h2 id=\"bar\">Bar<a href=\"#bar\"></a></h2>")))

(test-case extra-markup-format-control-in-text
  (assert (string= (extra-markup "~a") "~a")))

(test-case read-post
  (write-file "test-tmp/2020-06-01-quux-quuz.html"
              (format nil "<!-- title: Foo Bar -->~%Baz Qux"))
  (let ((post (read-post "test-tmp/2020-06-01-quux-quuz.html")))
    (assert (string= (aget "date" post) "2020-06-01"))
    (assert (string= (aget "slug" post) "quux-quuz"))
    (assert (string= (aget "title" post) "Foo Bar"))
    (assert (string= (aget "body" post) "Baz Qux"))
    (assert (string= (aget "rss-date" post) "Mon, 01 Jun 2020 00:00:00 +0000"))
    (assert (string= (aget "simple-date" post) "01 Jun 2020"))))

(test-case read-post-without-date
  (write-file "test-tmp/quux-quuz.html" "Baz Qux")
  (let ((params (read-post "test-tmp/quux-quuz.html")))
    (assert (eq (aget "date" params) nil))
    (assert (string= (aget "slug" params) "quux-quuz"))
    (assert (string= (aget "body" params) "Baz Qux"))))

(test-case read-post-date-in-filename-only
  (write-file "test-tmp/2020-06-01-quux-quuz.html" "Baz Qux")
  (let ((params (read-post "test-tmp/2020-06-01-quux-quuz.html")))
    (assert (string= (aget "date" params) "2020-06-01"))
    (assert (string= (aget "slug" params) "quux-quuz"))
    (assert (string= (aget "body" params) "Baz Qux"))))

(test-case read-post-date-in-header-only
  (write-file "test-tmp/quux-quuz.html"
              (format nil "<!-- date: 2020-06-02 -->~%Baz Qux"))
  (let ((params (read-post "test-tmp/quux-quuz.html")))
    (assert (string= (aget "date" params) "2020-06-02"))
    (assert (string= (aget "slug" params) "quux-quuz"))
    (assert (string= (aget "body" params) "Baz Qux"))))

(test-case read-post-date-in-filename-and-header
  (write-file "test-tmp/2020-06-01-quux-quuz.html"
              (format nil "<!-- date: 2020-06-02 -->~%Baz Qux"))
  (let ((params (read-post "test-tmp/2020-06-01-quux-quuz.html")))
    (assert (string= (aget "date" params) "2020-06-02"))
    (assert (string= (aget "slug" params) "quux-quuz"))
    (assert (string= (aget "body" params) "Baz Qux"))))

(test-case render
  (let* ((template "Foo {{ var-x }} Baz {{ var-y }} Quux")
         (params (list (cons "var-x" "Bar") (cons "var-y" "Qux")))
         (result (render template params)))
    (assert (string= result "Foo Bar Baz Qux Quux"))))

(test-case render-no-param
  (assert (string= (render "Foo" nil) "Foo")))

(test-case render-one-param
  (let* ((template "{{ var-x }}")
         (params (list (cons "var-x" "Bar"))))
    (assert (string= (render template params) "Bar"))))

(test-case render-trailing-param
  (let* ((template "Foo {{ var-x }}")
         (params (list (cons "var-x" "Bar"))))
    (assert (string= (render template params) "Foo Bar"))))

(test-case render-missing-param-intact
  (let* ((template "Foo {{ var-x }}"))
    (assert (string= (render template nil) "Foo {{ var-x }}"))))

(test-case render-good-param-and-missing-param
  (let* ((template "Foo {{ var-x }} {{ var-y }}")
         (params (list (cons "var-x" "Bar"))))
    (assert (string= (render template params) "Foo Bar {{ var-y }}"))))

(test-case render-extra-params-ignored
  (let* ((template "Foo {{ var-x }}")
         (params (list (cons "var-x" "Bar") (cons "var-y" "Baz"))))
    (assert (string= (render template params) "Foo Bar"))))

(test-case render-head-html-css
  (let ((s "  <link rel=\"stylesheet\" href=\"css/foo.css\">~%"))
    (assert (string= (head-html "foo.css" '(("root" . "")))
                     (format nil s)))))

(test-case render-head-html-js
  (assert (string= (head-html "foo.js" '(("root" . "")))
                   (format nil "  <script src=\"js/foo.js\"></script>~%"))))

(test-case render-head-html-inc
  (assert (string= (head-html "test.inc" '())
                   (format nil "  <!-- {{ a }} test include -->~%"))))

(test-case render-head-html-inc-params
  (assert (string= (head-html "test.inc" '(("a" . "apple")))
                   (format nil "  <!-- apple test include -->~%"))))

(test-case render-head-html-css-root
  (let ((s "  <link rel=\"stylesheet\" href=\"../css/foo.css\">~%"))
    (assert (string= (head-html "foo.css" '(("root" . "../")))
                     (format nil s)))))

(test-case render-head-html-js-root
  (let ((s "  <script src=\"../js/foo.js\"></script>~%"))
    (assert (string= (head-html "foo.js" '(("root" . "../")))
                     (format nil s)))))

(test-case render-head-html-js-css-inc
  (assert (string=
           (head-html "foo.css bar.js test.inc baz.css qux.js test.inc"
                      '(("root" . "") ("a" . "apple")))
           "  <link rel=\"stylesheet\" href=\"css/foo.css\">
  <script src=\"js/bar.js\"></script>
  <!-- apple test include -->
  <link rel=\"stylesheet\" href=\"css/baz.css\">
  <script src=\"js/qux.js\"></script>
  <!-- apple test include -->
")))

(test-case set-nested-template
  (let ((layout "{{ foo }}"))
    (set-nested-template layout "{{ foo }}{{ body }}{{ baz }}")
    (assert (string= layout "{{ foo }}{{ foo }}{{ baz }}")))
  (let ((layout "foo"))
    (set-nested-template layout "foo {{ body }} bar")
    (assert (string= layout "foo foo bar"))))

(test-case relative-root-path
  (assert (string= (relative-root-path "_site/") "./"))
  (assert (string= (relative-root-path "_site/index.html") "./"))
  (assert (string= (relative-root-path "_site/foo") "./"))
  (assert (string= (relative-root-path "_site/foo/") "../"))
  (assert (string= (relative-root-path "_site/foo/index.html") "../"))
  (assert (string= (relative-root-path "_site/foo/bar") "../"))
  (assert (string= (relative-root-path "_site/foo/bar/") "../../"))
  (assert (string= (relative-root-path "_site/foo/bar/index.html") "../../")))

(test-case add-head-params-imports
  (let ((params (list (cons "import" "foo.js")))
        (result (format nil "  <script src=\"~~ajs/foo.js\"></script>~%")))
    (add-head-params "_site/" params)
    (assert (string= (aget "imports" params) (format nil result "./")))
    (add-head-params "_site/foo.html" params)
    (assert (string= (aget "imports" params) (format nil result "./")))
    (add-head-params "_site/foo/" params)
    (assert (string= (aget "imports" params) (format nil result "../")))
    (add-head-params "_site/foo/bar.html" params)
    (assert (string= (aget "imports" params) (format nil result "../")))))

(test-case add-head-params-canonical-url
  (let ((params (list (cons "site-url" "https://example.com/"))))
    (add-head-params "_site/" params)
    (assert (string= (aget "canonical-url" params) "https://example.com/"))
    (add-head-params "_site/foo/" params)
    (assert (string= (aget "canonical-url" params) "https://example.com/foo/"))
    (add-head-params "_site/foo/bar/" params)
    (assert (string= (aget "canonical-url" params)
                     "https://example.com/foo/bar/"))))

(test-case add-head-params-canonical-url-index
  (let ((params (list (cons "site-url" "https://example.com/"))))
    (add-head-params "_site/index.html" params)
    (assert (string= (aget "canonical-url" params) "https://example.com/"))
    (add-head-params "_site/foo/index.html" params)
    (assert (string= (aget "canonical-url" params) "https://example.com/foo/"))
    (add-head-params "_site/foo/bar/index.html" params)
    (assert (string= (aget "canonical-url" params)
                     "https://example.com/foo/bar/"))))

(test-case make-posts-single
  (write-file "test-tmp/content/foo.txt" "foo")
  (make-posts "test-tmp/content/foo.txt" "test-tmp/output/out.txt"
              "[{{ body }}]" nil)
  (assert (string= (read-file "test-tmp/output/out.txt") "[foo]")))

(test-case make-posts-multiple
  (write-file "test-tmp/content/2020-06-01-foo.txt" "foo")
  (write-file "test-tmp/content/2020-06-02-bar.txt" "bar")
  (write-file "test-tmp/content/2020-06-03-baz.txt" "baz")
  (make-posts "test-tmp/content/*.txt" "test-tmp/output/{{ slug }}.txt"
              "[{{ body }}]" nil)
  (assert (string= (read-file "test-tmp/output/foo.txt") "[foo]"))
  (assert (string= (read-file "test-tmp/output/bar.txt") "[bar]"))
  (assert (string= (read-file "test-tmp/output/baz.txt") "[baz]")))

(test-case make-posts-multiple-sort
  (write-file "test-tmp/content/2020-06-01-foo.txt" "foo")
  (write-file "test-tmp/content/2020-06-02-bar.txt" "bar")
  (write-file "test-tmp/content/2020-06-03-baz.txt" "baz")
  (let ((posts (make-posts "test-tmp/content/*.txt"
                           "test-tmp/output/{{ slug }}.txt"
                           "[{{ body }}]" nil)))
    (assert (= (length posts) 3))
    (assert (string= (aget "date" (first posts)) "2020-06-01"))
    (assert (string= (aget "date" (second posts)) "2020-06-02"))
    (assert (string= (aget "date" (third posts)) "2020-06-03"))))

(test-case make-posts-filename-params
  (write-file "test-tmp/content/2020-06-01-foo.txt" "foo")
  (make-posts "test-tmp/content/*.txt" "test-tmp/output/{{ slug }}.txt"
              "[{{ date }} {{ slug }} {{ body }}]" nil)
  (assert (string= (read-file "test-tmp/output/foo.txt")
                   "[2020-06-01 foo foo]")))

(test-case make-posts-filename-params-overrides-call-params
  (write-file "test-tmp/content/2020-06-01-foo.txt" "foo")
  (make-posts "test-tmp/content/*.txt"
              "test-tmp/output/{{ slug }}.txt"
              "[{{ date }} {{ slug }} {{ body }}]"
              (list (cons "date" "2020-06-01") (cons "slug" "quux")))
  (assert (string= (read-file "test-tmp/output/foo.txt")
                   "[2020-06-01 foo foo]")))

(test-case make-posts-callback
  (defun callback (params)
    (declare (ignore params))
    (list (cons "a" "apple")))
  (write-file "test-tmp/content/foo.txt" "foo")
  (make-posts "test-tmp/content/foo.txt"
              "test-tmp/output/foo.txt"
              "[{{ body }} {{ a }}]"
              (list (cons "callback" #'callback)))
  (assert (string= (read-file "test-tmp/output/foo.txt")
                   "[foo apple]")))

(test-case make-posts-callback-params-overrides-call-params
  (defun callback (params)
    (declare (ignore params))
    (list (cons "a" "ant")))
  (write-file "test-tmp/content/foo.txt" "foo")
  (make-posts "test-tmp/content/foo.txt"
              "test-tmp/output/foo.txt"
              "[{{ body }} {{ a }}]"
              (list (cons "a" "apple") (cons "callback" #'callback)))
  (assert (string= (read-file "test-tmp/output/foo.txt") "[foo ant]")))

(test-case make-posts-no-content-rendering
  (write-file "test-tmp/content/foo.txt" "foo {{ a }} bar")
  (make-posts "test-tmp/content/foo.txt"
              "test-tmp/output/foo.txt"
              "[{{ body }}]"
              (list (cons "a" "apple")))
  (assert (string= (read-file "test-tmp/output/foo.txt")
                   "[foo {{ a }} bar]")))

(test-case make-posts-content-rendering
  (write-file "test-tmp/content/foo.txt" "foo {{ a }} bar")
  (make-posts "test-tmp/content/foo.txt"
              "test-tmp/output/foo.txt"
              "[{{ body }}]"
              (list (cons "a" "apple") (cons "render" "yes")))
  (assert (string= (read-file "test-tmp/output/foo.txt") "[foo apple bar]")))

(test-case make-posts-import-css
  (write-file "test-tmp/content/foo.txt" "foo")
  (make-posts "test-tmp/content/foo.txt"
              "test-tmp/output/foo.txt"
              "[{{ imports }}{{ body }}]"
              (list (cons "import" "foo.css") (cons "root" "")))
  (let ((s "[  <link rel=\"stylesheet\" href=\"../../css/foo.css\">
foo]"))
    (assert (string= (read-file "test-tmp/output/foo.txt") s))))

(test-case make-posts-import-js
  (write-file "test-tmp/content/foo.txt" "foo")
  (make-posts "test-tmp/content/foo.txt"
              "test-tmp/output/foo.txt"
              "[{{ imports }}{{ body }}]"
              (list (cons "import" "foo.js")))
  (assert (string= (read-file "test-tmp/output/foo.txt")
                   "[  <script src=\"../../js/foo.js\"></script>
foo]")))

(test-case make-post-list
  (write-file "test-tmp/content/2020-06-01-foo.txt" "foo")
  (write-file "test-tmp/content/2020-06-02-bar.txt" "bar")
  (write-file "test-tmp/content/2020-06-03-baz.txt" "baz")
  (let ((posts (make-posts "test-tmp/content/*.txt"
                           "test-tmp/output/{{ slug }}.txt"
                           "[{{ body }}]" nil)))
    (make-post-list posts "test-tmp/list.html"
                    "[{{ count }} {{ post-label }} {{ body }}]"
                    "[{{ body }}]" nil))
  (assert (string= (read-file "test-tmp/list.html")
                   "[3 posts [baz][bar][foo]]")))

(test-case make-post-list-post-call-params
  (write-file "test-tmp/content/2020-06-01-foo.txt" "foo")
  (write-file "test-tmp/content/2020-06-02-bar.txt" "bar")
  (write-file "test-tmp/content/2020-06-03-baz.txt" "baz")
  (let ((posts (make-posts "test-tmp/content/*.txt"
                           "test-tmp/output/{{ slug }}.txt"
                           "[{{ body }}]" nil)))
    (make-post-list posts "test-tmp/list.html"
                    "[{{ a }} {{ body }}]"      ; Param used here
                    "[{{ a }} {{ body }}]"      ; and here.
                    (list (cons "a" "apple")))) ; Call param.
  (assert (string= (read-file "test-tmp/list.html")
                   "[apple [apple baz][apple bar][apple foo]]")))

(test-case make-post-list-post-params-override-call-params
  (write-file "test-tmp/content/2020-06-01-foo.txt"
              (format nil "<!-- a: air -->~%foo")) ; Post param.
  (write-file "test-tmp/content/2020-06-02-bar.txt"
              (format nil "<!-- a: ant -->~%bar")) ; Post param.
  (write-file "test-tmp/content/2020-06-03-baz.txt"
              (format nil "<!-- a: ash -->~%baz")) ; Post param.
  (let ((posts (make-posts "test-tmp/content/*.txt"
                           "test-tmp/output/{{ slug }}.txt"
                           "[{{ body }}]"
                           nil)))
    ;; The call below passes a call param but it is going to be
    ;; overridden by post params.
    (make-post-list posts "test-tmp/list.html"
                    "[{{ a }} {{ body }}]"  ; Param used here
                    "[{{ a }} {{ body }}]"  ; and here.
                    (list (cons "a" "apple"))))
  (assert (string= (read-file "test-tmp/list.html")
                   "[apple [ash baz][ant bar][air foo]]")))

(test-case read-comment
  (let ((text "<!-- date: 2020-06-01 07:08:09 +0000 -->
<!-- name: Alice -->
x
<!-- date: 2020-06-02 17:18:19 +0000 -->
<!-- name: Bob -->
<!-- url: https://example.com/ -->
yz
"))
    (multiple-value-bind (p next-index) (read-comment text 0)
      (assert (string= (aget "date" p) "2020-06-01 07:08:09 +0000"))
      (assert (string= (aget "simple-date" p) "01 Jun 2020 07:08 GMT"))
      (assert (string= (aget "name" p) "Alice"))
      (assert (string= (aget "commenter" p) "Alice"))
      (assert (string= (aget "body" p) (format nil "x~%")))
      (assert (= next-index 64)))
    (multiple-value-bind (p next-index) (read-comment text 64)
      (assert (string= (aget "date" p) "2020-06-02 17:18:19 +0000"))
      (assert (string= (aget "simple-date" p) "02 Jun 2020 17:18 GMT"))
      (assert (string= (aget "name" p) "Bob"))
      (assert (string= (aget "commenter" p)
                       "<a href=\"https://example.com/\">Bob</a>"))
      (assert (string= (aget "body" p) (format nil "yz~%")))
      (assert (eq next-index nil)))))

(test-case read-comments-single
  (write-file "test-tmp/comments.txt" "<!-- date: 2020-06-01 07:08:09 +0000 -->
<!-- name: Alice -->
Foo")
  (multiple-value-bind (comments slug) (read-comments "test-tmp/comments.txt")
    (assert (string= slug "comments"))
    (assert (= (length comments) 1))
    (let ((comment1 (first comments)))
      (assert (string= (aget "date" comment1) "2020-06-01 07:08:09 +0000"))
      (assert (string= (aget "name" comment1) "Alice"))
      (assert (string= (aget "body" comment1) "Foo")))))

(test-case read-comments-multiple
  (write-file "test-tmp/comments.txt" "<!-- date: 2020-06-01 00:00:01 +0000 -->
<!-- author: Alice -->
X
<!-- date: 2020-06-02 00:00:02 +0000 -->
<!-- author: Bob -->
Y
<!-- date: 2020-06-03 00:00:03 +0000 -->
<!-- author: Carol -->
Z")
  (multiple-value-bind (comments slug) (read-comments "test-tmp/comments.txt")
    (assert (string= slug "comments"))
    (assert (= (length comments) 3))
    (let* ((comment1 (first comments))
           (comment2 (second comments))
           (comment3 (third comments)))
      (assert (string= (aget "date" comment1) "2020-06-03 00:00:03 +0000"))
      (assert (string= (aget "author" comment1) "Carol"))
      (assert (string= (aget "body" comment1) (format nil "Z")))
      (assert (string= (aget "date" comment2) "2020-06-02 00:00:02 +0000"))
      (assert (string= (aget "author" comment2) "Bob"))
      (assert (string= (aget "body" comment2) (format nil "Y~%")))
      (assert (string= (aget "date" comment3) "2020-06-01 00:00:01 +0000"))
      (assert (string= (aget "author" comment3) "Alice"))
      (assert (string= (aget "body" comment3) (format nil "X~%"))))))

(test-case make-comment-list
  (make-comment-list
   (list (cons "slug" "foo")
         (cons "title" "Foo"))
   (list (list (cons "date" "2020-06-01")
               (cons "author" "Alice")
               (cons "body" "Foo"))
         (list (cons "date" "2020-06-02")
               (cons "author" "Bob")
               (cons "body" "Bar"))
         (list (cons "date" "2020-06-03")
               (cons "author" "Carol")
               (cons "body" "Baz")))
   "test-tmp/{{ slug }}.html"
   "[{{ title }} {{ count }} {{ comment-label }} {{ post-title }} {{ body }}]"
   "[{{ date }} {{ author }} {{ body }} {{ comment-id }}]" nil)
  (assert(string= (read-file "test-tmp/foo.html")
                  (join-strings '("[Comments on Foo 3 comments Foo "
                                  "[2020-06-03 Carol Baz 1]"
                                  "[2020-06-02 Bob Bar 2]"
                                  "[2020-06-01 Alice Foo 3]]")))))

(test-case make-comment-list-imports
  (make-comment-list (list (cons "slug" "foo") (cons "title" "Foo"))
                     (list (list (cons "date" "2020-06-01")
                                 (cons "author" "Alice")
                                 (cons "body" "Foo")))
                     "test-tmp/comments.html"
                     "[{{ imports }}]"
                     "[{{ body }}]"
                     (list (cons "root" "")))
  (assert
   (string= (read-file "test-tmp/comments.html")
            "[  <link rel=\"stylesheet\" href=\"../css/comment.css\">
]")))

(test-case make-comment-none
  (make-comment-none (list (cons "slug" "foo") (cons "title" "Foo"))
                     "test-tmp/{{ slug }}.html"
                     "[{{ title }} {{ post-title }} {{ a }}]"
                     (list (cons "a" "apple")))
  (assert (string= (read-file "test-tmp/foo.html")
                   "[Comments on Foo Foo apple]")))

(test-case links-html-single
  (assert (string= (links-html (list (cons "pdf" "foo.pdf")))
                   " [<a href=\"foo.pdf\" class=\"basic\">PDF</a>]"))
  (assert (string= (links-html (list (cons "url" "foo.html")))
                   " [<a href=\"foo.html\" class=\"basic\">url</a>]"))
  (assert (string= (links-html (list (cons "pdf" "foo.pdf")
                                     (cons "url" "foo.html")))
                   (join-strings
                    (list " [<a href=\"foo.html\" class=\"basic\">url</a>]"
                          " [<a href=\"foo.pdf\" class=\"basic\">PDF</a>]")))))

;; End test cases.

(test-done)
