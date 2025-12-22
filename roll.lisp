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


(require "asdf")

(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system "cxml"))

(defvar *site-mode* nil)

(load "site.lisp")
(load "defs.lisp")


;;; Special Modes
;;; -------------

(defvar *roll-mode* t
  "Run main function iff true.")


;;; Tool Definitions
;;; ----------------

(defun make-feed-item (universal-time title link domain)
  "Create an alist for the given feed item."
  (list (cons "date" (format-content-date universal-time))
        (cons "short-date" (format-short-date universal-time))
        (cons "title" (html-escape
                       (string-trim-ws
                        (string-truncate-words
                         (string-crush-ws (remove-odd-chars title)) 70 2))))
        (cons "link" (string-trim-ws (html-escape link :amp t)))
        (cons "domain" domain)))

(defun node-tag (node tag-name)
  "From the given XML node, return the first node matching the given tag-name."
  (let ((matching-nodes (dom:get-elements-by-tag-name node tag-name)))
    (when (plusp (dom:length matching-nodes))
      (dom:item matching-nodes 0))))

(defun node-text (node)
  "From the given XML node, return all text within that node."
  (when node
    (with-output-to-string (out)
      (dom:do-node-list (child-node (dom:child-nodes node))
        (when (dom:text-node-p child-node)
          (write-string (dom:data child-node) out))))))

(defun node-attr (node attr-name)
  "From the given XML node, return value of the given attribute."
  (when node
    (dom:get-attribute node attr-name)))

(defun read-rss-feed (document domain)
  "Read RSS feed entries."
  (let ((entries (dom:get-elements-by-tag-name document "item"))
        (result))
    (write-log "Found ~a RSS entries" (dom:length entries))
    (dom:do-node-list (entry entries)
      (let* ((title (node-text (node-tag entry "title")))
             (link (node-text (node-tag entry "link")))
             (raw-date (or (node-text (node-tag entry "pubDate"))
                           (node-text (node-tag entry "dc:date"))))
             (univ-time (when raw-date (or (try-parse-rss-date raw-date)
                                           (try-parse-iso-date raw-date))))
             (full-date (when univ-time (format-content-date univ-time))))
        (write-log "Found RSS entry: pubDate: ~s (~s); link: ~s; title: ~s"
                   raw-date full-date link title)
        (when (and (stringp title) (string/= title "")
                   (stringp link) (string/= link "")
                   (good-link link) univ-time)
          (push (make-feed-item univ-time title link domain) result))))
    result))

(defun read-atom-feed (document domain)
  "Read Atom feed entries."
  (let ((entries (dom:get-elements-by-tag-name document "entry"))
        (result))
    (write-log "Found ~a Atom entries" (dom:length entries))
    (dom:do-node-list (entry entries)
      (let* ((title (node-text (node-tag entry "title")))
             (link (node-attr (node-tag entry "link") "href"))
             (raw-date (node-text (node-tag entry "published")))
             (univ-time (when raw-date (or (try-parse-rss-date raw-date)
                                           (try-parse-iso-date raw-date))))
             (full-date (when univ-time (format-content-date univ-time))))
        (write-log "Found Atom entry: pubDate: ~s (~s); link: ~s; title: ~s"
                   raw-date full-date link title)
        (when (and (stringp title) (string/= title "")
                   (stringp link) (string/= link "")
                   (good-link link)  univ-time)
          (push (make-feed-item univ-time title link domain) result))))
    result))

(defun read-feed (src-path)
  "Read feed items from the given feed file path."
  (let* ((document (cxml:parse (read-file src-path) (cxml-dom:make-dom-builder)))
         (domain (string-replace ".xml" "" (file-namestring src-path)))
         (root-name (dom:node-name (dom:document-element document))))
    (cond ((string= root-name "rss")
           (read-rss-feed document domain))
          ((string= root-name "feed")
           (read-atom-feed document domain))
          (t
           (write-log "ERROR: Feed ~a is neither RSS feed nor Atom feed" src-path)))))

(defun read-feeds (src max-per-feed)
  "Read most recent max-per-feed number of posts from each XML feed."
  (loop for path in (directory src)
        do (write-log "~%Reading ~a ..." (file-namestring path))
        append (last-n max-per-feed (sort-by-date (read-feed path)))))

(defun make-roll (dst-path params)
  "Read XML feeds from local cache and create blogroll page."
  (let* ((max-per-feed 5)
         (page-layout (read-file "layout/page.html"))
         (list-layout (read-file "layout/roll/list.html"))
         (item-layout (read-file "layout/roll/item.html"))
         (items (sort-by-date (read-feeds "_cache/roll/*.xml" max-per-feed)))
         (count (length items))
         (rendered-items))
    (write-log "~%Rendering blogroll with last ~a posts from each file ..." max-per-feed)
    (set-nested-template list-layout page-layout)
    (dolist (item items)
      (write-log "Rendering item: ~a ~a (~a)"
                 (aget "date" item) (aget "link" item) (aget "title" item))
      (push (render item-layout item) rendered-items))
    (aput "body" (join-strings rendered-items) params)
    (aput "count" count params)
    (aput "entry-label" (if (= count 1) "entry" "entries") params)
    (aput "gen-date" (format-long-date (get-universal-time) " at ") params)
    (aput "max-per-feed" (format nil "~r" max-per-feed) params)
    (write-page dst-path list-layout params)))

(defun roll ()
  "Generate blogroll."
  (write-log "Creating blogroll ...")
  (let* ((dst-path "_cache/roll/roll.html")
         (params (list (cons "apex" "_cache/roll/")
                       (cons "current-year" (nth-value 5 (get-decoded-time)))
                       (cons "head" "main.css")
                       (cons "index" ""))))
    (when (probe-file "params.lisp")
      (setf params (append (read-list "params.lisp") params)))
    (when *params*
      (setf params (append *params* params)))
    (aput "title" (render "{{ nick }}'s Blogroll" params) params)
    (aput "subtitle" "" params)
    (when (probe-file dst-path)
      (delete-file dst-path))
    (make-roll dst-path params))
  (write-log "~%Done"))

(when *roll-mode*
  (roll))
