;;;; Form Application
;;;; ================

(ql:quickload "hunchentoot")
(defvar *main-mode* nil)
(load "site.lisp")

(setf *random-state* (make-random-state t))

(defvar *form-mode* t
  "Run main function iff true.")


;;; Tool Definitions
;;; ----------------

(defvar *cache-directory* "/opt/data/form/"
  "Directory where post files and data are written to and read from.")

(defvar *log-directory* "/opt/log/form/"
  "Directory where log files are written to.")

(defun universal-time-string (universal-time-seconds)
  "Return given universal time in yyyy-mm-dd HH:MM:SS +0000 format."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time-seconds 0)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d +0000"
            year month date hour min sec)))

(defun current-utc-time-string ()
  "Return current UTC date and time in yyyy-mm-dd HH:MM:SS +0000 format."
  (universal-time-string (get-universal-time)))

(defun log-file-path ()
  "Return path to the log file."
  (format nil "~aform.log" *log-directory*))

(defun append-file (filename text)
  "Append text to file and close the file."
  (make-directory filename)
  (with-open-file (f filename :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
    (write-sequence text f)))

(defun real-ip ()
  "Return address of the remote client (not of the local reverse-proxy)."
  (hunchentoot:real-remote-addr))

(defun write-form-log (fmt &rest args)
  "Log message with specified arguments."
  (when *log-mode*
    (append-file (log-file-path)
     (with-output-to-string (s)
       (format s "~a - [~a] \"~a ~a\" "
               (real-ip)
               (current-utc-time-string)
               (hunchentoot:request-method*)
               (hunchentoot:request-uri*))
       (apply #'format s fmt args)
       (terpri s)))))

(defun time-based-filename (time-string)
  "Convert UTC time string to a filename."
  (setf time-string (string-replace ":" "-" time-string))
  (setf time-string (string-replace " " "_" time-string))
  (setf time-string (string-replace "+" "" time-string)))

(defun from-get (name)
  "Get the value of a GET parameter."
  (hunchentoot:get-parameter name))

(defun from-post (name)
  "Get the value of a POST parameter."
  (hunchentoot:post-parameter name))

(defun write-comment (directory ip current-time params)
  "Save comment to a file."
  (let* ((time-string (universal-time-string current-time))
         (text (with-output-to-string (s)
                 (format s "post: ~a~%" (get-value "post" params))
                 (format s "user-agent: ~a~%" (hunchentoot:user-agent))
                 (format s "remote-addr: ~a~%" (hunchentoot:remote-addr*))
                 (format s "real-ip: ~a~%" ip)
                 (format s "<!-- date: ~a -->~%" time-string)
                 (format s "<!-- name: ~a -->~%" (get-value "name" params))
                 (when (string/= (get-value "url" params) "")
                   (format s "<!-- url: ~a -->~%" (get-value "url" params)))
                 (format s "~a~%" (get-value "comment" params))))
         (filename (format nil "~acomment_~a_~a_~a.txt" directory
                           (get-value "slug" params)
                           (time-based-filename time-string)
                           (random 1000000))))
    (write-file filename text)))

(defmacro add-page-params (params)
  "Add common parameters necessary for page rendering."
  `(progn
     (add-value "root" "../../" ,params)
     (add-value "index" "" ,params)
     (add-value "subtitle" " - Susam Pal" ,params)
     (add-value "site-url" "https://susam.net/" ,params)
     (add-value "zone-title" "Blog" ,params)
     (add-value "zone-path" "blog/" ,params)
     (add-value "initial-year" 2006 ,params)
     (add-value "current-year" (nth-value 5 (get-decoded-time)) ,params)
     (add-value "canonical-url" "" ,params)
     (add-value "heads" (head-html "main.css" ,params) ,params)
     (add-value "imports" (head-html "form.css" ,params) ,params)))

(defun form-index-page ()
  "Return HTML response for form index page."
  (let ((page-layout (read-file "layout/page.html"))
        (index-layout (read-file "layout/form/index.html"))
        (params))
    (setf index-layout (render page-layout (list (cons "body" index-layout))))
    (add-page-params params)
    (add-value "title" "Forms" params)
    (render index-layout params)))

(defun format-status (lines)
  "Format status lines for display in form response."
  (setf lines (loop for x in lines collect (format nil "<li>~a</li>~%" x)))
  (setf lines (join-strings lines))
  (setf lines (format nil "<ul>~%~a</ul>~%" lines)))

(defun read-options (directory)
  "Read options file."
  (let ((path (merge-pathnames "opt.lisp" directory)))
    (when (probe-file path)
      (read-from-string (read-file path)))))


;;; Post Control
;;; ------------

(defvar *last-post-time* 0
  "The universal-time at which the last post was successfully submitted.")

(defvar *flood-table* (make-hash-table :test #'equal :synchronized t)
  "A map of IP addresses to last time they made a successful post.")

(defmacro set-flood-data (ip current-time last-post-time-var flood-table-var)
  "Update flood control state variables."
  `(progn
     (setf ,last-post-time-var ,current-time)
     (setf (gethash ,ip ,flood-table-var) ,current-time)))

(defun global-flood-p (options current-time last-post-time)
  "Compute number of seconds before next post will be accepted."
  (let* ((post-interval (getf options :global-post-interval 0))
         (wait-time (- (+ last-post-time post-interval) current-time)))
    (when (plusp wait-time)
      wait-time)))

(defun client-flood-p (options ip current-time flood-table)
  "Compute number of seconds client must wait to avoid client flooding."
  (let ((post-interval (or (getf options :client-post-interval) 0)))
    (maphash #'(lambda (key value)
                 (when (>= current-time (+ value post-interval))
                   (remhash key flood-table)))
             flood-table)
    (write-form-log "Flood table size is ~a" (hash-table-count flood-table))
    (let* ((last-post-time (gethash ip flood-table 0))
           (wait-time (- (+ last-post-time post-interval) current-time)))
      (when (plusp wait-time)
        wait-time))))

(defun dodgy-ip-p (options ip)
  "Check if given IP address is not allowed to post."
  (let ((prefixes (getf options :ban))
        (formatted-ip (format nil "~a$" ip))
        (result))
    (dolist (prefix prefixes)
      (when (string-starts-with prefix formatted-ip)
        (setf result ip)
        (return)))
    result))


;;; Comment Form
;;; ------------

(defun input-intact-message ()
  "Return a message that describes that user input has been left intact."
  (format nil "~@{~a~}"
          "Your input is left intact below in case you want to "
          "edit, copy, or resubmit it."))

(defun reject-comment-p (options ip current-time params)
  "Validate post and return a list of error messages on failure."
  (let ((max-name-length 100)
        (max-url-length 1000)
        (max-comment-length 1000)
        (result)
        (errors))
    (when (or (string= (get-value "post" params) "")
              (string= (get-value "name" params) "")
              (string= (get-value "comment" params) ""))
      (push "Invalid request." errors))
    (when (getf options :read-only)
      (push "New comments have been disabled temporarily." errors))
    (when (> (length (get-value "name" params)) max-name-length)
      (push (format nil "Name exceeds ~a characters." max-name-length) errors))
    (when (> (length (get-value "url" params)) max-url-length)
      (push (format nil "URL exceeds ~a characters." max-url-length) errors))
    (when (> (length (get-value "comment" params)) max-comment-length)
      (push (format nil "Comment exceeds ~a characters." max-comment-length) errors))
    (when (setf result (dodgy-ip-p options ip))
      (push (format nil "IP address ~a is banned." result) errors))
    (when (setf result (global-flood-p options current-time *last-post-time*))
      (push (format nil "Wait for ~a s before submitting." result) errors))
    (when (setf result (client-flood-p options ip current-time *flood-table*))
      (push (format nil "Wait for ~a s before resubmitting." result) errors))
    (reverse errors)))

(defun dodgy-comment-p (params)
  "Check if post content has invalid fields."
  (or (string/= (get-value "post" params) (get-value "slug" params))
      (string/= (from-post (get-value "xkey" params)) (get-value "xval" params))))

(defun reject-comment (layout errors params)
  "Reject post with error messages."
  (write-form-log "Comment rejected:~{ ~a~}" errors)
  (add-value "title" "Post Comment" params)
  (add-value "class" "error" params)
  (add-value "status" (format-status errors) params)
  (render layout params))

(defun accept-comment (directory layout ip current-time params)
  "Update flood data and save post."
  (if (dodgy-comment-p params)
      (write-form-log "Dodgy comment")
      (progn
        (write-form-log "Written comment")
        (write-comment directory ip current-time params)))
  (set-flood-data ip current-time *last-post-time* *flood-table*)
  (add-value "title" "Comment Submitted" params)
  (add-value "class" "success" params)
  (let ((lines (list "Successfully submitted comment."
                     "It is now awaiting review and may be published after review."
                     (input-intact-message))))
    (add-value "status" (format-status lines) params))
  (render layout params))

(defun comment-form-post (directory layout options params)
  "Return processed form page."
  (let ((ip (real-ip))
        (current-time (get-universal-time))
        (errors))
    (add-value "post" (or (from-get "post") "") params)
    (dolist (key (list "slug" "name" "url" "comment"))
      (add-value key (or (from-post key) "") params))
    (if (setf errors (reject-comment-p options ip current-time params))
        (reject-comment layout errors params)
        (accept-comment directory layout ip current-time params))))

(defun comment-form-get (layout params)
  "Return empty form page."
  (add-value "title" "Post Comment" params)
  (add-value "class" "" params)
  (add-value "status" "" params)
  (add-value "post" (or (from-get "post") "") params)
  (add-value "slug" (or (from-get "post") "") params)
  (add-value "name" "" params)
  (add-value "url" "" params)
  (add-value "comment" "" params)
  (render layout params))

(defun comment-form (directory)
  "Comment form application."
  (let* ((page-layout (read-file "layout/page.html"))
         (form-layout (read-file "layout/form/comment.html"))
         (method (hunchentoot:request-method*))
         (options (read-options directory))
         (xkey (getf options :xkey "k"))
         (xval (getf options :xval "v"))
         (params))
    (setf form-layout (render page-layout (list (cons "body" form-layout))))
    (add-page-params params)
    (add-value "xkey" xkey params)
    (add-value "xval" xval params)
    (if (eq method :post)
        (comment-form-post directory form-layout options params)
        (comment-form-get form-layout params))))


;;; HTTP Request Handlers
;;; ---------------------

(defun define-handlers ()
  "Define handlers for HTTP requests."
  (hunchentoot:define-easy-handler (index :uri "/form/") ()
    (when (member (hunchentoot:request-method*) '(:head :get))
      (form-index-page)))
  (hunchentoot:define-easy-handler (comment :uri "/form/comment/") ()
    (when (member (hunchentoot:request-method*) '(:head :get :post))
      (comment-form *cache-directory*))))


;;; HTTP Server
;;; -----------

(defun start-server ()
  "Start HTTP server."
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :address "127.0.0.1"
                                 :port 4242
                                 :access-log-destination (log-file-path))))
    (setf (hunchentoot:acceptor-document-root acceptor) #p"_site/")
    (hunchentoot:start acceptor)))

(defun serve-form ()
  "Set up HTTP request handlers and start server."
  (define-handlers)
  (start-server)
  (sleep most-positive-fixnum))

(when *form-mode*
  (serve-form))
