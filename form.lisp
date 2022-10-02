;;;; Comment and Subscriber Forms
;;;; ============================

(ql:quickload "hunchentoot")
(defvar *main-mode* nil)
(load "site.lisp")

(setf *random-state* (make-random-state t))

(defvar *form-mode* t
  "Run main function iff true.")


;;; Tool Definitions
;;; ----------------

(defun universal-time-string (universal-time-seconds)
  "Return given universal time in yyyy-mm-dd HH:MM:SS +0000 format."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time-seconds 0)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d +0000"
            year month date hour min sec)))

(defun current-utc-time-string ()
  "Return current UTC date and time in yyyy-mm-dd HH:MM:SS +0000 format."
  (universal-time-string (get-universal-time)))

(defun time-based-filename (time-string)
  "Convert UTC time string to a filename."
  (setf time-string (string-replace ":" "-" time-string))
  (setf time-string (string-replace " " "_" time-string))
  (setf time-string (string-replace "+" "" time-string)))

(defun real-ip ()
  "Return address of the remote client (not of the local reverse-proxy)."
  (hunchentoot:real-remote-addr))

(defun write-log (fmt &rest args)
  "Log message with specified arguments."
  (when *log-mode*
    (format t "~a - [~a] \"~a ~a\" "
            (real-ip)
            (current-utc-time-string)
            (hunchentoot:request-method*)
            (hunchentoot:request-uri*))
    (apply #'format t fmt args)
    (terpri)))

(defun from-get (name)
  "Get the value of a GET parameter."
  (hunchentoot:get-parameter name))

(defun from-post (name)
  "Get the value of a POST parameter."
  (hunchentoot:post-parameter name))

(defun write-comment (ip current-time params)
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
         (filename (format nil "/opt/cache/comment_~a_~a_~a.txt"
                           (get-value "slug" params)
                           (time-based-filename time-string)
                           (random 1000000))))
    (write-file filename text)))

(defun write-subscriber (ip current-time email action)
  "Save subscriber/unsubscriber to a file."
  (let* ((time-string (universal-time-string current-time))
         (text (format nil "~a ~a (~a)~%" time-string email ip))
         (filename (format nil "/opt/cache/~a_~a_~a.txt"
                           action
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

(defun read-options ()
  "Read options file."
  (let ((path "/opt/cache/opt.lisp"))
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
  (let ((post-interval (or (getf options :client-post-interval) 60)))
    (maphash #'(lambda (key value)
                 (when (>= current-time (+ value post-interval))
                   (remhash key flood-table)))
             flood-table)
    (write-log "Flood table size is ~a" (hash-table-count flood-table))
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

(defun dodgy-comment-p (xkey xval params)
  "Check if post content has invalid fields."
  (or (string/= (get-value "post" params) (get-value "slug" params))
      (string/= (from-post xkey) xval)))

(defun reject-comment (layout errors xkey params)
  "Reject post with error messages."
  (write-log "Comment rejected:~{ ~a~}" errors)
  (add-value "title" "Post Comment" params)
  (add-value "class" "error" params)
  (add-value "status" (format-status errors) params)
  (render layout params))

(defun accept-comment (layout ip current-time xkey xval params)
  "Update flood data and save post."
  (if (dodgy-comment-p xkey xval params)
      (write-log "Dodgy comment")
      (progn
        (write-log "Written comment")
        (write-comment ip current-time params)))
  (set-flood-data ip current-time *last-post-time* *flood-table*)
  (add-value "title" "Comment Submitted" params)
  (add-value "class" "success" params)
  (let ((lines (list "Successfully submitted comment."
                     "It is now awaiting review and may be published after review."
                     (input-intact-message))))
    (add-value "status" (format-status lines) params))
  (render layout params))

(defun comment-form-post (layout options xkey xval params)
  "Return processed form page."
  (let ((ip (real-ip))
        (current-time (get-universal-time))
        (errors))
    (add-value "post" (or (from-get "post") "") params)
    (dolist (key (list "slug" "name" "url" "comment"))
      (add-value key (or (from-post key) "") params))
    (if (setf errors (reject-comment-p options ip current-time params))
        (reject-comment layout errors xkey params)
        (accept-comment layout ip current-time xkey xval params))))

(defun comment-form-get (layout xkey xval params)
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

(defun comment-form ()
  "Comment form application."
  (let* ((page-layout (read-file "layout/page.html"))
         (form-layout (read-file "layout/form/comment.html"))
         (method (hunchentoot:request-method*))
         (options (read-options))
         (xkey (getf options :xkey "k"))
         (xval (getf options :xval "v"))
         (params))
    (setf form-layout (render page-layout (list (cons "body" form-layout))))
    (add-page-params params)
    (add-value "xkey" xkey params)
    (add-value "xval" xval params)
    (if (eq method :post)
        (comment-form-post form-layout options xkey xval params)
        (comment-form-get form-layout xkey xval params))))


;;; Subscriber Form
;;; ---------------

(defun subscriber-form-get (layout params ykey yval)
  "Return empty form page."
  (add-value "title" "Post Comment" params)
  (add-value "class" "" params)
  (add-value "status" "" params)
  (add-value "post" (or (from-get "post") "") params)
  (add-value "slug" (or (from-get "post") "") params)
  (add-value "name" "" params)
  (add-value "url" "" params)
  (add-value "comment" "" params)
  (add-value "ykey" ykey params)
  (add-value "yval" yval params)
  (render layout params))

(defun subscriber-purpose (action)
  "Return purpose phrase for subscriber form."
  (let ((subscribers (+ 232 27)))
    (if (string= action "subscribe")
        (format nil "join ~a other subscribers and receive" subscribers)
        (format nil "stop receiving"))))

(defun subscriber-button (action)
  "Return text for the subscriber form submit button."
  (if (string= action "subscribe") "Subscribe Now" "Unsubscribe Now"))

(defun reject-subscriber-p (options ip current-time params)
  (let ((max-email-length 100)
        (result)
        (errors))
    (when (string= (get-value "email" params) "")
      (push "Invalid request." errors))
    (when (getf options :read-only)
      (push "New requests have been disabled temporarily." errors))
    (when (> (length (get-value "email" params)) max-email-length)
      (push "Email exceeds ~a characters." max-email-length) errors)
    (when (setf result (dodgy-ip-p options ip))
      (push (format nil "IP address ~a is banned." result) errors))
    (when (setf result (global-flood-p options current-time *last-post-time*))
      (push (format nil "Wait for ~a s before submitting." result) errors))
    (when (setf result (client-flood-p options ip current-time *flood-table*))
      (push (format nil "Wait for ~a s before resubmitting." result) errors))
    (reverse errors)))

(defun dodgy-subscriber-p (ykey yval)
  "Check if subscriber has invalid fields."
  (string/= (from-post ykey) yval))

(defun reject-subscriber (layout errors action ykey params)
  "Reject subscriber with error messages."
  (write-log "Subscriber rejected:~{ ~a~}" errors)
  (add-value "title" (string-capitalize action) params)
  (add-value "class" "error" params)
  (add-value "status" (format-status errors) params)
  (render layout params))

(defun accept-subscriber (layout ip current-time action ykey yval params)
  "Update flood data and save subscriber."
  (let ((email (get-value "email" params)))
    (if (dodgy-subscriber-p ykey yval)
        (write-log "Dodgy ~ar: ~a" action email)
        (progn
          (write-log "Written ~ar: ~a" action email)
          (write-subscriber ip current-time email action))))
  (set-flood-data ip current-time *last-post-time* *flood-table*)
  (add-value "title" (format nil "Successfully ~@(~a~)d" action) params)
  (add-value "class" "success" params)
  (let ((lines (list (format nil "Successfully ~ad." action)
                     (input-intact-message))))
    (add-value "status" (format-status lines) params))
  (render layout params))

(defun subscriber-form-post (layout options action ykey yval params)
  "Return processed subscriber form page."
  (add-value "email" (or (from-post "email") "") params)
  (let ((ip (real-ip))
        (current-time (get-universal-time))
        (errors))
    (if (setf errors (reject-subscriber-p options ip current-time params))
        (reject-subscriber layout errors action ykey params)
        (accept-subscriber layout ip current-time action ykey yval params))))

(defun subscriber-form-get (layout action ykey yval params)
  "Return empty subscriber form page."
  (add-value "title" (string-capitalize action) params)
  (add-value "class" "" params)
  (add-value "status" "" params)
  (add-value "email" "" params)
  (render layout params))

(defun subscriber-form (action)
  "Subscriber form application."
  (let* ((page-layout (read-file "layout/page.html"))
         (form-layout (read-file "layout/form/subscribe.html"))
         (method (hunchentoot:request-method*))
         (options (read-options))
         (ykey (getf options :ykey "k"))
         (yval (getf options :yval "v"))
         (params))
    (setf form-layout (render page-layout (list (cons "body" form-layout))))
    (add-page-params params)
    (add-value "purpose" (subscriber-purpose action) params)
    (add-value "submit" (subscriber-button action) params)
    (add-value "ykey" ykey params)
    (add-value "yval" yval params)
    (if (eq method :post)
        (subscriber-form-post form-layout options action ykey yval params)
        (subscriber-form-get form-layout action ykey yval params))))


;;; HTTP Request Handlers
;;; ---------------------

(defun define-handlers ()
  "Define handlers for HTTP requests."
  (hunchentoot:define-easy-handler (index :uri "/form/") ()
    (when (member (hunchentoot:request-method*) '(:head :get))
      (form-index-page)))

  (hunchentoot:define-easy-handler (comment :uri "/form/comment/") ()
    (when (member (hunchentoot:request-method*) '(:head :get :post))
      (comment-form)))

  (hunchentoot:define-easy-handler (subscribe :uri "/form/subscribe/") ()
    (when (member (hunchentoot:request-method*) '(:head :get :post))
      (subscriber-form "subscribe")))

  (hunchentoot:define-easy-handler (unsubscribe :uri "/form/unsubscribe/") ()
    (when (member (hunchentoot:request-method*) '(:head :get :post))
      (subscriber-form "unsubscribe"))))


;;; HTTP Server
;;; -----------

(defun start-server ()
  "Start HTTP server."
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :address "127.0.0.1" :port 4242)))
    (setf (hunchentoot:acceptor-document-root acceptor) #p"_site/")
    (hunchentoot:start acceptor)))

(defun serve-form ()
  "Set up HTTP request handlers and start server."
  (define-handlers)
  (start-server)
  (sleep most-positive-fixnum))


(when *form-mode*
  (serve-form))
