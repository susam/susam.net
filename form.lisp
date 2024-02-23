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

(defvar *data-directory* "/opt/data/form/"
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
                 (format s "p: ~a~%" (aget "p" params))
                 (format s "user-agent: ~a~%" (hunchentoot:user-agent))
                 (format s "remote-addr: ~a~%" (hunchentoot:remote-addr*))
                 (format s "real-ip: ~a~%" ip)
                 (format s "<!-- date: ~a -->~%" time-string)
                 (format s "<!-- name: ~a -->~%" (aget "name" params))
                 (when (string/= (aget "url" params) "")
                   (format s "<!-- url: ~a -->~%" (aget "url" params)))
                 (format s "~a~%" (aget "comment" params))))
         (filename (format nil "~acomment_~a_~a_~a.txt" directory
                           (aget "slug" params)
                           (time-based-filename time-string)
                           (random 1000000))))
    (write-file filename text)))

(defun write-subscriber (directory ip current-time email action)
  "Save subscriber/unsubscriber to a file."
  (let* ((time-string (universal-time-string current-time))
         (text (format nil "~a ~a (~a)~%" time-string email ip))
         (filename (format nil "~a~a_~a_~a.txt" directory action
                           (time-based-filename time-string)
                           (random 1000000))))
    (write-file filename text)))

(defmacro add-page-params (params)
  "Add common parameters necessary for page rendering."
  `(progn
     (aput "root" "../../" ,params)
     (aput "index" "" ,params)
     (aput "zone-link" "" ,params)
     (aput "current-year" (nth-value 5 (get-decoded-time)) ,params)
     (aput "neat-url" (fstr "https://susam.net~a" (hunchentoot:request-uri*)) ,params)
     (aput "heads" (head-html "main.css" ,params) ,params)
     (aput "imports" (head-html "form.css" ,params) ,params)
     (when (probe-file "params.lisp")
       (setf params (append (read-list "params.lisp") params)))))

(defun form-index-page ()
  "Return HTML response for form index page."
  (let ((page-layout (read-file "layout/page.html"))
        (index-layout (read-file "layout/form/index.html"))
        (params))
    (setf index-layout (render page-layout (list (cons "body" index-layout))))
    (add-page-params params)
    (aput "title" "Forms" params)
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

(defun calc-token (a)
  "Calculate token from given integer."
  (let ((b (mod a 97))
        (c (mod a 71)))
    (+ (* 1000000 a) (* 1000 b) c)))

(defun dodgy-post-p (token)
  "Check if post content contains invalid token."
  (let* ((digits (and (string/= token "") (every #'digit-char-p token)))
         (x (if digits (parse-integer token) 0))
         (a (floor x 1000000))
         (xx (calc-token a)))
    (or (< x 403) (/= x xx))))

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
    (when (or (string= (aget "p" params) "")
              (string= (aget "name" params) "")
              (string= (aget "comment" params) ""))
      (push "Invalid request." errors))
    (when (getf options :read-only)
      (push "New comments have been disabled temporarily." errors))
    (when (> (length (aget "name" params)) max-name-length)
      (push (format nil "Name exceeds ~a characters." max-name-length) errors))
    (when (> (length (aget "url" params)) max-url-length)
      (push (format nil "URL exceeds ~a characters." max-url-length) errors))
    (when (> (length (aget "comment" params)) max-comment-length)
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
  (or (string/= (aget "p" params) (aget "slug" params))
      (string/= (from-post (aget "ukey" params)) (aget "uval" params))
      (string/= (from-post (aget "vkey" params)) (aget "vval" params))
      (dodgy-post-p (or (from-post "token") ""))))

(defun reject-comment (layout errors params)
  "Reject post with error messages."
  (write-form-log "Comment rejected:~{ ~a~}" errors)
  (aput "title" "Post Comment" params)
  (aput "class" "error" params)
  (aput "status" (format-status errors) params)
  (render layout params))

(defun accept-comment (directory layout ip current-time params)
  "Update flood data and save post."
  (if (dodgy-comment-p params)
      (write-form-log "Dodgy comment")
      (progn
        (write-form-log "Written comment")
        (write-comment directory ip current-time params)))
  (set-flood-data ip current-time *last-post-time* *flood-table*)
  (aput "title" "Comment Submitted" params)
  (aput "class" "success" params)
  (let ((lines (list "Successfully submitted comment."
                     "It is now awaiting review and may be published after review."
                     (input-intact-message))))
    (aput "status" (format-status lines) params))
  (render layout params))

(defun comment-form-post (directory layout options params)
  "Return processed form page."
  (write-form-log "Data ~a=~a ~a=~a token=~a slug=~a name=~a url=~a comment=~a"
                  (aget "ukey" params) (from-post (aget "ukey" params))
                  (aget "vkey" params) (from-post (aget "vkey" params))
                  (from-post "token")
                  (from-post "slug")
                  (from-post "name")
                  (from-post "url")
                  (let ((comment (from-post "comment")))
                    (subseq comment 0 (min 40 (length comment)))))
  (let ((ip (real-ip))
        (current-time (get-universal-time))
        (errors))
    (aput "p" (or (from-get "p") "") params)
    (dolist (key (list "slug" "name" "url" "comment"))
      (aput key (or (from-post key) "") params))
    (if (setf errors (reject-comment-p options ip current-time params))
        (reject-comment layout errors params)
        (accept-comment directory layout ip current-time params))))

(defun comment-form-get (layout params)
  "Return empty form page."
  (aput "title" "Post Comment" params)
  (aput "class" "" params)
  (aput "status" "" params)
  (aput "p" (or (from-get "p") "") params)
  (aput "slug" (or (from-get "p") "") params)
  (aput "name" "" params)
  (aput "url" "" params)
  (aput "comment" "" params)
  (render layout params))

(defun comment-form (directory)
  "Comment form application."
  (let* ((page-layout (read-file "layout/page.html"))
         (form-layout (read-file "layout/form/comment.html"))
         (method (hunchentoot:request-method*))
         (options (read-options directory))
         (params))
    (setf form-layout (render page-layout (list (cons "body" form-layout))))
    (add-page-params params)
    (aput "ukey" (getf options :ukey "uk") params)
    (aput "uval" (getf options :uval "uv") params)
    (aput "vkey" (getf options :vkey "vk") params)
    (aput "vval" (getf options :vval "vv") params)
    (if (eq method :post)
        (comment-form-post directory form-layout options params)
        (comment-form-get form-layout params))))


;;; Subscriber Form
;;; ---------------

(defun subscriber-intention (action)
  "Return intention phrase for subscriber form."
  (let ((subscribers (+ 232 30)))
    (if (string= action "subscribe")
        (format nil "join ~a other subscribers and receive" subscribers)
        (format nil "stop receiving"))))

(defun subscriber-button (action)
  "Return text for the subscriber form submit button."
  (if (string= action "subscribe") "Subscribe Now" "Unsubscribe Now"))

(defun subscriber-purpose (action)
  "Return purpose phrase for subscriber form."
  (if (string= action "subscribe") "added to a" "removed from the"))

(defun reject-subscriber-p (options ip current-time params)
  (let ((max-email-length 100)
        (result)
        (errors))
    (when (string= (aget "email" params) "")
      (push "Invalid request." errors))
    (when (getf options :read-only)
      (push "New requests have been disabled temporarily." errors))
    (when (> (length (aget "email" params)) max-email-length)
      (push "Email exceeds ~a characters." max-email-length) errors)
    (when (setf result (dodgy-ip-p options ip))
      (push (format nil "IP address ~a is banned." result) errors))
    (when (setf result (global-flood-p options current-time *last-post-time*))
      (push (format nil "Wait for ~a s before submitting." result) errors))
    (when (setf result (client-flood-p options ip current-time *flood-table*))
      (push (format nil "Wait for ~a s before resubmitting." result) errors))
    (reverse errors)))

(defun dodgy-subscriber-p (params)
  "Check if subscriber has invalid fields."
  (or (string/= (from-post (aget "xkey" params)) (aget "xval" params))
      (string/= (from-post (aget "ykey" params)) (aget "yval" params))
      (dodgy-post-p (or (from-post "token") ""))))

(defun reject-subscriber (layout errors action params)
  "Reject subscriber with error messages."
  (write-form-log "Subscriber rejected:~{ ~a~}" errors)
  (aput "title" (string-capitalize action) params)
  (aput "class" "error" params)
  (aput "status" (format-status errors) params)
  (render layout params))

(defun accept-subscriber (directory layout ip current-time action params)
  "Update flood data and save subscriber."
  (let ((email (aget "email" params)))
    (if (dodgy-subscriber-p params)
        (write-form-log "Dodgy ~ar: ~a" action email)
        (progn
          (write-form-log "Written ~ar: ~a" action email)
          (write-subscriber directory ip current-time email action))))
  (set-flood-data ip current-time *last-post-time* *flood-table*)
  (aput "title" (format nil "Successfully ~@(~a~)d" action) params)
  (aput "class" "success" params)
  (let ((lines (list (format nil "Successfully ~ad." action)
                     (input-intact-message))))
    (aput "status" (format-status lines) params))
  (render layout params))

(defun subscriber-form-post (directory layout options action params)
  "Return processed subscriber form page."
  (write-form-log "Data ~a=~a ~a=~a token=~a email=~a"
                  (aget "xkey" params) (from-post (aget "xkey" params))
                  (aget "ykey" params) (from-post (aget "ykey" params))
                  (from-post "token")
                  (from-post "email"))
  (aput "email" (or (from-post "email") "") params)
  (let ((ip (real-ip))
        (current-time (get-universal-time))
        (errors))
    (if (setf errors (reject-subscriber-p options ip current-time params))
        (reject-subscriber layout errors action params)
        (accept-subscriber directory layout ip current-time action params))))

(defun subscriber-form-get (layout action params)
  "Return empty subscriber form page."
  (aput "title" (string-capitalize action) params)
  (aput "class" "" params)
  (aput "status" "" params)
  (aput "email" "" params)
  (render layout params))

(defun subscriber-form (directory action)
  "Subscriber form application."
  (let* ((page-layout (read-file "layout/page.html"))
         (form-layout (read-file "layout/form/subscribe.html"))
         (method (hunchentoot:request-method*))
         (options (read-options directory))
         (params))
    (setf form-layout (render page-layout (list (cons "body" form-layout))))
    (add-page-params params)
    (aput "intention" (subscriber-intention action) params)
    (aput "submit" (subscriber-button action) params)
    (aput "purpose" (subscriber-purpose action) params)
    (aput "xkey" (getf options :xkey "xk") params)
    (aput "xval" (getf options :xval "xv") params)
    (aput "ykey" (getf options :ykey "yk") params)
    (aput "yval" (getf options :yval "yv") params)
    (if (eq method :post)
        (subscriber-form-post directory form-layout options action params)
        (subscriber-form-get form-layout action params))))


;;; HTTP Request Handlers
;;; ---------------------

(defun define-handlers ()
  "Define handlers for HTTP requests."
  (hunchentoot:define-easy-handler (index :uri "/form/") ()
    (when (member (hunchentoot:request-method*) '(:head :get))
      (form-index-page)))
  (hunchentoot:define-easy-handler (comment :uri "/form/comment/") ()
    (when (member (hunchentoot:request-method*) '(:head :get :post))
      (comment-form *data-directory*)))
  (hunchentoot:define-easy-handler (subscribe :uri "/form/subscribe/") ()
    (when (member (hunchentoot:request-method*) '(:head :get :post))
      (subscriber-form *data-directory* "subscribe")))
  (hunchentoot:define-easy-handler (unsubscribe :uri "/form/unsubscribe/") ()
    (when (member (hunchentoot:request-method*) '(:head :get :post))
      (subscriber-form *data-directory* "unsubscribe"))))


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
  (format t "Starting server; data directory: ~a; log directory: ~a~%"
          *data-directory* *log-directory*)
  (start-server)
  (sleep most-positive-fixnum))

(when *form-mode*
  (serve-form))
