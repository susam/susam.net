(ql:quickload "hunchentoot")
(defvar *main-mode* nil)
(load "site.lisp")

(setf *random-state* (make-random-state t))

(defun current-utc-time-string ()
  "Return current UTC date and time in yyyy-mm-dd HH:MM:SS format."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour min sec)))

(defun time-based-filename (time-string)
  "Convert UTC time string to a filename."
  (string-replace ":" "-" (string-replace " " "_" time-string)))

(defun x-real-ip ()
  "Return the real remote address passed on by the reverse proxy."
  (let ((real-ip (hunchentoot:header-in* :x-real-ip)))
    (if real-ip real-ip (format nil "Found to be ~a" real-ip))))

(defun from-get (name)
  "Get the value of a GET parameter."
  (hunchentoot:get-parameter name))

(defun from-post (name)
  "Get the value of a POST parameter."
  (hunchentoot:post-parameter name))

(defun write-comment (params)
  "Save comment to a file."
  (let* ((time-string (current-utc-time-string))
         (text (with-output-to-string (s)
                 (format s "post: ~a~%" (get-value "post-value" params))
                 (format s "user-agent: ~a~%" (hunchentoot:user-agent))
                 (format s "remote-addr: ~a~%" (hunchentoot:remote-addr*))
                 (format s "x-real-ip: ~a~%" (x-real-ip))
                 (format s "<!-- date: ~a +0000 -->~%" time-string)
                 (format s "<!-- name: ~a -->~%" (get-value "name-value" params))
                 (when (string/= (get-value "url-value" params) "")
                   (format s "<!-- url: ~a -->~%" (get-value "url-value" params)))
                 (format s "~a~%" (get-value "comment-value" params))))
         (filename (format nil "/opt/cache/comment_~a_~a_~a.txt"
                           (get-value "slug-value" params)
                           (time-based-filename time-string)
                           (random 1000000))))
    (write-file filename text)))

(defun write-subscriber (email action)
  "Save subscriber/unsubscriber to a file."
  (let* ((time-string (current-utc-time-string))
         (text (format nil "~a ~a~%" time-string email))
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

(defvar *input-info-line*
  "Your input has been left intact below in case you want to copy,
edit, or resubmit it.")

(defvar *comment-success-lines*
  (list "Comment was submitted successfully."
        "It is now awaiting review and it may be published after review."
        *input-info-line*))

(defun comment-form ()
  "Return HTML response to the request handler for comments."
  (let* ((page-layout (read-file "layout/page.html"))
         (form-layout (read-file "layout/form/comment.html"))
         (post-key "post")
         (name-key "name")
         (url-key "url")
         (comment-key "comment")
         (slug-key "slug")
         (info-key "email")
         (meta-key "meta")
         (hash-key "hash")
         (method (hunchentoot:request-method*))
         (error-lines)
         (info-lines)
         (success-lines)
         (params))
    ;; Common parameters for page.
    (add-page-params params)
    ;; Static values for form.
    (add-value "post-key" post-key params)
    (add-value "name-key" name-key params)
    (add-value "url-key" url-key params)
    (add-value "comment-key" comment-key params)
    (add-value "slug-key" slug-key params)
    (add-value "info-key" info-key params)
    (add-value "meta-key" meta-key params)
    (add-value "hash-key" hash-key params)
    (add-value "post-value" (or (from-get post-key) "") params)
    (add-value "slug-value" (or (from-get post-key) "") params)
    (add-value "info-value" "" params)
    (add-value "meta-value" "" params)
    (add-value "hash-value" "" params)
    (add-value "submit-value" "Submit Comment" params)
    ;; Preserve submitted values, if any.
    (add-value "name-value" (or (from-post name-key) "") params)
    (add-value "url-value" (or (from-post url-key) "") params)
    (add-value "comment-value" (or (from-post comment-key) "") params)
    ;; Handle request.
    (when (eq method :post)
      ;; If required fields are missing, reject the request.
      (when (or (string= (get-value "post-value" params) "")
                (string= (get-value "name-value" params) "")
                (string= (get-value "comment-value" params) ""))
        (push "Invalid request." error-lines))
      ;; Check metadata.
      (when (or (string/= (from-post slug-key) (from-get post-key))
                (string/= (from-post info-key) "")
                (string/= (from-post meta-key) "")
                (string/= (from-post hash-key) ""))
        (push "Some metadata is missing." info-lines))
      ;; If no error, set success message.
      (unless error-lines
        (setf success-lines *comment-success-lines*)))
    ;; Create response.
    (cond
      (error-lines
       (add-value "title" "Post Comment" params)
       (add-value "class" "error" params)
       (add-value "status" (format-status error-lines) params))
      (info-lines
       (add-value "title" "Comment Submitted" params)
       (add-value "class" "success" params)
       (add-value "status" (format-status success-lines) params))
      (success-lines
       (write-comment params)
       (add-value "title" "Comment Submitted" params)
       (add-value "class" "success" params)
       (add-value "status" (format-status success-lines) params))
      (t
       (add-value "title" "Post Comment" params)
       (add-value "class" "" params)
       (add-value "status" "" params)))
    ;; Render response.
    (setf form-layout (render page-layout (list (cons "body" form-layout))))
    (render form-layout params)))

(defun subscribe-form (action)
  "Return HTML response to the request handler for subscribe/unsubscribe form."
  (let ((page-layout (read-file "layout/page.html"))
        (form-layout (read-file (format nil "layout/form/subscribe.html")))
        (subscribers (+ 232 27))
        (email-key "email")
        (info-key "comment")
        (meta-key "meta")
        (hash-key "hash")
        (method (hunchentoot:request-method*))
        (success-line (format nil "Successfully ~ad." action))
        (error-lines)
        (info-lines)
        (success-lines)
        (params))
    ;; Common parameters for page.
    (add-page-params params)
    ;; Static values for form.
    (add-value "email-key" email-key params)
    (add-value "info-key" info-key params)
    (add-value "meta-key" meta-key params)
    (add-value "hash-key" hash-key params)
    (add-value "info-value" "" params)
    (add-value "meta-value" "" params)
    (add-value "hash-value" "" params)
    ;; Separate values for subscribe and unsubscribe forms.
    (cond
      ((string= action "subscribe")
       (add-value "purpose" (format nil "join ~a other subscribers and receive"
                                    subscribers) params)
       (add-value "submit-value" "Subscribe Now!" params))
      (t
       (add-value "purpose" "stop receiving" params)
       (add-value "submit-value" "Unsubscribe" params)))
    ;; Preserve submitted values, if any.
    (add-value "email-value" (or (from-post email-key) "") params)
    ;; Handle request.
    (when (eq method :post)
      ;; If required fields are missing, reject the request.
      (when (string= (get-value "email-value" params) "")
        (push "Invalid request." error-lines))
      ;; Check metadata.
      (when (or (string/= (from-post info-key) "")
                (string/= (from-post meta-key) "")
                (string/= (from-post hash-key) ""))
        (push "Some metdata is missing." info-lines))
      ;; If no error, set success message.
      (unless error-lines
        (setf success-lines (list success-line *input-info-line*))))
    ;; Create response.
    (cond
      (error-lines
       (add-value "title" (string-capitalize action) params)
       (add-value "class" "error" params)
       (add-value "status" (format-status error-lines) params))
      (info-lines
       (add-value "title" (string-capitalize success-line) params)
       (add-value "class" "success" params)
       (add-value "status" (format-status success-lines) params))
      (success-lines
       (write-subscriber (get-value "email-value" params) action)
       (add-value "title" (string-capitalize success-line) params)
       (add-value "class" "success" params)
       (add-value "status" (format-status success-lines) params))
      (t
       (add-value "title" (string-capitalize action) params)
       (add-value "class" "" params)
       (add-value "status" "" params)))
    ;; Render response.
    (setf form-layout (render page-layout (list (cons "body" form-layout))))
    (render form-layout params)))

(hunchentoot:define-easy-handler (index :uri "/form/") ()
  (when (member (hunchentoot:request-method*) '(:head :get))
    (form-index-page)))

(hunchentoot:define-easy-handler (comment :uri "/form/comment/") ()
  (when (member (hunchentoot:request-method*) '(:head :get :post))
    (comment-form)))

(hunchentoot:define-easy-handler (subscribe :uri "/form/subscribe/") ()
  (when (member (hunchentoot:request-method*) '(:head :get :post))
    (subscribe-form "subscribe")))

(hunchentoot:define-easy-handler (unsubscribe :uri "/form/unsubscribe/") ()
  (when (member (hunchentoot:request-method*) '(:head :get :post))
    (subscribe-form "unsubscribe")))

(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                  :address "127.0.0.1" :port 4242))
(setf (hunchentoot:acceptor-document-root *acceptor*) #p"_site/")
(hunchentoot:start *acceptor*)
(sleep most-positive-fixnum)
