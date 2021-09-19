(ql:quickload "hunchentoot")
(defvar *main-mode* nil)
(load "site.lisp")

(setf *random-state* (make-random-state t))

(defun current-utc-date-string ()
  "Return current UTC date in yyyy-mm-dd format."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore sec min hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun current-utc-time-string ()
  "Return current UTC time in hh:mm:ss format."
  (multiple-value-bind (sec min hour)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour min sec)))

(defun write-comment (params)
  "Save comment to a file."
  (let ((text (with-output-to-string (s)
                (format s "post: ~a~%" (get-value "post" params))
                (format s "user-agent: ~a~%" (hunchentoot:user-agent))
                (format s "remote-addr: ~a~%" (hunchentoot:remote-addr*))
                (format s "x-real-ip: ~a~%" (hunchentoot:header-in* :x-real-ip))
                (format s "<!-- date: ~a ~a +0000 -->~%"
                        (current-utc-date-string)
                        (current-utc-time-string))
                (format s "<!-- name: ~a -->~%" (get-value "name" params))
                (when (string/= (get-value "url" params) "")
                  (format s "<!-- url: ~a -->~%" (get-value "url" params)))
                (format s "~a~%" (get-value "comment" params))))
        (filename (format nil "/opt/cache/c_~a_~a_~a_~a.txt"
                          (get-value "slug" params)
                          (current-utc-date-string)
                          (string-replace ":" "-" (current-utc-time-string))
                          (random 1000000))))
    (write-file filename text)))

(defun write-subscriber (email action)
  "Save subscriber/unsubscriber to a file."
  (let ((text (format nil "~a~%" email))
        (filename (format nil "/opt/cache/~a_~a_~a_~a.txt"
                          action
                          (current-utc-date-string)
                          (string-replace ":" "-" (current-utc-time-string))
                          (random 1000000))))
    (write-file filename text)))

(defun comment-form-page (method post slug name url comment email)
  "Return HTML response to the request handler for comments."
  (let ((page-layout (read-file "layout/page.html"))
        (form-layout (read-file "layout/forms/comment.html"))
        (success-lines '("Comment was submitted successfully."
                         "It may be published after review."))
        (error-lines)
        (status-lines)
        (params))
    (cond
      ;; Handle GET request.
      ((eq method :get)
       ;; Set empty form.
       (add-value "post" (or post "") params)
       (add-value "slug" (or post "") params)
       (add-value "name" "" params)
       (add-value "url" "" params)
       (add-value "comment" "" params)
       (add-value "email" "" params)
       (add-value "class" "" params)
       (add-value "status" "" params))
      ;; Handle POST request.
      ((eq method :post)
       ;; Preserve posted form data.
       (add-value "post" (or post "") params)
       (add-value "slug" (or post "") params)
       (add-value "name" (or name "") params)
       (add-value "url" (or url "") params)
       (add-value "comment" (or comment "") params)
       ;; Validate post data.
       (when (or (string= post "") (string/= slug post) (string/= email ""))
         (push "Invalid request." error-lines))
       (when (string= (get-value "name" params) "")
         (push "You must mention your name." error-lines))
       (when (string= (get-value "comment" params) "")
         (push "You must write a comment message." error-lines))
       (cond (error-lines
              (add-value "class" "error" params)
              (setf status-lines error-lines))
             (t
              (add-value "class" "success" params)
              (setf status-lines success-lines)
              (write-comment params)))
       (setf status-lines (loop for x in status-lines
                                collect (format nil "<li>~a</li>~%" x)))
       (setf status-lines (join-strings status-lines))
       (setf status-lines (format nil "<ul>~%~a</ul>~%" status-lines))
       (add-value "status" status-lines params)))
    ;; Add page parameters.
    (add-value "root" "../../" params)
    (add-value "title" "Post Comment" params)
    (add-value "subtitle" " - Susam Pal" params)
    (add-value "site-url" "https://susam.in/" params)
    (add-value "current-year" (nth-value 5 (get-decoded-time)) params)
    (add-value "import" "form.css" params)
    (add-imports params)
    ;; Render form layout.
    (setf form-layout (render page-layout (list (cons "body" form-layout))))
    (render form-layout params)))

(defun comment-subscribe-page (method email action)
  "Return HTML response to the request handler for subscribe/unsubscribe form."
  (let ((page-layout (read-file "layout/page.html"))
        (form-layout (read-file (format nil "layout/forms/~a.html" action)))
        (params))
    (cond
      ;; Handle GET request.
      ((eq method :get)
       (add-value "class" "" params)
       (add-value "email" "" params)
       (add-value "readonly" "" params))
      ;; Handle POST request.
      ((eq method :post)
       (add-value "email" (or email "") params)
       (cond
         ((string= (get-value "email" params) "")
          (add-value "class" "error" params)
          (add-value "readonly" "" params)
          (add-value "status" "<p>You must enter your email address!</p>" params))
         (t
          (add-value "class" "success" params)
          (add-value "readonly" "readonly" params)
          (add-value "status" (format nil "<p>Successfully ~ad!</p>" action) params)
          (write-subscriber email action)))))
    ;; Add page parameters.
    (add-value "root" "../../" params)
    (add-value "title" (string-capitalize action) params)
    (add-value "subtitle" " - Susam Pal" params)
    (add-value "site-url" "https://susam.in/" params)
    (add-value "current-year" (nth-value 5 (get-decoded-time)) params)
    (add-value "import" "form.css" params)
    (add-imports params)
    ;; Render form layout.
    (setf form-layout (render page-layout (list (cons "body" form-layout))))
    (render form-layout params)))

(hunchentoot:define-easy-handler (comment :uri "/app/comment/")
    ((post :request-type :get)
     (slug :request-type :post)
     (name :request-type :post)
     (url :request-type :post)
     (comment :request-type :post)
     (email :request-type :post))
  (let ((method (hunchentoot:request-method*)))
    (when (member method '(:head :get :post))
      (comment-form-page method post slug name url comment email))))

(hunchentoot:define-easy-handler (subscribe :uri "/app/subscribe/")
    ((email :request-type :post))
  (let ((method (hunchentoot:request-method*)))
    (when (member method '(:head :get :post))
      (comment-subscribe-page method email "subscribe"))))

(hunchentoot:define-easy-handler (unsubscribe :uri "/app/unsubscribe/")
    ((email :request-type :post))
  (let ((method (hunchentoot:request-method*)))
    (when (member method '(:head :get :post))
      (comment-subscribe-page method email "unsubscribe"))))

(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                  :address "127.0.0.1" :port 4242))
(setf (hunchentoot:acceptor-document-root *acceptor*) #p"static/")
(hunchentoot:start *acceptor*)
(sleep most-positive-fixnum)
