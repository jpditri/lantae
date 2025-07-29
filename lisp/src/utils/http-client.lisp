;;;; http-client.lisp - HTTP client wrapper for Lantae LISP
;;;;
;;;; Provides a functional HTTP client interface with:
;;;; - JSON request/response handling
;;;; - Error handling with result monad
;;;; - Retry logic
;;;; - Streaming support

(defpackage :lantae-http
  (:use :cl)
  (:export #:http-request
           #:http-get
           #:http-post
           #:http-stream
           #:with-http-error-handling
           #:parse-json-response))

(in-package :lantae-http)

;;; Dependencies check
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (progn
        (require :drakma)
        (require :cl-json)
        (require :flexi-streams))
    (error ()
      (format t "~%WARNING: Required libraries not found.~%")
      (format t "Please install: (ql:quickload '(:drakma :cl-json :flexi-streams))~%~%"))))

;;; Result type for error handling
(defstruct http-result
  success-p
  data
  error
  status-code
  headers)

(defun http-success (data &key (status-code 200) headers)
  "Create successful HTTP result"
  (make-http-result :success-p t 
                    :data data 
                    :status-code status-code
                    :headers headers))

(defun http-failure (error &key status-code)
  "Create failed HTTP result"
  (make-http-result :success-p nil 
                    :error error
                    :status-code status-code))

;;; JSON handling
(defun encode-json (data)
  "Encode data to JSON string"
  (handler-case
      (json:encode-json-to-string data)
    (error (e)
      (error "Failed to encode JSON: ~A" e))))

(defun decode-json (json-string)
  "Decode JSON string to Lisp data"
  (handler-case
      (json:decode-json-from-string json-string)
    (error (e)
      (error "Failed to decode JSON: ~A" e))))

(defun parse-json-response (response)
  "Parse JSON response body"
  (if (stringp response)
      (decode-json response)
      (decode-json (flexi-streams:octets-to-string response :external-format :utf-8))))

;;; HTTP request wrapper
(defun http-request (url &key 
                         (method :get)
                         headers
                         content
                         (content-type "application/json")
                         (timeout 30)
                         (retries 3))
  "Make HTTP request with error handling"
  (let ((attempt 0)
        (delay 1))
    (loop
      (handler-case
          (multiple-value-bind (body status-code response-headers uri stream must-close reason)
              (drakma:http-request url
                                   :method method
                                   :content-type content-type
                                   :content (when content
                                            (if (stringp content)
                                                content
                                                (encode-json content)))
                                   :additional-headers headers
                                   :want-stream nil
                                   :connection-timeout timeout)
            (declare (ignore uri stream must-close))
            
            (cond
              ;; Success
              ((and (>= status-code 200) (< status-code 300))
               (return (http-success body 
                                   :status-code status-code
                                   :headers response-headers)))
              
              ;; Client error
              ((and (>= status-code 400) (< status-code 500))
               (return (http-failure (format nil "HTTP ~A: ~A" status-code reason)
                                   :status-code status-code)))
              
              ;; Server error - retry
              ((>= status-code 500)
               (error "Server error: HTTP ~A" status-code))
              
              ;; Other
              (t
               (return (http-failure (format nil "Unexpected status: HTTP ~A" status-code)
                                   :status-code status-code)))))
        
        (error (e)
          (incf attempt)
          (if (>= attempt retries)
              (return (http-failure (format nil "Request failed after ~A attempts: ~A" 
                                          retries e)))
              (progn
                (format t "Attempt ~A failed, retrying in ~A seconds...~%" 
                        attempt delay)
                (sleep delay)
                (setf delay (* delay 2))))))))

;;; Convenience functions
(defun http-get (url &key headers (timeout 30))
  "Make GET request"
  (http-request url :method :get :headers headers :timeout timeout))

(defun http-post (url data &key headers (timeout 30))
  "Make POST request with JSON data"
  (http-request url 
                :method :post 
                :content data
                :headers headers
                :timeout timeout))

;;; Streaming support
(defun http-stream (url &key 
                        (method :post)
                        headers
                        content
                        (content-type "application/json")
                        callback
                        (timeout 30))
  "Make streaming HTTP request"
  (handler-case
      (let ((stream (drakma:http-request url
                                         :method method
                                         :content-type content-type
                                         :content (when content
                                                  (if (stringp content)
                                                      content
                                                      (encode-json content)))
                                         :additional-headers headers
                                         :want-stream t
                                         :connection-timeout timeout)))
        (unwind-protect
             (loop for line = (read-line stream nil nil)
                   while line
                   do (when callback
                        (funcall callback line)))
          (close stream))
        (http-success t))
    (error (e)
      (http-failure (format nil "Streaming failed: ~A" e)))))

;;; Error handling macro
(defmacro with-http-error-handling (&body body)
  "Execute body with HTTP error handling"
  `(handler-case
       (progn ,@body)
     (error (e)
       (http-failure (format nil "HTTP operation failed: ~A" e)))))

;;; Provider-specific helpers
(defun build-chat-request (model messages &key (temperature 0.7) (stream nil))
  "Build standard chat request format"
  `((:model . ,model)
    (:messages . ,(coerce messages 'vector))
    (:temperature . ,temperature)
    (:stream . ,stream)))

(defun extract-chat-response (response)
  "Extract content from chat response"
  (let ((parsed (parse-json-response response)))
    (or (cdr (assoc :content (cdr (assoc :message parsed))))
        (cdr (assoc :response parsed))
        (cdr (assoc :text parsed))
        "No response content found")))

;;; URL builders
(defun build-url (base-path &rest path-components)
  "Build URL from base and path components"
  (format nil "~A~{/~A~}" 
          (string-right-trim "/" base-path)
          (mapcar (lambda (p) (string-trim "/" (string p))) path-components)))

(defun add-query-params (url params)
  "Add query parameters to URL"
  (if params
      (format nil "~A?~{~A=~A~^&~}"
              url
              (loop for (key value) on params by #'cddr
                    collect (string-downcase (string key))
                    collect (if (stringp value)
                               value
                               (format nil "~A" value))))
      url))