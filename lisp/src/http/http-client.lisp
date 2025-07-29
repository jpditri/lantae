;;;; http-client.lisp - HTTP client wrapper for API calls
;;;;
;;;; This module provides:
;;;; - Unified HTTP client interface
;;;; - JSON serialization/deserialization
;;;; - Error handling and retries
;;;; - Request/response logging
;;;; - Streaming support

(defpackage :lantae-http
  (:use :cl)
  (:export #:http-request
           #:http-get
           #:http-post
           #:http-put
           #:http-delete
           #:with-http-retries
           #:parse-json-response
           #:json-encode
           #:make-http-client
           #:*default-timeout*
           #:*default-retries*))

(in-package :lantae-http)

;;; Dependencies - using built-in or minimal external libs
(defvar *default-timeout* 30
  "Default timeout in seconds for HTTP requests")

(defvar *default-retries* 3
  "Default number of retries for failed requests")

;;; JSON utilities
(defun json-encode (data)
  "Encode Lisp data structure to JSON string"
  (cond
    ((null data) "null")
    ((eq data t) "true")
    ((stringp data) (format nil "~S" data))
    ((numberp data) (format nil "~A" data))
    ((listp data)
     (if (and (listp (car data)) 
              (keywordp (caar data)))
         ;; Object (plist)
         (format nil "{~{~A~^,~}}"
                 (loop for (key value) on data by #'cddr
                       collect (format nil "~S:~A" 
                                     (string-downcase (string key))
                                     (json-encode value))))
         ;; Array
         (format nil "[~{~A~^,~}]"
                 (mapcar #'json-encode data))))
    ((hash-table-p data)
     (format nil "{~{~A~^,~}}"
             (loop for key being the hash-keys of data
                   using (hash-value value)
                   collect (format nil "~S:~A"
                                 (string-downcase (string key))
                                 (json-encode value)))))
    (t (format nil "~S" data))))

(defun parse-json-response (json-string)
  "Parse JSON string to Lisp data structure"
  ;; Simple JSON parser - in production would use a library like cl-json
  (parse-json-internal json-string))

(defun parse-json-internal (string)
  "Internal JSON parser implementation"
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) string)))
    (cond
      ((string= trimmed "null") nil)
      ((string= trimmed "true") t)
      ((string= trimmed "false") nil)
      ((char= (char trimmed 0) #\")
       ;; String
       (read-from-string trimmed))
      ((char= (char trimmed 0) #\{)
       ;; Object - parse as plist
       (parse-json-object trimmed))
      ((char= (char trimmed 0) #\[)
       ;; Array
       (parse-json-array trimmed))
      ((or (digit-char-p (char trimmed 0))
           (char= (char trimmed 0) #\-)
           (char= (char trimmed 0) #\+))
       ;; Number
       (read-from-string trimmed))
      (t (error "Invalid JSON: ~A" trimmed)))))

(defun parse-json-object (string)
  "Parse JSON object to plist"
  ;; Simplified parser - extracts key-value pairs
  (let ((content (subseq string 1 (1- (length string))))
        (result '()))
    ;; This is a simplified implementation
    ;; In production, use a proper JSON library
    (loop for pair in (split-string content #\,)
          do (let* ((colon-pos (position #\: pair))
                    (key (intern (string-upcase 
                                (string-trim '(#\Space #\Tab #\") 
                                           (subseq pair 0 colon-pos)))
                               :keyword))
                    (value (parse-json-internal 
                          (string-trim '(#\Space #\Tab) 
                                     (subseq pair (1+ colon-pos))))))
               (push value result)
               (push key result)))
    (nreverse result)))

(defun parse-json-array (string)
  "Parse JSON array to list"
  (let ((content (subseq string 1 (1- (length string)))))
    (mapcar #'parse-json-internal
            (split-string content #\,))))

(defun split-string (string delimiter)
  "Split string by delimiter character"
  (let ((parts '())
        (current "")
        (depth 0)
        (in-string nil))
    (loop for char across string
          do (cond
               ((and (char= char #\") (not (char= (char string (1- (position char string))) #\\)))
                (setf in-string (not in-string))
                (setf current (concatenate 'string current (string char))))
               ((and (not in-string) (member char '(#\{ #\[)))
                (incf depth)
                (setf current (concatenate 'string current (string char))))
               ((and (not in-string) (member char '(#\} #\])))
                (decf depth)
                (setf current (concatenate 'string current (string char))))
               ((and (char= char delimiter) (zerop depth) (not in-string))
                (push (string-trim '(#\Space #\Tab) current) parts)
                (setf current ""))
               (t
                (setf current (concatenate 'string current (string char))))))
    (when (> (length current) 0)
      (push (string-trim '(#\Space #\Tab) current) parts))
    (nreverse parts)))

;;; HTTP client structure
(defstruct http-client
  "HTTP client configuration"
  timeout
  retries
  headers
  log-requests-p)

(defun make-default-http-client ()
  "Create HTTP client with default settings"
  (make-http-client :timeout *default-timeout*
                    :retries *default-retries*
                    :headers '()
                    :log-requests-p nil))

;;; HTTP request functions
(defun http-request (method url &key 
                              (body nil)
                              (headers '())
                              (timeout *default-timeout*)
                              (client (make-default-http-client)))
  "Make HTTP request using built-in capabilities"
  (when (http-client-log-requests-p client)
    (format t "~&HTTP ~A ~A~%" method url))
  
  ;; Implementation using implementation-specific HTTP client
  ;; For SBCL, we could use sb-bsd-sockets
  ;; For now, we'll use a system call to curl as a portable solution
  (http-request-curl method url body headers timeout))

(defun http-request-curl (method url body headers timeout)
  "HTTP request implementation using curl"
  (let* ((temp-file (format nil "/tmp/lantae-http-~A.tmp" (get-universal-time)))
         (header-args (loop for (key . value) in headers
                           append (list "-H" (format nil "~A: ~A" key value))))
         (method-arg (list "-X" (string method)))
         (data-args (when body
                     (list "-d" body)))
         (timeout-arg (list "--max-time" (format nil "~A" timeout)))
         (cmd-args (append (list "curl" "-s" "-S" "-o" temp-file "-w" "%{http_code}")
                          method-arg
                          header-args
                          data-args
                          timeout-arg
                          (list url))))
    
    (unwind-protect
        (multiple-value-bind (output error-output exit-code)
            (run-program-capture-output cmd-args)
          (if (zerop exit-code)
              (let ((status-code (parse-integer output :junk-allowed t))
                    (response-body (read-file-string temp-file)))
                (values response-body status-code))
              (error "HTTP request failed: ~A" error-output)))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(defun run-program-capture-output (args)
  "Run external program and capture output"
  #+sbcl
  (let* ((process (sb-ext:run-program (first args) (rest args)
                                     :output :stream
                                     :error :stream
                                     :wait t))
         (output (read-stream-to-string (sb-ext:process-output process)))
         (error-output (read-stream-to-string (sb-ext:process-error process)))
         (exit-code (sb-ext:process-exit-code process)))
    (values output error-output exit-code))
  #+ccl
  (multiple-value-bind (output error-output exit-code)
      (ccl:run-program (first args) (rest args) 
                      :output :stream 
                      :error :stream)
    (values output error-output exit-code))
  #-(or sbcl ccl)
  (error "run-program not implemented for this Lisp implementation"))

(defun read-stream-to-string (stream)
  "Read entire stream to string"
  (with-output-to-string (out)
    (loop for line = (read-line stream nil nil)
          while line
          do (write-line line out))))

(defun read-file-string (filename)
  "Read entire file as string"
  (with-open-file (stream filename :if-does-not-exist nil)
    (when stream
      (let ((data (make-string (file-length stream))))
        (read-sequence data stream)
        data))))

;;; Convenience functions
(defun http-get (url &key headers (timeout *default-timeout*))
  "Make HTTP GET request"
  (http-request :GET url :headers headers :timeout timeout))

(defun http-post (url body &key headers (timeout *default-timeout*))
  "Make HTTP POST request"
  (http-request :POST url :body body :headers headers :timeout timeout))

(defun http-put (url body &key headers (timeout *default-timeout*))
  "Make HTTP PUT request"
  (http-request :PUT url :body body :headers headers :timeout timeout))

(defun http-delete (url &key headers (timeout *default-timeout*))
  "Make HTTP DELETE request"
  (http-request :DELETE url :headers headers :timeout timeout))

;;; Retry logic
(defmacro with-http-retries ((retries &key (delay 1)) &body body)
  "Execute body with retry logic for HTTP requests"
  (let ((retry-count (gensym))
        (retry-delay (gensym))
        (result (gensym)))
    `(let ((,retry-count ,retries)
           (,retry-delay ,delay))
       (loop for attempt from 1 to ,retry-count
             do (handler-case
                    (return (progn ,@body))
                  (error (e)
                    (if (< attempt ,retry-count)
                        (progn
                          (format t "~&Attempt ~A failed: ~A~%" attempt e)
                          (format t "Retrying in ~A seconds...~%" ,retry-delay)
                          (sleep ,retry-delay)
                          (setf ,retry-delay (* ,retry-delay 2)))
                        (error e))))))))

;;; Streaming support
(defun http-stream-request (method url &key body headers callback)
  "Make streaming HTTP request with callback for chunks"
  ;; This would require more sophisticated HTTP client
  ;; For now, return a placeholder
  (error "Streaming not yet implemented"))