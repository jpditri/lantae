;;;; gemini-provider.lisp - Google Gemini provider implementation
;;;;
;;;; Implements the Google Gemini provider with:
;;;; - Chat completion API via Google AI Platform
;;;; - Model listing
;;;; - Error handling
;;;; - Support for multiple Gemini models

(defpackage :lantae-providers-gemini
  (:use :cl :lantae-http :lantae-providers)
  (:export #:make-gemini-provider
           #:gemini-chat
           #:gemini-stream
           #:gemini-list-models
           #:test-gemini-connection))

(in-package :lantae-providers-gemini)

;;; Gemini API configuration
(defparameter *gemini-base-url* "https://generativelanguage.googleapis.com/v1beta"
  "Base URL for Google Gemini API")

(defparameter *gemini-models*
  '("gemini-1.5-pro" "gemini-1.5-flash" "gemini-1.0-pro" 
    "gemini-pro" "gemini-pro-vision")
  "Available Gemini models")

(defun get-endpoint (endpoint &rest args)
  "Get full URL for Gemini endpoint"
  (apply #'build-url *gemini-base-url* endpoint args))

(defun get-api-key ()
  "Get Gemini API key from environment"
  (or #+sbcl (sb-ext:posix-getenv "GEMINI_API_KEY")
      #-sbcl nil
      (error "Gemini API key not found. Set GEMINI_API_KEY environment variable.")))

(defun format-gemini-headers ()
  "Format headers for Gemini API requests"
  (list (cons "Content-Type" "application/json")))

(defun format-gemini-message (message)
  "Format message for Gemini API"
  (let ((role (if (listp message) (getf message :role) "user"))
        (content (if (listp message) (getf message :content) message)))
    (list 
     (cons :role (if (string= role "assistant") "model" role))
     (cons :parts (list (list (cons :text (if (stringp content) 
                                              content 
                                              (format nil "~A" content)))))))))

(defun handle-gemini-error (status-code response-body)
  "Handle Gemini API errors"
  (case status-code
    (400 "Bad request to Gemini API. Check your request format.")
    (401 "Invalid Gemini API key. Check your credentials.")
    (403 "Access forbidden. Check your Gemini API permissions.")
    (429 "Rate limit exceeded for Gemini. Please wait and try again.")
    (500 "Gemini API internal error. Please try again later.")
    (t (format nil "Gemini API error ~A: ~A" status-code response-body))))

(defun parse-gemini-response (response-body)
  "Parse Gemini API response"
  (handler-case
      (let* ((json-data (decode-json response-body))
             (candidates (cdr (assoc :candidates json-data)))
             (first-candidate (first candidates))
             (content (cdr (assoc :content first-candidate)))
             (parts (cdr (assoc :parts content)))
             (first-part (first parts))
             (text (cdr (assoc :text first-part))))
        (success (list :content text)))
    (error (e)
      (failure (format nil "Failed to parse Gemini response: ~A" e)))))

(defun gemini-chat (api-key model messages temperature)
  "Send chat request to Gemini API"
  (let* ((formatted-messages (mapcar #'format-gemini-message messages))
         (request-body (encode-json 
                        (list (cons :contents formatted-messages)
                              (cons :generation_config 
                                    (list (cons :temperature temperature))))))
         (url (get-endpoint (format nil "models/~A:generateContent" model)))
         (headers (format-gemini-headers)))
    
    (handler-case
        (multiple-value-bind (response-body status-code)
            (http-request url 
                         :method :post
                         :headers headers
                         :body request-body
                         :parameters (list (cons "key" api-key)))
          (if (= status-code 200)
              (parse-gemini-response response-body)
              (failure (handle-gemini-error status-code response-body))))
      (error (e)
        (failure (format nil "Gemini API request failed: ~A" e))))))

(defun gemini-stream (api-key model messages temperature callback)
  "Stream chat response from Gemini API"
  (let* ((formatted-messages (mapcar #'format-gemini-message messages))
         (request-body (encode-json 
                        (list (cons :contents formatted-messages)
                              (cons :generation_config 
                                    (list (cons :temperature temperature))))))
         (url (get-endpoint (format nil "models/~A:streamGenerateContent" model)))
         (headers (format-gemini-headers)))
    
    (handler-case
        (http-stream url 
                     :method :post
                     :headers headers
                     :content request-body
                     :parameters (list (cons "key" api-key))
                     :callback (lambda (chunk)
                                 (when chunk
                                   (let ((parsed (parse-gemini-stream-chunk chunk)))
                                     (when parsed
                                       (funcall callback parsed))))))
      (error (e)
        (failure (format nil "Gemini streaming request failed: ~A" e))))))

(defun parse-gemini-stream-chunk (chunk)
  "Parse streaming chunk from Gemini"
  (when (and chunk (> (length chunk) 6) (string= (subseq chunk 0 6) "data: "))
    (let ((json-str (subseq chunk 6)))
      (unless (string= json-str "[DONE]")
        (handler-case
            (let* ((data (decode-json json-str))
                   (candidates (cdr (assoc :candidates data)))
                   (first-candidate (first candidates))
                   (content (cdr (assoc :content first-candidate)))
                   (parts (cdr (assoc :parts content)))
                   (first-part (first parts))
                   (text (cdr (assoc :text first-part))))
              text)
          (error () nil))))))

(defun gemini-list-models ()
  "List available Gemini models"
  (success *gemini-models*))

(defun test-gemini-connection (&optional api-key)
  "Test connection to Gemini API"
  (let ((key (or api-key (get-api-key))))
    (handler-case
        (let ((result (gemini-chat key "gemini-pro" 
                                   (list (list :role "user" :content "Hello"))
                                   0.1)))
          (if (result-success-p result)
              (format t "✅ Gemini connection successful~%")
              (format t "❌ Gemini connection failed: ~A~%" (result-error result))))
      (error (e)
        (format t "❌ Gemini connection error: ~A~%" e)))))

(defun make-gemini-provider (&key api-key)
  "Create Gemini provider"
  (let ((key (or api-key (get-api-key))))
    (create-provider
     :name "gemini"
     :chat-fn (lambda (model messages temperature)
                (gemini-chat key model messages temperature))
     :stream-fn (lambda (model messages temperature callback)
                  (gemini-stream key model messages temperature callback))
     :models-fn #'gemini-list-models
     :config `(:api-key ,key))))