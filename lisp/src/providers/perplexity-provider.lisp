;;;; perplexity-provider.lisp - Perplexity AI provider implementation
;;;;
;;;; Implements the Perplexity AI provider with:
;;;; - Chat completion API
;;;; - Model listing
;;;; - Error handling
;;;; - Support for multiple Perplexity models

(defpackage :lantae-providers-perplexity
  (:use :cl :lantae-http :lantae-providers)
  (:export #:make-perplexity-provider
           #:perplexity-chat
           #:perplexity-stream
           #:perplexity-list-models
           #:test-perplexity-connection))

(in-package :lantae-providers-perplexity)

;;; Perplexity API configuration
(defparameter *perplexity-base-url* "https://api.perplexity.ai"
  "Base URL for Perplexity AI API")

(defparameter *perplexity-models*
  '("llama-3.1-sonar-small-128k-online" "llama-3.1-sonar-large-128k-online"
    "llama-3.1-sonar-huge-128k-online" "llama-3.1-sonar-small-128k-chat"
    "llama-3.1-sonar-large-128k-chat" "llama-3.1-8b-instruct"
    "llama-3.1-70b-instruct" "mixtral-8x7b-instruct")
  "Available Perplexity models")

(defun get-endpoint (endpoint)
  "Get full URL for Perplexity endpoint"
  (build-url *perplexity-base-url* endpoint))

(defun get-api-key ()
  "Get Perplexity API key from environment"
  (or #+sbcl (sb-ext:posix-getenv "PERPLEXITY_API_KEY")
      #-sbcl nil
      (error "Perplexity API key not found. Set PERPLEXITY_API_KEY environment variable.")))

(defun format-perplexity-headers (api-key)
  "Format headers for Perplexity API requests"
  (list (cons "Authorization" (format nil "Bearer ~A" api-key))
        (cons "Content-Type" "application/json")))

(defun format-perplexity-message (message)
  "Format message for Perplexity API"
  (let ((role (if (listp message) (getf message :role) "user"))
        (content (if (listp message) (getf message :content) message)))
    (list (cons :role role)
          (cons :content (if (stringp content) 
                             content 
                             (format nil "~A" content))))))

(defun handle-perplexity-error (status-code response-body)
  "Handle Perplexity API errors"
  (case status-code
    (400 "Bad request to Perplexity API. Check your request format.")
    (401 "Invalid Perplexity API key. Check your credentials.")
    (403 "Access forbidden. Check your Perplexity API permissions.")
    (429 "Rate limit exceeded for Perplexity. Please wait and try again.")
    (500 "Perplexity API internal error. Please try again later.")
    (t (format nil "Perplexity API error ~A: ~A" status-code response-body))))

(defun parse-perplexity-response (response-body)
  "Parse Perplexity API response"
  (handler-case
      (let* ((json-data (decode-json response-body))
             (choices (cdr (assoc :choices json-data)))
             (first-choice (first choices))
             (message (cdr (assoc :message first-choice)))
             (content (cdr (assoc :content message))))
        (success (list :content content)))
    (error (e)
      (failure (format nil "Failed to parse Perplexity response: ~A" e)))))

(defun perplexity-chat (api-key model messages temperature)
  "Send chat request to Perplexity API"
  (let* ((formatted-messages (mapcar #'format-perplexity-message messages))
         (request-body (encode-json 
                        (list (cons :model model)
                              (cons :messages formatted-messages)
                              (cons :temperature temperature)
                              (cons :max_tokens 1000))))
         (url (get-endpoint "chat/completions"))
         (headers (format-perplexity-headers api-key)))
    
    (handler-case
        (multiple-value-bind (response-body status-code)
            (http-request url 
                         :method :post
                         :headers headers
                         :body request-body)
          (if (= status-code 200)
              (parse-perplexity-response response-body)
              (failure (handle-perplexity-error status-code response-body))))
      (error (e)
        (failure (format nil "Perplexity API request failed: ~A" e))))))

(defun perplexity-stream (api-key model messages temperature callback)
  "Stream chat response from Perplexity API"
  (let* ((formatted-messages (mapcar #'format-perplexity-message messages))
         (request-body (encode-json 
                        (list (cons :model model)
                              (cons :messages formatted-messages)
                              (cons :temperature temperature)
                              (cons :max_tokens 1000)
                              (cons :stream t))))
         (url (get-endpoint "chat/completions"))
         (headers (format-perplexity-headers api-key)))
    
    (handler-case
        (http-stream url 
                     :method :post
                     :headers headers
                     :content request-body
                     :callback (lambda (chunk)
                                 (when chunk
                                   (let ((parsed (parse-perplexity-stream-chunk chunk)))
                                     (when parsed
                                       (funcall callback parsed))))))
      (error (e)
        (failure (format nil "Perplexity streaming request failed: ~A" e))))))

(defun parse-perplexity-stream-chunk (chunk)
  "Parse streaming chunk from Perplexity"
  (when (and chunk (> (length chunk) 6) (string= (subseq chunk 0 6) "data: "))
    (let ((json-str (subseq chunk 6)))
      (unless (string= json-str "[DONE]")
        (handler-case
            (let* ((data (decode-json json-str))
                   (choices (cdr (assoc :choices data)))
                   (first-choice (first choices))
                   (delta (cdr (assoc :delta first-choice)))
                   (content (cdr (assoc :content delta))))
              content)
          (error () nil))))))

(defun perplexity-list-models ()
  "List available Perplexity models"
  (success *perplexity-models*))

(defun test-perplexity-connection (&optional api-key)
  "Test connection to Perplexity API"
  (let ((key (or api-key (get-api-key))))
    (handler-case
        (let ((result (perplexity-chat key "llama-3.1-sonar-small-128k-chat" 
                                       (list (list :role "user" :content "Hello"))
                                       0.1)))
          (if (result-success-p result)
              (format t "✅ Perplexity connection successful~%")
              (format t "❌ Perplexity connection failed: ~A~%" (result-error result))))
      (error (e)
        (format t "❌ Perplexity connection error: ~A~%" e)))))

(defun make-perplexity-provider (&key api-key)
  "Create Perplexity provider"
  (let ((key (or api-key (get-api-key))))
    (create-provider
     :name "perplexity"
     :chat-fn (lambda (model messages temperature)
                (perplexity-chat key model messages temperature))
     :stream-fn (lambda (model messages temperature callback)
                  (perplexity-stream key model messages temperature callback))
     :models-fn #'perplexity-list-models
     :config `(:api-key ,key))))