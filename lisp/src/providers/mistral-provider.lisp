;;;; mistral-provider.lisp - Mistral AI provider implementation
;;;;
;;;; Implements the Mistral AI provider with:
;;;; - Chat completion API
;;;; - Model listing
;;;; - Error handling
;;;; - Support for multiple Mistral models

(defpackage :lantae-providers-mistral
  (:use :cl :lantae-http :lantae-providers)
  (:export #:make-mistral-provider
           #:mistral-chat
           #:mistral-stream
           #:mistral-list-models
           #:test-mistral-connection))

(in-package :lantae-providers-mistral)

;;; Mistral API configuration
(defparameter *mistral-base-url* "https://api.mistral.ai/v1"
  "Base URL for Mistral AI API")

(defparameter *mistral-models*
  '("mistral-large-latest" "mistral-medium-latest" "mistral-small-latest"
    "codestral-latest" "mistral-embed")
  "Available Mistral models")

(defun get-endpoint (endpoint)
  "Get full URL for Mistral endpoint"
  (build-url *mistral-base-url* endpoint))

(defun get-api-key ()
  "Get Mistral API key from environment"
  (or #+sbcl (sb-ext:posix-getenv "MISTRAL_API_KEY")
      #-sbcl nil
      (error "Mistral API key not found. Set MISTRAL_API_KEY environment variable.")))

(defun format-mistral-headers (api-key)
  "Format headers for Mistral API requests"
  (list (cons "Authorization" (format nil "Bearer ~A" api-key))
        (cons "Content-Type" "application/json")))

(defun format-mistral-message (message)
  "Format message for Mistral API"
  (let ((role (if (listp message) (getf message :role) "user"))
        (content (if (listp message) (getf message :content) message)))
    (list (cons :role role)
          (cons :content (if (stringp content) 
                             content 
                             (format nil "~A" content))))))

(defun handle-mistral-error (status-code response-body)
  "Handle Mistral API errors"
  (case status-code
    (400 "Bad request to Mistral API. Check your request format.")
    (401 "Invalid Mistral API key. Check your credentials.")
    (403 "Access forbidden. Check your Mistral API permissions.")
    (429 "Rate limit exceeded for Mistral. Please wait and try again.")
    (500 "Mistral API internal error. Please try again later.")
    (t (format nil "Mistral API error ~A: ~A" status-code response-body))))

(defun parse-mistral-response (response-body)
  "Parse Mistral API response"
  (handler-case
      (let* ((json-data (decode-json response-body))
             (choices (cdr (assoc :choices json-data)))
             (first-choice (first choices))
             (message (cdr (assoc :message first-choice)))
             (content (cdr (assoc :content message))))
        (success (list :content content)))
    (error (e)
      (failure (format nil "Failed to parse Mistral response: ~A" e)))))

(defun mistral-chat (api-key model messages temperature)
  "Send chat request to Mistral API"
  (let* ((formatted-messages (mapcar #'format-mistral-message messages))
         (request-body (encode-json 
                        (list (cons :model model)
                              (cons :messages formatted-messages)
                              (cons :temperature temperature)
                              (cons :max_tokens 1000))))
         (url (get-endpoint "chat/completions"))
         (headers (format-mistral-headers api-key)))
    
    (handler-case
        (multiple-value-bind (response-body status-code)
            (http-request url 
                         :method :post
                         :headers headers
                         :body request-body)
          (if (= status-code 200)
              (parse-mistral-response response-body)
              (failure (handle-mistral-error status-code response-body))))
      (error (e)
        (failure (format nil "Mistral API request failed: ~A" e))))))

(defun mistral-stream (api-key model messages temperature callback)
  "Stream chat response from Mistral API"
  (let* ((formatted-messages (mapcar #'format-mistral-message messages))
         (request-body (encode-json 
                        (list (cons :model model)
                              (cons :messages formatted-messages)
                              (cons :temperature temperature)
                              (cons :max_tokens 1000)
                              (cons :stream t))))
         (url (get-endpoint "chat/completions"))
         (headers (format-mistral-headers api-key)))
    
    (handler-case
        (http-stream url 
                     :method :post
                     :headers headers
                     :content request-body
                     :callback (lambda (chunk)
                                 (when chunk
                                   (let ((parsed (parse-mistral-stream-chunk chunk)))
                                     (when parsed
                                       (funcall callback parsed))))))
      (error (e)
        (failure (format nil "Mistral streaming request failed: ~A" e))))))

(defun parse-mistral-stream-chunk (chunk)
  "Parse streaming chunk from Mistral"
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

(defun mistral-list-models ()
  "List available Mistral models"
  (success *mistral-models*))

(defun test-mistral-connection (&optional api-key)
  "Test connection to Mistral API"
  (let ((key (or api-key (get-api-key))))
    (handler-case
        (let ((result (mistral-chat key "mistral-small-latest" 
                                    (list (list :role "user" :content "Hello"))
                                    0.1)))
          (if (result-success-p result)
              (format t "✅ Mistral connection successful~%")
              (format t "❌ Mistral connection failed: ~A~%" (result-error result))))
      (error (e)
        (format t "❌ Mistral connection error: ~A~%" e)))))

(defun make-mistral-provider (&key api-key)
  "Create Mistral provider"
  (let ((key (or api-key (get-api-key))))
    (create-provider
     :name "mistral"
     :chat-fn (lambda (model messages temperature)
                (mistral-chat key model messages temperature))
     :stream-fn (lambda (model messages temperature callback)
                  (mistral-stream key model messages temperature callback))
     :models-fn #'mistral-list-models
     :config `(:api-key ,key))))