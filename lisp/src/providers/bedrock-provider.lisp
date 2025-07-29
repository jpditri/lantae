;;;; bedrock-provider.lisp - AWS Bedrock provider implementation
;;;;
;;;; Implements the AWS Bedrock provider with:
;;;; - Chat completion API via AWS Bedrock Runtime
;;;; - Model listing for supported models
;;;; - AWS IAM authentication
;;;; - Support for Claude, Titan, and other AWS models

(defpackage :lantae-providers-bedrock
  (:use :cl :lantae-http :lantae-providers)
  (:export #:make-bedrock-provider
           #:bedrock-chat
           #:bedrock-list-models
           #:test-bedrock-connection))

(in-package :lantae-providers-bedrock)

;;; AWS Bedrock configuration
(defparameter *bedrock-base-url* "https://bedrock-runtime.us-east-1.amazonaws.com"
  "Base URL for AWS Bedrock Runtime API")

(defparameter *bedrock-models*
  '("anthropic.claude-3-5-sonnet-20241022-v2:0"
    "anthropic.claude-3-5-haiku-20241022-v1:0"
    "anthropic.claude-3-opus-20240229-v1:0"
    "anthropic.claude-3-sonnet-20240229-v1:0"
    "anthropic.claude-3-haiku-20240307-v1:0"
    "amazon.titan-text-premier-v1:0"
    "amazon.titan-text-express-v1"
    "cohere.command-text-v14"
    "ai21.j2-ultra-v1"
    "meta.llama2-70b-chat-v1")
  "Available AWS Bedrock models")

(defun get-endpoint (endpoint)
  "Get full URL for Bedrock endpoint"
  (build-url *bedrock-base-url* endpoint))

(defun get-aws-credentials ()
  "Get AWS credentials from environment"
  (let ((access-key (or #+sbcl (sb-ext:posix-getenv "AWS_ACCESS_KEY_ID")
                        #-sbcl nil))
        (secret-key (or #+sbcl (sb-ext:posix-getenv "AWS_SECRET_ACCESS_KEY")
                        #-sbcl nil))
        (session-token (or #+sbcl (sb-ext:posix-getenv "AWS_SESSION_TOKEN")
                           #-sbcl nil))
        (region (or #+sbcl (sb-ext:posix-getenv "AWS_DEFAULT_REGION")
                    #-sbcl nil
                    "us-east-1")))
    (unless (and access-key secret-key)
      (error "AWS credentials not found. Set AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY environment variables."))
    (list :access-key access-key
          :secret-key secret-key
          :session-token session-token
          :region region)))

(defun format-bedrock-headers (credentials)
  "Format headers for Bedrock API requests"
  (list (cons "Content-Type" "application/json")
        (cons "Accept" "application/json")
        ;; Note: AWS signature v4 would be implemented here in production
        ;; For now, this is a placeholder structure
        (cons "Authorization" (format nil "AWS4-HMAC-SHA256 Credential=~A" 
                                     (getf credentials :access-key)))))

(defun format-bedrock-message (message)
  "Format message for Bedrock API"
  (let ((role (if (listp message) (getf message :role) "user"))
        (content (if (listp message) (getf message :content) message)))
    (list (cons :role role)
          (cons :content (if (stringp content) 
                             content 
                             (format nil "~A" content))))))

(defun handle-bedrock-error (status-code response-body)
  "Handle Bedrock API errors"
  (case status-code
    (400 "Bad request to Bedrock API. Check your request format.")
    (401 "Invalid AWS credentials. Check your AWS access key and secret.")
    (403 "Access forbidden. Check your AWS IAM permissions for Bedrock.")
    (429 "Rate limit exceeded for Bedrock. Please wait and try again.")
    (500 "Bedrock API internal error. Please try again later.")
    (t (format nil "Bedrock API error ~A: ~A" status-code response-body))))

(defun parse-bedrock-response (response-body model-id)
  "Parse Bedrock API response based on model type"
  (handler-case
      (let ((json-data (decode-json response-body)))
        (cond
          ;; Anthropic Claude models
          ((string-prefix-p "anthropic.claude" model-id)
           (let ((content (cdr (assoc :content json-data))))
             (success (list :content content))))
          ;; Amazon Titan models
          ((string-prefix-p "amazon.titan" model-id)
           (let* ((results (cdr (assoc :results json-data)))
                  (first-result (first results))
                  (output-text (cdr (assoc :outputText first-result))))
             (success (list :content output-text))))
          ;; Default handling
          (t
           (let ((output (or (cdr (assoc :completion json-data))
                            (cdr (assoc :generated_text json-data))
                            (cdr (assoc :text json-data)))))
             (success (list :content (or output "No response")))))))
    (error (e)
      (failure (format nil "Failed to parse Bedrock response: ~A" e)))))

(defun string-prefix-p (prefix string)
  "Check if string starts with prefix"
  (and (>= (length string) (length prefix))
       (string= prefix (subseq string 0 (length prefix)))))

(defun bedrock-chat (credentials model messages temperature)
  "Send chat request to Bedrock API"
  (let* ((formatted-messages (mapcar #'format-bedrock-message messages))
         (request-body (encode-json 
                        (cond
                          ;; Anthropic Claude format
                          ((string-prefix-p "anthropic.claude" model)
                           (list (cons :anthropic_version "bedrock-2023-05-31")
                                 (cons :max_tokens 1000)
                                 (cons :messages formatted-messages)
                                 (cons :temperature temperature)))
                          ;; Amazon Titan format
                          ((string-prefix-p "amazon.titan" model)
                           (list (cons :inputText (getf (first messages) :content))
                                 (cons :textGenerationConfig 
                                       (list (cons :temperature temperature)
                                             (cons :maxTokenCount 1000)))))
                          ;; Default format
                          (t (list (cons :prompt (getf (first messages) :content))
                                  (cons :max_tokens 1000)
                                  (cons :temperature temperature))))))
         (url (get-endpoint (format nil "model/~A/invoke" model)))
         (headers (format-bedrock-headers credentials)))
    
    (handler-case
        (multiple-value-bind (response-body status-code)
            (http-request url 
                         :method :post
                         :headers headers
                         :body request-body)
          (if (= status-code 200)
              (parse-bedrock-response response-body model)
              (failure (handle-bedrock-error status-code response-body))))
      (error (e)
        (failure (format nil "Bedrock API request failed: ~A" e))))))

(defun bedrock-list-models ()
  "List available Bedrock models"
  (success *bedrock-models*))

(defun test-bedrock-connection (&optional credentials)
  "Test connection to Bedrock API"
  (let ((creds (or credentials 
                   (handler-case (get-aws-credentials) (error () nil)))))
    (if creds
        (handler-case
            (let ((result (bedrock-chat creds "anthropic.claude-3-haiku-20240307-v1:0" 
                                       (list (list :role "user" :content "Hello"))
                                       0.1)))
              (if (result-success-p result)
                  (format t "✅ Bedrock connection successful~%")
                  (format t "❌ Bedrock connection failed: ~A~%" (result-error result))))
          (error (e)
            (format t "❌ Bedrock connection error: ~A~%" e)))
        (format t "❌ AWS credentials not found. Set AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY.~%"))))

(defun make-bedrock-provider (&key credentials)
  "Create Bedrock provider"
  (let ((creds (or credentials (get-aws-credentials))))
    (create-provider
     :name "bedrock"
     :chat-fn (lambda (model messages temperature)
                (bedrock-chat creds model messages temperature))
     :stream-fn nil ; Bedrock streaming would require more complex implementation
     :models-fn #'bedrock-list-models
     :config `(:credentials ,creds
              :region ,(getf creds :region)
              :supports-streaming nil
              :supports-tools nil))))