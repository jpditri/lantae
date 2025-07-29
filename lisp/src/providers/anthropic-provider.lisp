;;;; anthropic-provider-simple.lisp - Simplified Anthropic provider
;;;;
;;;; Basic Anthropic API provider without complex nested structures

(defpackage :lantae-providers-anthropic
  (:use :cl :lantae-http :lantae-providers)
  (:export #:make-anthropic-provider
           #:anthropic-chat
           #:anthropic-list-models
           #:test-anthropic-connection))

(in-package :lantae-providers-anthropic)

;;; Anthropic API configuration
(defparameter *anthropic-base-url* "https://api.anthropic.com/v1")
(defparameter *anthropic-models*
  '("claude-3-5-sonnet-20241022" "claude-3-5-haiku-20241022"
    "claude-3-opus-20240229" "claude-3-sonnet-20240229" "claude-3-haiku-20240307"))

(defun get-endpoint (endpoint)
  "Get full URL for Anthropic endpoint"
  (build-url *anthropic-base-url* endpoint))

(defun get-api-key ()
  "Get Anthropic API key from environment"
  (or #+sbcl (sb-ext:posix-getenv "ANTHROPIC_API_KEY")
      #-sbcl nil
      (error "Anthropic API key not found. Set ANTHROPIC_API_KEY environment variable.")))

(defun format-anthropic-headers (api-key)
  "Format headers for Anthropic API requests"
  (list (cons "x-api-key" api-key)
        (cons "anthropic-version" "2023-06-01")
        (cons "Content-Type" "application/json")))

(defun format-anthropic-message (message)
  "Format message for Anthropic API - simplified version"
  (let ((role (if (listp message) (getf message :role) "user"))
        (content (if (listp message) (getf message :content) message)))
    (list (cons :role role)
          (cons :content (list (list (cons :type "text") 
                                    (cons :text (if (stringp content) 
                                                   content 
                                                   (format nil "~A" content)))))))))

(defun handle-anthropic-error (status-code response-body)
  "Handle Anthropic API errors"
  (case status-code
    (401 "Invalid Anthropic API key. Check your credentials.")
    (429 "Rate limit exceeded for Anthropic. Please wait and try again.")
    ((500 502 503) "Anthropic service temporarily unavailable. Please try again later.")
    (t (format nil "Anthropic API error ~A: ~A" status-code response-body))))

(defun parse-anthropic-response (response-json)
  "Parse Anthropic API response"
  (let ((content (cdr (assoc :content response-json))))
    (when (and content (> (length content) 0))
      (let ((first-content (first content)))
        (if (string= (cdr (assoc :type first-content)) "text")
            (cdr (assoc :text first-content))
            (format nil "~A" first-content))))))

(defun anthropic-chat (api-key model messages temperature &key max-tokens)
  "Send chat request to Anthropic"
  (let* ((url (get-endpoint "messages"))
         (headers (format-anthropic-headers api-key))
         (formatted-messages (mapcar #'format-anthropic-message messages))
         (request-body (list (cons :model model)
                            (cons :messages formatted-messages)
                            (cons :temperature temperature)
                            (cons :max_tokens (or max-tokens 4096))))
         (result (http-request url 
                              :method :post
                              :headers headers
                              :content request-body
                              :content-type "application/json")))
    
    (if (http-result-success-p result)
        (let* ((response (parse-json-response (http-result-data result)))
               (content-text (parse-anthropic-response response)))
          (if content-text
              (success (list (cons :role "assistant") (cons :content content-text)))
              (failure "No response from Anthropic")))
        (failure (handle-anthropic-error 
                 (http-result-status-code result)
                 (http-result-data result))))))

(defun anthropic-list-models (api-key)
  "List available Anthropic models"
  (declare (ignore api-key))
  (success *anthropic-models*))

(defun make-anthropic-provider (&key api-key)
  "Create Anthropic provider instance"
  (let ((key (or api-key (get-api-key))))
    (unless key
      (error "Anthropic API key required"))
    (lantae-providers:create-provider
     :name "anthropic"
     :chat-fn (lambda (model messages temperature)
               (anthropic-chat key model messages temperature))
     :stream-fn nil
     :models-fn (lambda ()
                 (anthropic-list-models key))
     :config (list (cons :api-key key)
                  (cons :base-url *anthropic-base-url*)
                  (cons :supports-streaming nil)
                  (cons :supports-tools nil)
                  (cons :default-model "claude-3-5-sonnet-20241022")
                  (cons :default-temperature 0.1)))))

(defun test-anthropic-connection (&key api-key)
  "Test if Anthropic API is accessible"
  (let* ((key (or api-key (handler-case (get-api-key) (error () nil))))
         (test-messages (list (list (cons :role "user") (cons :content "Hello")))))
    (if key
        (let ((result (anthropic-chat key "claude-3-5-haiku-20241022" test-messages 0.1)))
          (if (result-success-p result)
              (progn
                (format t "✓ Anthropic API is accessible~%")
                t)
              (progn
                (format t "✗ Cannot connect to Anthropic: ~A~%" (result-error result))
                nil)))
        (progn
          (format t "✗ Anthropic API key not found. Set ANTHROPIC_API_KEY environment variable.~%")
          nil))))