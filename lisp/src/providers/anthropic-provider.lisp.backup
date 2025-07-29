;;;; anthropic-provider.lisp - Anthropic provider implementation
;;;;
;;;; Implements the Anthropic provider with:
;;;; - Claude chat completion API
;;;; - Model listing
;;;; - Tool usage support
;;;; - Error handling
;;;; - Anthropic-specific response format

(defpackage :lantae-providers-anthropic
  (:use :cl :lantae-http :lantae-providers)
  (:export #:make-anthropic-provider
           #:anthropic-chat
           #:anthropic-stream
           #:anthropic-list-models
           #:test-anthropic-connection))

(in-package :lantae-providers-anthropic)

;;; Anthropic API configuration
(defparameter *anthropic-base-url* "https://api.anthropic.com/v1"
  "Base URL for Anthropic API")

(defparameter *anthropic-endpoints*
  '(:messages "messages")
  "Anthropic API endpoints")

(defparameter *anthropic-models*
  '("claude-3-5-sonnet-20241022" "claude-3-5-haiku-20241022"
    "claude-3-opus-20240229" "claude-3-sonnet-20240229" "claude-3-haiku-20240307")
  "Available Anthropic models")

(defparameter *model-max-tokens*
  '(("claude-3-5-sonnet-20241022" . 8192)
    ("claude-3-5-haiku-20241022" . 8192)
    ("claude-3-opus-20240229" . 4096)
    ("claude-3-sonnet-20240229" . 4096)
    ("claude-3-haiku-20240307" . 4096))
  "Maximum tokens by model")

(defun get-endpoint (endpoint)
  "Get full URL for Anthropic endpoint"
  (build-url *anthropic-base-url* (getf *anthropic-endpoints* endpoint)))

(defun get-api-key ()
  "Get Anthropic API key from environment"
  (or #+sbcl (sb-ext:posix-getenv "ANTHROPIC_API_KEY")
      #-sbcl nil
      (error "Anthropic API key not found. Set ANTHROPIC_API_KEY environment variable.")))

(defun get-max-tokens (model)
  "Get maximum tokens for model"
  (or (cdr (assoc model *model-max-tokens* :test #'string=))
      4096))

;;; Request/Response handling
(defun format-anthropic-headers (api-key)
  "Format headers for Anthropic API requests"
  `(("x-api-key" . ,api-key)
    ("anthropic-version" . "2023-06-01")
    ("Content-Type" . "application/json")))

(defun format-anthropic-message (message)
  "Format message for Anthropic API"
  (cond
    ;; Already formatted as alist with content array
    ((and (listp message) (listp (car message)) (assoc :role message))
     message)
    ;; List format (:role "user" :content "text")
    ((and (listp message) (getf message :role))
     (let ((role (getf message :role))
           (content (getf message :content)))
       `((:role . ,role)
         (:content . ,(if (stringp content)
                         (list `((:type . "text") (:text . ,content)))
                         content)))))
    ;; Simple cons pair (role . content)
    ((and (consp message) (not (listp (cdr message))))
     `((:role . ,(car message))
       (:content . ,(list `((:type . "text") (:text . ,(cdr message))))))
    ;; String - assume user message
    ((stringp message)
     `((:role . "user")
       (:content . ,(list `((:type . "text") (:text . ,message))))))
    ;; Default
    (t
     `((:role . "user")
       (:content . ,(list `((:type . "text") (:text . ,(format nil "~A" message)))))))

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
        (cond
          ;; Text response
          ((string= (cdr (assoc :type first-content)) "text")
           (cdr (assoc :text first-content)))
          ;; Tool use response
          ((string= (cdr (assoc :type first-content)) "tool_use")
           `(:tool_use (:id . ,(cdr (assoc :id first-content)))
                      (:name . ,(cdr (assoc :name first-content)))
                      (:input . ,(cdr (assoc :input first-content)))))
          ;; Default
          (t (format nil "~A" first-content)))))))

;;; Chat implementation
(defun anthropic-chat (api-key model messages temperature &key tools max-tokens)
  "Send chat request to Anthropic"
  (let* ((url (get-endpoint :messages))
         (headers (format-anthropic-headers api-key))
         (formatted-messages (mapcar #'format-anthropic-message messages))
         (request-body `((:model . ,model)
                        (:messages . ,formatted-messages)
                        (:temperature . ,temperature)
                        (:max_tokens . ,(or max-tokens (get-max-tokens model)))))
         ;; Add tools if provided
         (final-body (if tools
                        (append request-body `((:tools . ,tools)))
                        request-body))
         (result (http-request url 
                              :method :post
                              :headers headers
                              :content final-body
                              :content-type "application/json")))
    
    (if (http-result-success-p result)
        (let* ((response (parse-json-response (http-result-data result)))
               (content-text (parse-anthropic-response response)))
          (if content-text
              (success `(:role "assistant" :content ,content-text))
              (failure "No response from Anthropic")))
        (failure (handle-anthropic-error 
                 (http-result-status-code result)
                 (http-result-data result))))))

(defun anthropic-stream (api-key model messages temperature callback &key tools max-tokens)
  "Stream chat response from Anthropic"
  (let* ((url (get-endpoint :messages))
         (headers (format-anthropic-headers api-key))
         (formatted-messages (mapcar #'format-anthropic-message messages))
         (request-body `((:model . ,model)
                        (:messages . ,formatted-messages)
                        (:temperature . ,temperature)
                        (:max_tokens . ,(or max-tokens (get-max-tokens model)))
                        (:stream . t)))
         ;; Add tools if provided
         (final-body (if tools
                        (append request-body `((:tools . ,tools)))
                        request-body)))
    
    (http-stream url
                 :method :post
                 :headers headers
                 :content final-body
                 :content-type "application/json"
                 :callback (lambda (line)
                            (handler-case
                                (when (and line 
                                          (> (length line) 6)
                                          (string= (subseq line 0 6) "data: "))
                                  (let ((data (subseq line 6)))
                                    (unless (string= data "[DONE]")
                                      (let* ((json (decode-json data))
                                             (type (cdr (assoc :type json))))
                                        (when (string= type "content_block_delta")
                                          (let* ((delta (cdr (assoc :delta json)))
                                                 (text (cdr (assoc :text delta))))
                                            (when (and callback text)
                                              (funcall callback text))))))))
                              (error () nil))))))

;;; Model management
(defun anthropic-list-models (api-key)
  "List available Anthropic models"
  (declare (ignore api-key))
  ;; For now, return the hardcoded list like Ruby implementation
  ;; Anthropic doesn't have a public models endpoint
  (success *anthropic-models*))

;;; Provider factory
(defun make-anthropic-provider (&key api-key)
  "Create Anthropic provider instance"
  (let ((key (or api-key (get-api-key))))
    (unless key
      (error "Anthropic API key required"))
    (lantae-providers:create-provider
     :name "anthropic"
     :chat-fn (lambda (model messages temperature)
               (anthropic-chat key model messages temperature))
     :stream-fn (lambda (model messages temperature callback)
                 (anthropic-stream key model messages temperature callback))
     :models-fn (lambda ()
                 (anthropic-list-models key))
     :config `(:api-key ,key
              :base-url ,*anthropic-base-url*
              :supports-streaming t
              :supports-tools t
              :default-model "claude-3-5-sonnet-20241022"
              :default-temperature 0.1))))

;;; Connection testing
(defun test-anthropic-connection (&key api-key)
  "Test if Anthropic API is accessible"
  (let* ((key (or api-key (handler-case (get-api-key) (error () nil))))
         (test-messages '((:role "user" :content "Hello"))))
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

;;; Tool/Function calling support
(defun format-tool-definition (tool)
  "Format tool definition for Anthropic API"
  `((:name . ,(getf tool :name))
    (:description . ,(getf tool :description))
    (:input_schema . ,(getf tool :input_schema))))

(defun execute-tool-calls (tool-calls available-tools)
  "Execute tool calls and return results"
  (mapcar (lambda (tool-call)
            (let* ((tool-id (getf tool-call :id))
                   (tool-name (getf tool-call :name))
                   (tool-input (getf tool-call :input)))
              ;; This would need to be implemented based on available tools
              `((:type . "tool_result")
                (:tool_use_id . ,tool-id)
                (:content . "Tool execution result"))))
          tool-calls))

;;; Model-specific configurations
(defun get-model-config (model)
  "Get configuration for specific model"
  (cond
    ((string-prefix-p "claude-3-5" model)
     `(:supports-system-message t
       :supports-temperature t
       :reasoning-model nil
       :max-tokens 8192))
    ((string-prefix-p "claude-3-opus" model)
     `(:supports-system-message t
       :supports-temperature t
       :reasoning-model t
       :max-tokens 4096))
    (t
     `(:supports-system-message t
       :supports-temperature t
       :reasoning-model nil
       :max-tokens 4096))))

;;; Utility functions
(defun string-prefix-p (prefix string)
  "Check if string starts with prefix"
  (and (>= (length string) (length prefix))
       (string= prefix (subseq string 0 (length prefix)))))

;;; Usage estimation
(defun estimate-tokens (text)
  "Rough estimation of tokens in text (1 token ≈ 4 characters)"
  (ceiling (length text) 4))

(defun estimate-cost (model tokens)
  "Estimate cost based on model and token count"
  ;; Simplified cost estimation - would need real pricing
  (let ((cost-per-1k (cond
                       ((string-prefix-p "claude-3-5-sonnet" model) 0.003)
                       ((string-prefix-p "claude-3-5-haiku" model) 0.00025)
                       ((string-prefix-p "claude-3-opus" model) 0.015)
                       ((string-prefix-p "claude-3-sonnet" model) 0.003)
                       ((string-prefix-p "claude-3-haiku" model) 0.00025)
                       (t 0.003))))
    (* tokens cost-per-1k 0.001)))

;;; Message conversion utilities
(defun openai-to-anthropic-messages (openai-messages)
  "Convert OpenAI message format to Anthropic format"
  (mapcar (lambda (msg)
            (let ((role (getf msg :role))
                  (content (getf msg :content)))
              `((:role . ,role)
                (:content . ,(list `((:type . "text") (:text . ,content))))))
          openai-messages))

(defun anthropic-to-openai-response (anthropic-response)
  "Convert Anthropic response to OpenAI-like format"
  (let ((content (parse-anthropic-response anthropic-response)))
    `((:role . "assistant")
      (:content . ,content))))