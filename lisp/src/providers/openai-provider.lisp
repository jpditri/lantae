;;;; openai-provider.lisp - OpenAI provider implementation
;;;;
;;;; Implements the OpenAI provider with:
;;;; - Chat completion API
;;;; - Model listing
;;;; - Streaming support
;;;; - Function calling support
;;;; - Error handling

(defpackage :lantae-providers-openai
  (:use :cl :lantae-http :lantae-providers)
  (:export #:make-openai-provider
           #:openai-chat
           #:openai-stream
           #:openai-list-models
           #:test-openai-connection))

(in-package :lantae-providers-openai)

;;; OpenAI API configuration
(defparameter *openai-base-url* "https://api.openai.com/v1"
  "Base URL for OpenAI API")

(defparameter *openai-endpoints*
  '(:chat "chat/completions"
    :models "models")
  "OpenAI API endpoints")

(defparameter *openai-models*
  '("gpt-4o" "gpt-4o-mini" "gpt-4-turbo" "gpt-4"
    "o1-preview" "o1-mini"
    "gpt-3.5-turbo" "gpt-3.5-turbo-16k")
  "Available OpenAI models")

(defparameter *model-max-tokens*
  '(("o1-preview" . 32768)
    ("o1-mini" . 32768)
    ("gpt-4o" . 8192)
    ("gpt-4o-mini" . 8192)
    ("gpt-4-turbo" . 8192)
    ("gpt-4" . 8192)
    ("gpt-3.5-turbo" . 4096)
    ("gpt-3.5-turbo-16k" . 16384))
  "Maximum tokens by model")

(defun get-endpoint (endpoint)
  "Get full URL for OpenAI endpoint"
  (build-url *openai-base-url* (getf *openai-endpoints* endpoint)))

(defun get-api-key ()
  "Get OpenAI API key from environment"
  (or #+sbcl (sb-ext:posix-getenv "OPENAI_API_KEY")
      #-sbcl nil
      (error "OpenAI API key not found. Set OPENAI_API_KEY environment variable.")))

(defun get-max-tokens (model)
  "Get maximum tokens for model"
  (or (cdr (assoc model *model-max-tokens* :test #'string=))
      4096))

;;; Request/Response handling
(defun format-openai-headers (api-key)
  "Format headers for OpenAI API requests"
  `(("Authorization" . ,(format nil "Bearer ~A" api-key))
    ("Content-Type" . "application/json")))

(defun format-openai-message (message)
  "Format message for OpenAI API"
  (cond
    ;; Already formatted as alist
    ((and (listp message) (listp (car message)) (assoc :role message))
     message)
    ;; List format (:role "user" :content "text")
    ((and (listp message) (getf message :role))
     `((:role . ,(getf message :role))
       (:content . ,(getf message :content))))
    ;; Simple cons pair (role . content)
    ((and (consp message) (not (listp (cdr message))))
     `((:role . ,(car message))
       (:content . ,(cdr message))))
    ;; String - assume user message
    ((stringp message)
     `((:role . "user")
       (:content . ,message)))
    ;; Default
    (t
     `((:role . "user")
       (:content . ,(format nil "~A" message))))))

(defun handle-openai-error (status-code response-body)
  "Handle OpenAI API errors"
  (case status-code
    (401 "Invalid OpenAI API key. Check your credentials.")
    (429 "Rate limit exceeded for OpenAI. Please wait and try again.")
    ((500 502 503) "OpenAI service temporarily unavailable. Please try again later.")
    (t (format nil "OpenAI API error ~A: ~A" status-code response-body))))

;;; Chat implementation
(defun openai-chat (api-key model messages temperature &key tools max-tokens)
  "Send chat request to OpenAI"
  (let* ((url (get-endpoint :chat))
         (headers (format-openai-headers api-key))
         (formatted-messages (mapcar #'format-openai-message messages))
         (request-body `((:model . ,model)
                        (:messages . ,formatted-messages)
                        (:temperature . ,temperature)
                        (:max_tokens . ,(or max-tokens (get-max-tokens model)))
                        (:stream . nil)))
         ;; Add tools if provided
         (final-body (if tools
                        (append request-body `((:tools . ,tools) (:tool_choice . "auto")))
                        request-body))
         (result (http-request url 
                              :method :post
                              :headers headers
                              :content final-body
                              :content-type "application/json")))
    
    (if (http-result-success-p result)
        (let* ((response (parse-json-response (http-result-data result)))
               (choices (cdr (assoc :choices response))))
          (if (and choices (> (length choices) 0))
              (let* ((choice (first choices))
                     (message (cdr (assoc :message choice)))
                     (content (cdr (assoc :content message)))
                     (tool-calls (cdr (assoc :tool_calls message))))
                (success `(:role "assistant" 
                          :content ,content
                          ,@(when tool-calls `(:tool_calls ,tool-calls)))))
              (failure "No response from OpenAI")))
        (failure (handle-openai-error 
                 (http-result-status-code result)
                 (http-result-data result))))))

(defun openai-stream (api-key model messages temperature callback &key tools max-tokens)
  "Stream chat response from OpenAI"
  (let* ((url (get-endpoint :chat))
         (headers (format-openai-headers api-key))
         (formatted-messages (mapcar #'format-openai-message messages))
         (request-body `((:model . ,model)
                        (:messages . ,formatted-messages)
                        (:temperature . ,temperature)
                        (:max_tokens . ,(or max-tokens (get-max-tokens model)))
                        (:stream . t)))
         ;; Add tools if provided
         (final-body (if tools
                        (append request-body `((:tools . ,tools) (:tool_choice . "auto")))
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
                                             (choices (cdr (assoc :choices json))))
                                        (when (and choices (> (length choices) 0))
                                          (let* ((choice (first choices))
                                                 (delta (cdr (assoc :delta choice)))
                                                 (content (cdr (assoc :content delta))))
                                            (when (and callback content)
                                              (funcall callback content))))))))
                              (error () nil))))))

;;; Model management
(defun openai-list-models (api-key)
  "List available OpenAI models"
  (declare (ignore api-key))
  ;; For now, return the hardcoded list like Ruby implementation
  ;; Could be extended to call the actual models API endpoint
  (success *openai-models*))

;;; Provider factory
(defun make-openai-provider (&key api-key)
  "Create OpenAI provider instance"
  (let ((key (or api-key (get-api-key))))
    (unless key
      (error "OpenAI API key required"))
    (lantae-providers:create-provider
     :name "openai"
     :chat-fn (lambda (model messages temperature)
               (openai-chat key model messages temperature))
     :stream-fn (lambda (model messages temperature callback)
                 (openai-stream key model messages temperature callback))
     :models-fn (lambda ()
                 (openai-list-models key))
     :config `(:api-key ,key
              :base-url ,*openai-base-url*
              :supports-streaming t
              :supports-tools t
              :default-model "gpt-4o"
              :default-temperature 0.1))))

;;; Connection testing
(defun test-openai-connection (&key api-key)
  "Test if OpenAI API is accessible"
  (let* ((key (or api-key (handler-case (get-api-key) (error () nil))))
         (test-messages '((:role "user" :content "Hello"))))
    (if key
        (let ((result (openai-chat key "gpt-4o-mini" test-messages 0.1)))
          (if (result-success-p result)
              (progn
                (format t "✓ OpenAI API is accessible~%")
                t)
              (progn
                (format t "✗ Cannot connect to OpenAI: ~A~%" (result-error result))
                nil)))
        (progn
          (format t "✗ OpenAI API key not found. Set OPENAI_API_KEY environment variable.~%")
          nil))))

;;; Tool/Function calling support
(defun format-tool-definition (tool)
  "Format tool definition for OpenAI API"
  `((:type . "function")
    (:function . ,tool)))

(defun execute-tool-calls (tool-calls available-tools)
  "Execute tool calls and return results"
  (mapcar (lambda (tool-call)
            (let* ((function-info (cdr (assoc :function tool-call)))
                   (function-name (cdr (assoc :name function-info)))
                   (function-args (cdr (assoc :arguments function-info)))
                   (tool-id (cdr (assoc :id tool-call))))
              ;; This would need to be implemented based on available tools
              `((:role . "tool")
                (:tool_call_id . ,tool-id)
                (:name . ,function-name)
                (:content . "Tool execution result"))))
          tool-calls))

;;; Utility functions
(defun string-prefix-p (prefix string)
  "Check if string starts with prefix"
  (and (>= (length string) (length prefix))
       (string= prefix (subseq string 0 (length prefix)))))

;;; Model-specific configurations
(defun get-model-config (model)
  "Get configuration for specific model"
  (cond
    ((member model '("o1-preview" "o1-mini") :test #'string=)
     `(:supports-system-message nil
       :supports-temperature nil
       :reasoning-model t))
    ((string-prefix-p "gpt-4" model)
     `(:supports-system-message t
       :supports-temperature t
       :reasoning-model nil))
    (t
     `(:supports-system-message t
       :supports-temperature t
       :reasoning-model nil))))

;;; Usage estimation
(defun estimate-tokens (text)
  "Rough estimation of tokens in text (1 token ≈ 4 characters)"
  (ceiling (length text) 4))

(defun estimate-cost (model tokens)
  "Estimate cost based on model and token count"
  ;; Simplified cost estimation - would need real pricing
  (let ((cost-per-1k (case (intern (string-upcase model))
                       (|GPT-4O| 0.005)
                       (|GPT-4O-MINI| 0.00015)
                       (|GPT-4-TURBO| 0.01)
                       (|GPT-4| 0.03)
                       (t 0.002))))
    (* tokens cost-per-1k 0.001)))