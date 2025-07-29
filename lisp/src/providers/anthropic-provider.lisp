;;;; anthropic-provider-simple.lisp - Simplified Anthropic provider
;;;;
;;;; Basic Anthropic API provider without complex nested structures

(defpackage :lantae-providers-anthropic
  (:use :cl :lantae-http :lantae-providers)
  (:export #:make-anthropic-provider
           #:anthropic-chat
           #:anthropic-stream
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
  "Parse Anthropic API response with tool call support"
  (let ((content (cdr (assoc :content response-json)))
        (stop-reason (cdr (assoc :stop_reason response-json))))
    (when (and content (> (length content) 0))
      (let ((text-content "")
            (tool-calls nil))
        ;; Process all content blocks
        (dolist (content-block content)
          (let ((block-type (cdr (assoc :type content-block))))
            (cond
              ((string= block-type "text")
               (setf text-content (concatenate 'string text-content 
                                             (cdr (assoc :text content-block)))))
              ((string= block-type "tool_use")
               (push `((:id . ,(cdr (assoc :id content-block)))
                      (:type . "function")
                      (:function . ((:name . ,(cdr (assoc :name content-block)))
                                   (:arguments . ,(encode-json (cdr (assoc :input content-block)))))))
                     tool-calls)))))
        ;; Return appropriate response format
        (if tool-calls
            `(:role "assistant"
              :content ,text-content
              :tool_calls ,(reverse tool-calls)
              :stop_reason ,stop-reason)
            text-content)))))

(defun anthropic-chat (api-key model messages temperature &key max-tokens tools)
  "Send chat request to Anthropic"
  (let* ((url (get-endpoint "messages"))
         (headers (format-anthropic-headers api-key))
         (formatted-messages (mapcar #'format-anthropic-message messages))
         (request-body (list (cons :model model)
                            (cons :messages formatted-messages)
                            (cons :temperature temperature)
                            (cons :max_tokens (or max-tokens 4096))))
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
               (parsed-response (parse-anthropic-response response)))
          (if parsed-response
              (success (if (listp parsed-response)
                          parsed-response
                          (list (cons :role "assistant") (cons :content parsed-response))))
              (failure "No response from Anthropic")))
        (failure (handle-anthropic-error 
                 (http-result-status-code result)
                 (http-result-data result))))))

(defun anthropic-stream (api-key model messages temperature callback)
  "Stream chat response from Anthropic"
  (let* ((formatted-messages (mapcar #'format-anthropic-message messages))
         (request-body (encode-json 
                        (list (cons :model model)
                              (cons :max_tokens 1000)
                              (cons :messages formatted-messages)
                              (cons :temperature temperature)
                              (cons :stream t))))
         (url (get-endpoint "messages"))
         (headers (format-anthropic-headers api-key)))
    
    (handler-case
        (http-stream url 
                     :method :post
                     :headers headers
                     :content request-body
                     :callback (lambda (chunk)
                                 (when chunk
                                   (let ((parsed (parse-anthropic-stream-chunk chunk)))
                                     (when parsed
                                       (funcall callback parsed))))))
      (error (e)
        (failure (format nil "Anthropic streaming request failed: ~A" e))))))

(defun parse-anthropic-stream-chunk (chunk)
  "Parse streaming chunk from Anthropic"
  (when (and chunk (> (length chunk) 6) (string= (subseq chunk 0 6) "data: "))
    (let ((json-str (subseq chunk 6)))
      (unless (string= json-str "[DONE]")
        (handler-case
            (let* ((data (decode-json json-str))
                   (delta (cdr (assoc :delta data)))
                   (text (cdr (assoc :text delta))))
              text)
          (error () nil))))))

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
     :chat-fn (lambda (model messages temperature &key tools)
               (anthropic-chat key model messages temperature :tools tools))
     :stream-fn (lambda (model messages temperature callback)
                  (anthropic-stream key model messages temperature callback))
     :models-fn (lambda ()
                 (anthropic-list-models key))
     :config (list (cons :api-key key)
                  (cons :base-url *anthropic-base-url*)
                  (cons :supports-streaming t)
                  (cons :supports-tools t)
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

;;; Tool/Function calling support
(defun format-anthropic-tool (tool)
  "Format tool definition for Anthropic API"
  `((:name . ,(cdr (assoc :name (cdr (assoc :function tool)))))
    (:description . ,(cdr (assoc :description (cdr (assoc :function tool)))))
    (:input_schema . ,(cdr (assoc :parameters (cdr (assoc :function tool)))))))

(defun handle-anthropic-tool-response (response model messages options)
  "Handle tool use response from Anthropic provider"
  (let* ((tool-calls (getf response :tool_calls))
         (tool-results (execute-anthropic-tools tool-calls)))
    ;; Add tool results to conversation and continue
    (when tool-results
      (let* ((updated-messages (append messages
                                      (list response)
                                      tool-results))
             (continue-response (anthropic-chat (getf options :api-key)
                                              model
                                              updated-messages
                                              (getf options :temperature))))
        (if (result-success-p continue-response)
            continue-response
            response)))))

(defun execute-anthropic-tools (tool-calls)
  "Execute Anthropic tool calls and format results"
  (mapcar (lambda (tool-call)
            (let* ((function-info (cdr (assoc :function tool-call)))
                   (function-name (cdr (assoc :name function-info)))
                   (function-args (cdr (assoc :arguments function-info)))
                   (tool-id (cdr (assoc :id tool-call))))
              ;; Format as Anthropic tool result message
              `((:role . "user")
                (:content . (((:type . "tool_result")
                             (:tool_use_id . ,tool-id)
                             (:content . "Tool execution result")))))))
          tool-calls))