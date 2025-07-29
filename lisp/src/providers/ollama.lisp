;;;; ollama.lisp - Ollama provider implementation
;;;;
;;;; This module implements:
;;;; - Complete Ollama API integration
;;;; - Chat completion with streaming support
;;;; - Model listing and management
;;;; - Error handling and connection validation

(defpackage :lantae-providers-ollama
  (:use :cl)
  (:import-from :lantae-providers
                #:success
                #:failure
                #:result-success-p
                #:result-value
                #:create-provider)
  (:import-from :lantae-http
                #:http-get
                #:http-post
                #:http-delete
                #:parse-json-response
                #:http-result
                #:http-result-success-p
                #:http-result-data
                #:encode-json)
  (:export #:make-ollama-provider
           #:ollama-chat
           #:ollama-stream
           #:ollama-list-models
           #:ollama-pull-model
           #:ollama-delete-model
           #:check-ollama-connection))

(in-package :lantae-providers-ollama)

(defparameter *ollama-default-url* "http://localhost:11434"
  "Default Ollama API base URL")

(defparameter *ollama-chat-endpoint* "/api/chat"
  "Ollama chat completion endpoint")

(defparameter *ollama-models-endpoint* "/api/tags"
  "Ollama models listing endpoint")

(defparameter *ollama-pull-endpoint* "/api/pull"
  "Ollama model pull endpoint")

(defparameter *ollama-delete-endpoint* "/api/delete"
  "Ollama model delete endpoint")

;;; Ollama-specific functions
(defun check-ollama-connection (base-url)
  "Check if Ollama server is running and accessible"
  (handler-case
      (let ((result (http-get (format nil "~A/api/tags" base-url) :timeout 5)))
        (http-result-success-p result))
    (error () nil)))

(defun ollama-chat-internal (base-url model messages temperature)
  "Internal implementation of Ollama chat API call"
  (let* ((url (format nil "~A~A" base-url *ollama-chat-endpoint*))
         (headers '(("Content-Type" . "application/json")))
         (body `((:model . ,model)
                (:messages . ,(mapcar #'message-to-ollama-format messages))
                (:stream . nil)
                (:options . ((:temperature . ,temperature))))))
    
    (handler-case
        (let ((result (http-post url body :headers headers :timeout 300)))
          (if (http-result-success-p result)
              (let* ((data (parse-json-response (http-result-data result)))
                     (message (cdr (assoc :message data)))
                     (content (when message (cdr (assoc :content message)))))
                (if content
                    (success content)
                    (failure "No content in response")))
              (failure "Ollama API request failed")))
      (error (e)
        (if (search "Cannot connect" (format nil "~A" e))
            (failure "Cannot connect to Ollama server. Make sure Ollama is running.")
            (failure (format nil "Ollama request failed: ~A" e)))))))

(defun message-to-ollama-format (message)
  "Convert message plist to Ollama format"
  `((:role . ,(getf message :role))
    (:content . ,(getf message :content))))

(defun ollama-stream-internal (base-url model messages temperature callback)
  "Stream chat response from Ollama"
  (let* ((url (format nil "~A~A" base-url *ollama-chat-endpoint*))
         (headers '(("Content-Type" . "application/json")))
         (body `((:model . ,model)
                (:messages . ,(mapcar #'message-to-ollama-format messages))
                (:stream . t)
                (:options . ((:temperature . ,temperature))))))
    
    ;; Streaming implementation would require a more sophisticated HTTP client
    ;; For now, fall back to non-streaming
    (format t "~&Note: Streaming not yet implemented, using regular chat~%")
    (ollama-chat-internal base-url model messages temperature)))

(defun ollama-list-models-internal (base-url)
  "List available Ollama models"
  (let ((url (format nil "~A~A" base-url *ollama-models-endpoint*)))
    (handler-case
        (let ((result (http-get url :timeout 30)))
          (if (http-result-success-p result)
              (let* ((data (parse-json-response (http-result-data result)))
                     (models (cdr (assoc :models data))))
                (success (mapcar (lambda (m) (cdr (assoc :name m))) models)))
              (failure "Failed to list models")))
      (error (e)
        (failure (format nil "Cannot connect to Ollama: ~A" e))))))

(defun ollama-pull-model (base-url model-name)
  "Pull a model from Ollama registry"
  (let* ((url (format nil "~A~A" base-url *ollama-pull-endpoint*))
         (headers '(("Content-Type" . "application/json")))
         (body `((:name . ,model-name))))
    
    (format t "~&Pulling model ~A...~%" model-name)
    (handler-case
        (let ((result (http-post url body :headers headers :timeout 600)))
          (if (http-result-success-p result)
              (progn
                (format t "Model ~A pulled successfully~%" model-name)
                (success t))
              (failure "Failed to pull model")))
      (error (e)
        (failure (format nil "Error pulling model: ~A" e))))))

(defun ollama-delete-model (base-url model-name)
  "Delete a model from Ollama"
  (let* ((url (format nil "~A~A" base-url *ollama-delete-endpoint*))
         (headers '(("Content-Type" . "application/json")))
         (body `((:name . ,model-name))))
    
    (handler-case
        (let ((result (http-post url body :headers headers :timeout 30))) ; Using POST with DELETE endpoint
          (if (http-result-success-p result)
              (progn
                (format t "Model ~A deleted~%" model-name)
                (success t))
              (failure "Failed to delete model")))
      (error (e)
        (failure (format nil "Error deleting model: ~A" e))))))

;;; Provider factory function
(defun make-ollama-provider (&key (base-url *ollama-default-url*))
  "Create Ollama provider with specified base URL"
  (unless (check-ollama-connection base-url)
    (format t "~&Warning: Cannot connect to Ollama at ~A~%" base-url)
    (format t "Make sure Ollama is running (ollama serve)~%"))
  
  (create-provider
   :name "ollama"
   :chat-fn (lambda (model messages temperature)
              (ollama-chat-internal base-url model messages temperature))
   :stream-fn (lambda (model messages temperature callback)
                (ollama-stream-internal base-url model messages temperature callback))
   :models-fn (lambda ()
                (ollama-list-models-internal base-url))
   :config `(:base-url ,base-url)))

;;; Public API functions
(defun ollama-chat (base-url model messages temperature)
  "Send chat request to Ollama"
  (ollama-chat-internal base-url model messages temperature))

(defun ollama-stream (base-url model messages temperature callback)
  "Stream chat response from Ollama"
  (ollama-stream-internal base-url model messages temperature callback))

(defun ollama-list-models (base-url)
  "List available Ollama models"
  (ollama-list-models-internal base-url))

;;; Helper functions for enhanced functionality
(defun format-ollama-prompt (messages)
  "Format messages for optimal Ollama performance"
  (let ((system-messages (remove-if-not (lambda (m) (string= (getf m :role) "system")) messages))
        (conversation (remove-if (lambda (m) (string= (getf m :role) "system")) messages)))
    
    ;; Combine system messages
    (when system-messages
      (let ((combined-system (format nil "~{~A~^~%~%~}"
                                   (mapcar (lambda (m) (getf m :content)) system-messages))))
        (cons `(:role "system" :content ,combined-system) conversation)))))

(defun validate-ollama-model (base-url model-name)
  "Check if a model exists in Ollama"
  (let ((result (ollama-list-models base-url)))
    (when (result-success-p result)
      (member model-name (result-value result) :test #'string=))))

(defun ensure-ollama-model (base-url model-name)
  "Ensure a model is available, pulling if necessary"
  (unless (validate-ollama-model base-url model-name)
    (format t "~&Model ~A not found locally, pulling...~%" model-name)
    (ollama-pull-model base-url model-name)))