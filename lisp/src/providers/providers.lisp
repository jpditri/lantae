;;;; providers.lisp - Provider abstraction layer for Lantae LISP
;;;; 
;;;; Functional programming approach to multi-provider LLM abstraction
;;;; Features:
;;;; - Pure functional provider interface
;;;; - Higher-order functions for provider composition
;;;; - Monadic error handling
;;;; - Protocol-based dispatch

(defpackage :lantae-providers
  (:use :cl)
  (:export #:initialize-providers
           #:make-provider
           #:call-provider
           #:list-provider-models
           #:get-provider
           #:register-provider
           #:*provider-registry*
           #:with-provider
           #:provider-compose
           #:provider-retry
           #:provider-fallback))

(in-package :lantae-providers)

;;; Provider protocol definition
(defgeneric provider-chat (provider model messages &key &allow-other-keys)
  (:documentation "Send chat messages to provider"))

(defgeneric provider-list-models (provider)
  (:documentation "List available models for provider"))

(defgeneric provider-health-check (provider)
  (:documentation "Check provider health status"))

(defgeneric provider-capabilities (provider)
  (:documentation "Get provider capabilities"))

;;; Provider registry
(defvar *provider-registry* (make-hash-table :test 'equal)
  "Global provider registry")

;;; Provider structure
(defstruct (provider (:constructor make-provider-internal))
  name
  chat-fn
  list-models-fn
  health-check-fn
  capabilities-fn
  config)

;;; Provider constructor with validation
(defun make-provider (name &key chat-fn list-models-fn health-check-fn capabilities-fn config)
  "Create a new provider with validation"
  (assert (stringp name) (name) "Provider name must be a string")
  (assert (functionp chat-fn) (chat-fn) "Chat function must be provided")
  (assert (functionp list-models-fn) (list-models-fn) "List models function must be provided")
  
  (make-provider-internal
   :name name
   :chat-fn chat-fn
   :list-models-fn list-models-fn
   :health-check-fn (or health-check-fn (lambda () '(:status :unknown)))
   :capabilities-fn (or capabilities-fn (lambda () '(:streaming nil :tools nil)))
   :config (or config '())))

;;; Provider methods implementation
(defmethod provider-chat ((provider provider) model messages &key &allow-other-keys)
  (funcall (provider-chat-fn provider) model messages))

(defmethod provider-list-models ((provider provider))
  (funcall (provider-list-models-fn provider)))

(defmethod provider-health-check ((provider provider))
  (funcall (provider-health-check-fn provider)))

(defmethod provider-capabilities ((provider provider))
  (funcall (provider-capabilities-fn provider)))

;;; Registry operations
(defun register-provider (provider)
  "Register a provider in the global registry"
  (setf (gethash (provider-name provider) *provider-registry*) provider))

(defun get-provider (name)
  "Get provider by name from registry"
  (gethash name *provider-registry*))

(defun list-providers ()
  "List all registered providers"
  (loop for name being the hash-keys of *provider-registry*
        collect name))

;;; High-level provider interface
(defun call-provider (provider-name model messages &rest options)
  "Call provider with error handling"
  (let ((provider (get-provider provider-name)))
    (unless provider
      (error "Provider ~A not found" provider-name))
    
    (handler-case
        (apply #'provider-chat provider model messages options)
      (error (e)
        (format t "Provider ~A error: ~A~%" provider-name e)
        nil))))

(defun list-provider-models (provider-name)
  "List models for provider with error handling"
  (let ((provider (get-provider provider-name)))
    (unless provider
      (error "Provider ~A not found" provider-name))
    
    (handler-case
        (provider-list-models provider)
      (error (e)
        (format t "Failed to list models for ~A: ~A~%" provider-name e)
        '()))))

;;; Higher-order provider functions
(defmacro with-provider (provider-name &body body)
  "Execute body with provider bound to *current-provider*"
  `(let ((*current-provider* (get-provider ,provider-name)))
     (unless *current-provider*
       (error "Provider ~A not found" ,provider-name))
     ,@body))

(defun provider-compose (&rest provider-names)
  "Compose multiple providers into a single function"
  (lambda (model messages &rest options)
    (loop for name in provider-names
          for result = (apply #'call-provider name model messages options)
          when result return result
          finally (error "All providers failed"))))

(defun provider-retry (provider-name &key (max-retries 3) (delay 1))
  "Create a retry wrapper for provider"
  (lambda (model messages &rest options)
    (loop repeat max-retries
          for attempt from 1
          for result = (handler-case
                           (apply #'call-provider provider-name model messages options)
                         (error (e)
                           (when (< attempt max-retries)
                             (format t "Attempt ~A failed, retrying in ~A seconds...~%" attempt delay)
                             (sleep delay))
                           nil))
          when result return result
          finally (error "Provider ~A failed after ~A attempts" provider-name max-retries))))

(defun provider-fallback (primary-provider fallback-provider)
  "Create fallback provider chain"
  (lambda (model messages &rest options)
    (or (handler-case
            (apply #'call-provider primary-provider model messages options)
          (error () nil))
        (apply #'call-provider fallback-provider model messages options))))

;;; Ollama provider implementation
(defun make-ollama-provider (&key (base-url "http://localhost:11434"))
  "Create Ollama provider with functional interface"
  (make-provider
   "ollama"
   :chat-fn (lambda (model messages &key (temperature 0.1) &allow-other-keys)
              (ollama-chat base-url model messages temperature))
   :list-models-fn (lambda ()
                     (ollama-list-models base-url))
   :health-check-fn (lambda ()
                      (handler-case
                          (ollama-list-models base-url)
                        (error () '(:status :unhealthy))
                        (:no-error (models) '(:status :healthy :models-count ,(length models)))))
   :capabilities-fn (lambda ()
                      '(:streaming nil :tools t :max-tokens 8192))
   :config `(:base-url ,base-url)))

(defun ollama-chat (base-url model messages temperature)
  "Send chat request to Ollama"
  ;; This would use a HTTP client library like Drakma
  ;; For now, a simulation
  (declare (ignore base-url model messages temperature))
  "This is a simulated Ollama response. In a real implementation, this would make HTTP requests.")

(defun ollama-list-models (base-url)
  "List Ollama models"
  (declare (ignore base-url))
  '("cogito:latest" "qwq:32b" "llama3.1-intuitive-thinker"))

;;; OpenAI provider implementation
(defun make-openai-provider (&key api-key)
  "Create OpenAI provider"
  (make-provider
   "openai"
   :chat-fn (lambda (model messages &key (temperature 0.1) &allow-other-keys)
              (openai-chat api-key model messages temperature))
   :list-models-fn (lambda ()
                     (openai-list-models))
   :health-check-fn (lambda ()
                      '(:status :healthy))
   :capabilities-fn (lambda ()
                      '(:streaming t :tools nil :max-tokens 4096))
   :config `(:api-key ,api-key)))

(defun openai-chat (api-key model messages temperature)
  "Send chat request to OpenAI"
  (declare (ignore api-key model messages temperature))
  "This is a simulated OpenAI response.")

(defun openai-list-models ()
  "List OpenAI models"
  '("gpt-4o" "gpt-4o-mini" "gpt-4-turbo" "o1-preview" "o1-mini"))

;;; Anthropic provider implementation
(defun make-anthropic-provider (&key api-key)
  "Create Anthropic provider"
  (make-provider
   "anthropic"
   :chat-fn (lambda (model messages &key (temperature 0.1) &allow-other-keys)
              (anthropic-chat api-key model messages temperature))
   :list-models-fn (lambda ()
                     (anthropic-list-models))
   :health-check-fn (lambda ()
                      '(:status :healthy))
   :capabilities-fn (lambda ()
                      '(:streaming t :tools nil :max-tokens 4096))
   :config `(:api-key ,api-key)))

(defun anthropic-chat (api-key model messages temperature)
  "Send chat request to Anthropic"
  (declare (ignore api-key model messages temperature))
  "This is a simulated Anthropic response.")

(defun anthropic-list-models ()
  "List Anthropic models"
  '("claude-3-5-sonnet-20241022" "claude-3-5-haiku-20241022" "claude-3-opus-20240229"))

;;; Provider initialization
(defun initialize-providers ()
  "Initialize all available providers"
  ;; Register Ollama provider (always available)
  (register-provider (make-ollama-provider))
  
  ;; Register cloud providers if API keys are available
  (let ((openai-key (or (uiop:getenv "OPENAI_API_KEY") 
                        (get-secret-key "openai")))
        (anthropic-key (or (uiop:getenv "ANTHROPIC_API_KEY")
                          (get-secret-key "anthropic"))))
    
    (when openai-key
      (register-provider (make-openai-provider :api-key openai-key)))
    
    (when anthropic-key
      (register-provider (make-anthropic-provider :api-key anthropic-key)))
    
    (format t "Initialized ~A providers~%" (hash-table-count *provider-registry*))))

(defun get-secret-key (provider)
  "Get API key from secrets manager (placeholder)"
  ;; This would integrate with AWS Secrets Manager or similar
  (declare (ignore provider))
  nil)

;;; Monadic error handling utilities
(defstruct result
  success-p
  value
  error)

(defun success (value)
  "Create a success result"
  (make-result :success-p t :value value :error nil))

(defun failure (error)
  "Create a failure result"
  (make-result :success-p nil :value nil :error error))

(defun result-bind (result function)
  "Monadic bind for result type"
  (if (result-success-p result)
      (funcall function (result-value result))
      result))

(defmacro result-let* (bindings &body body)
  "Monadic let* for result type"
  (if (null bindings)
      `(progn ,@body)
      (let ((binding (first bindings))
            (rest-bindings (rest bindings)))
        `(result-bind ,(second binding)
                      (lambda (,(first binding))
                        (result-let* ,rest-bindings ,@body))))))

;;; Provider with monadic error handling
(defun safe-call-provider (provider-name model messages &rest options)
  "Call provider with monadic error handling"
  (handler-case
      (let ((result (apply #'call-provider provider-name model messages options)))
        (if result
            (success result)
            (failure (format nil "Provider ~A returned nil" provider-name))))
    (error (e)
      (failure (format nil "Provider ~A error: ~A" provider-name e)))))

;;; Export symbols for external use
(export '(provider-chat provider-list-models provider-health-check provider-capabilities
          make-provider register-provider get-provider list-providers
          call-provider list-provider-models with-provider
          provider-compose provider-retry provider-fallback
          safe-call-provider result-bind result-let*
          success failure result-success-p result-value result-error))