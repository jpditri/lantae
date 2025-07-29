;;;; providers.lisp - Functional provider abstraction layer
;;;;
;;;; This module implements:
;;;; - Provider protocol using higher-order functions
;;;; - Monadic error handling for API calls
;;;; - Provider composition and chaining
;;;; - Retry logic with exponential backoff
;;;; - Streaming response handling

(defpackage :lantae-providers
  (:use :cl)
  (:export #:*provider-registry*
           #:register-provider
           #:get-provider
           #:list-providers
           #:provider-chat
           #:provider-stream
           #:provider-models
           #:create-provider
           #:with-provider
           #:initialize-providers
           #:list-provider-models
           #:result
           #:result-success-p
           #:result-value
           #:result-error
           #:success
           #:failure
           #:provider
           #:provider-name
           #:provider-chat-fn
           #:provider-stream-fn
           #:provider-models-fn
           #:provider-config))

(in-package :lantae-providers)

;;; Provider registry
(defvar *provider-registry* (make-hash-table :test 'equal)
  "Registry of available providers")

(defvar *current-provider* nil
  "Currently active provider")

;;; Provider protocol
(defstruct provider
  "Provider structure using functional approach"
  name
  chat-fn
  stream-fn
  models-fn
  config)

;;; Result monad for error handling
(defstruct result
  success-p
  value
  error)

(defun success (value)
  "Create successful result"
  (make-result :success-p t :value value))

(defun failure (error)
  "Create failure result"
  (make-result :success-p nil :error error))

(defmacro result-bind (result var &body body)
  "Monadic bind for result type"
  `(if (result-success-p ,result)
       (let ((,var (result-value ,result)))
         ,@body)
       ,result))

(defmacro result-let* (bindings &body body)
  "Multiple monadic binds"
  (if (null bindings)
      `(progn ,@body)
      `(result-bind ,(second (first bindings)) ,(first (first bindings))
         (result-let* ,(rest bindings)
           ,@body))))

;;; Forward declarations
(declaim (ftype (function (&key (:api-key t)) t) make-openai-provider make-anthropic-provider))
(declaim (ftype (function (&key (:base-url t)) t) make-ollama-provider))
(declaim (ftype (function (t t t t) t) ollama-chat openai-chat anthropic-chat))
(declaim (ftype (function (t) t) ollama-list-models get-secret-key))
(declaim (ftype (function () t) openai-list-models anthropic-list-models))

;;; Provider registry functions
(defun register-provider (provider)
  "Register a provider in the registry"
  (setf (gethash (provider-name provider) *provider-registry*) provider))

(defun get-provider (name)
  "Get provider by name"
  (gethash name *provider-registry*))

(defun list-providers ()
  "List all registered providers"
  (loop for name being the hash-keys of *provider-registry*
        collect name))

;;; Provider operations
(defun provider-chat (provider-name model messages &key (temperature 0.1))
  "Send chat request to provider"
  (let ((provider (get-provider provider-name)))
    (if provider
        (funcall (provider-chat-fn provider) model messages temperature)
        (failure (format nil "Provider ~A not found" provider-name)))))

(defun provider-stream (provider-name model messages &key (temperature 0.1) callback)
  "Stream chat response from provider"
  (let ((provider (get-provider provider-name)))
    (if provider
        (if (provider-stream-fn provider)
            (funcall (provider-stream-fn provider) model messages temperature callback)
            (failure "Provider does not support streaming"))
        (failure (format nil "Provider ~A not found" provider-name)))))

(defun provider-models (provider-name)
  "List available models for provider"
  (let ((provider (get-provider provider-name)))
    (if provider
        (funcall (provider-models-fn provider))
        (failure (format nil "Provider ~A not found" provider-name)))))

(defun list-provider-models (provider-name)
  "Get list of models for a provider"
  (let ((result (provider-models provider-name)))
    (if (result-success-p result)
        (result-value result)
        nil)))

;;; Provider creation helpers
(defun create-provider (&key name chat-fn stream-fn models-fn config)
  "Create a new provider structure"
  (make-provider :name name
                 :chat-fn chat-fn
                 :stream-fn stream-fn
                 :models-fn models-fn
                 :config config))

(defmacro with-provider (provider-name &body body)
  "Execute body with specified provider as current"
  `(let ((*current-provider* (get-provider ,provider-name)))
     ,@body))

;;; Retry logic
(defun with-retry (fn &key (max-retries 3) (initial-delay 1))
  "Execute function with exponential backoff retry"
  (labels ((try-once (attempt delay)
             (handler-case
                 (funcall fn)
               (error (e)
                 (if (< attempt max-retries)
                     (progn
                       (format t "Attempt ~A failed, retrying in ~A seconds...~%"
                               attempt delay)
                       (sleep delay)
                       (try-once (1+ attempt) (* delay 2)))
                     (failure (format nil "Failed after ~A attempts: ~A"
                                    max-retries e)))))))
    (try-once 1 initial-delay)))

(defun provider-retry (provider-fn &key (max-retries 3) (delay 1))
  "Retry provider function with exponential backoff"
  (let ((attempt 0))
    (loop
      (handler-case
          (return (funcall provider-fn))
        (error (e)
          (incf attempt)
          (when (>= attempt max-retries)
            (error e))
          (format t "Attempt ~A failed, retrying in ~A seconds...~%"
                  attempt delay)
          (sleep delay)
          (setf delay (* delay 2)))))))

;;; Provider implementations (fallback if specific implementations not loaded)
(defun make-ollama-provider (&key (base-url "http://localhost:11434"))
  "Create Ollama provider"
  (create-provider
   :name "ollama"
   :chat-fn (lambda (model messages temperature)
              (ollama-chat base-url model messages temperature))
   :stream-fn nil ; TODO: Implement streaming
   :models-fn (lambda ()
                (ollama-list-models base-url))
   :config `(:base-url ,base-url)))

(defun make-openai-provider (&key api-key)
  "Create OpenAI provider"
  (unless api-key
    (error "OpenAI API key required"))
  (create-provider
   :name "openai"
   :chat-fn (lambda (model messages temperature)
              (openai-chat api-key model messages temperature))
   :stream-fn nil ; TODO: Implement streaming
   :models-fn #'openai-list-models
   :config `(:api-key ,api-key)))

(defun make-anthropic-provider (&key api-key)
  "Create Anthropic provider"
  (unless api-key
    (error "Anthropic API key required"))
  (create-provider
   :name "anthropic"
   :chat-fn (lambda (model messages temperature)
              (anthropic-chat api-key model messages temperature))
   :stream-fn nil ; TODO: Implement streaming
   :models-fn #'anthropic-list-models
   :config `(:api-key ,api-key)))

;;; Provider initialization
(defun initialize-providers ()
  "Initialize all available providers"
  ;; Load provider implementations if available
  (handler-case
      (progn
        ;; Register Ollama provider (always available)
        (when (find-package :lantae-providers-ollama)
          (register-provider (funcall (intern "MAKE-OLLAMA-PROVIDER" :lantae-providers-ollama))))
        
        ;; Register OpenAI provider if package is loaded (even without API key for testing)
        (when (find-package :lantae-providers-openai)
          (handler-case
              (register-provider (funcall (intern "MAKE-OPENAI-PROVIDER" :lantae-providers-openai)))
            (error (e)
              (format t "OpenAI provider not registered: ~A~%" e))))
        
        ;; Register cloud providers if API keys are available
        (let ((openai-key (or #+sbcl (sb-ext:posix-getenv "OPENAI_API_KEY")
                              #-sbcl nil
                              (get-secret-key "openai")))
              (anthropic-key (or #+sbcl (sb-ext:posix-getenv "ANTHROPIC_API_KEY")
                                #-sbcl nil
                                (get-secret-key "anthropic"))))
          
          (when (and openai-key (find-package :lantae-providers-openai))
            (register-provider (funcall (intern "MAKE-OPENAI-PROVIDER" :lantae-providers-openai) 
                                       :api-key openai-key)))
          
          (when (and anthropic-key (find-package :lantae-providers-anthropic))
            (register-provider (funcall (intern "MAKE-ANTHROPIC-PROVIDER" :lantae-providers-anthropic)
                                       :api-key anthropic-key)))
          
          (format t "Initialized ~A providers~%" (hash-table-count *provider-registry*))))
    (error (e)
      (format t "Error initializing providers: ~A~%" e)
      ;; Still register placeholder providers
      (register-provider (make-ollama-provider)))))

(defun get-secret-key (provider)
  "Get API key from secrets manager (placeholder)"
  ;; This would integrate with AWS Secrets Manager or similar
  (declare (ignore provider))
  nil)

;;; Provider-specific implementations (placeholders)
(defun ollama-chat (base-url model messages temperature)
  "Send chat request to Ollama API"
  (declare (ignore base-url model messages temperature))
  ;; Placeholder - would use HTTP client to call Ollama API
  (success '(:content "This is a placeholder response from Ollama")))

(defun ollama-list-models (base-url)
  "List available Ollama models"
  (declare (ignore base-url))
  ;; Placeholder - would fetch from Ollama API
  (success '("cogito:latest" "llama2:latest" "mistral:latest")))

(defun openai-chat (api-key model messages temperature)
  "Send chat request to OpenAI API"
  (declare (ignore api-key model messages temperature))
  ;; Placeholder - would use HTTP client to call OpenAI API
  (success '(:content "This is a placeholder response from OpenAI")))

(defun openai-list-models ()
  "List available OpenAI models"
  ;; Placeholder - would fetch from OpenAI API
  (success '("gpt-4" "gpt-3.5-turbo")))

(defun anthropic-chat (api-key model messages temperature)
  "Send chat request to Anthropic API"
  (declare (ignore api-key model messages temperature))
  ;; Placeholder - would use HTTP client to call Anthropic API
  (success '(:content "This is a placeholder response from Anthropic")))

(defun anthropic-list-models ()
  "List available Anthropic models"
  ;; Placeholder
  (success '("claude-3-opus" "claude-3-sonnet" "claude-3-haiku")))

;;; Provider composition utilities
(defun chain-providers (providers)
  "Create a provider that tries each provider in sequence"
  (create-provider
   :name "chained"
   :chat-fn (lambda (model messages temperature)
              (loop for provider-name in providers
                    for result = (provider-chat provider-name model messages
                                               :temperature temperature)
                    when (result-success-p result)
                      return result
                    finally (return (failure "All providers failed"))))
   :models-fn (lambda ()
                (success (loop for provider-name in providers
                               append (list-provider-models provider-name))))))

(defun fallback-provider (primary fallback)
  "Create a provider with fallback"
  (create-provider
   :name (format nil "~A-with-fallback" primary)
   :chat-fn (lambda (model messages temperature)
              (let ((result (provider-chat primary model messages
                                         :temperature temperature)))
                (if (result-success-p result)
                    result
                    (provider-chat fallback model messages
                                 :temperature temperature))))
   :models-fn (lambda ()
                (success (append (list-provider-models primary)
                               (list-provider-models fallback))))))

;;; Provider middleware
(defun with-logging (provider-name)
  "Add logging to provider calls"
  (let ((provider (get-provider provider-name)))
    (when provider
      (create-provider
       :name (format nil "~A-logged" provider-name)
       :chat-fn (lambda (model messages temperature)
                  (format t "Calling ~A with model ~A~%" provider-name model)
                  (let ((result (funcall (provider-chat-fn provider)
                                       model messages temperature)))
                    (format t "Result: ~A~%" (if (result-success-p result)
                                               "Success" "Failure"))
                    result))
       :models-fn (provider-models-fn provider)))))

(defun with-caching (provider-name &key (ttl 300))
  "Add caching to provider calls"
  (declare (ignore ttl))
  (let ((provider (get-provider provider-name))
        (cache (make-hash-table :test 'equal)))
    (when provider
      (create-provider
       :name (format nil "~A-cached" provider-name)
       :chat-fn (lambda (model messages temperature)
                  (let ((cache-key (list model messages temperature)))
                    (or (gethash cache-key cache)
                        (let ((result (funcall (provider-chat-fn provider)
                                             model messages temperature)))
                          (when (result-success-p result)
                            (setf (gethash cache-key cache) result))
                          result))))
       :models-fn (provider-models-fn provider)))))