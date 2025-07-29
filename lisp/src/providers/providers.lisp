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
           #:provider-available-p
           #:validate-provider-switch
           #:switch-provider
           #:get-current-provider-name
           #:get-provider-info
           #:detect-available-providers
           #:check-api-key
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
           #:provider-config
           #:set-provider-tool-manager
           #:provider-supports-tools-p
           #:provider-call-tool
           #:handle-tool-response))

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
  tool-manager
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

(defun provider-available-p (name)
  "Check if provider is available and registered"
  (not (null (get-provider name))))

(defun validate-provider-switch (provider-name)
  "Validate if we can switch to the specified provider"
  (cond
    ((not (provider-available-p provider-name))
     (values nil (format nil "Provider '~A' is not available. Available: ~{~A~^, ~}" 
                        provider-name (list-providers))))
    (t (values t nil))))

(defun switch-provider (provider-name)
  "Switch to the specified provider with validation"
  (multiple-value-bind (valid-p error-msg) (validate-provider-switch provider-name)
    (if valid-p
        (progn
          (setf *current-provider* (get-provider provider-name))
          (values t (format nil "Switched to provider: ~A" provider-name)))
        (values nil error-msg))))

(defun get-current-provider-name ()
  "Get the name of the current provider"
  (when *current-provider*
    (provider-name *current-provider*)))

(defun get-provider-info (provider-name)
  "Get detailed information about a provider"
  (let ((provider (get-provider provider-name)))
    (when provider
      (list :name (provider-name provider)
            :has-streaming (not (null (provider-stream-fn provider)))
            :has-tools (not (null (provider-tool-manager provider)))
            :config (provider-config provider)))))

;;; Provider operations
(defun provider-chat (provider-name model messages &key (temperature 0.1) tools)
  "Send chat request to provider"
  (let ((provider (get-provider provider-name)))
    (if provider
        (if tools
            ;; Handle function calling
            (let ((formatted-tools (mapcar (lambda (tool)
                                           (funcall (intern "FORMAT-TOOL-FOR-PROVIDER" :lantae-function-calling)
                                                   provider-name tool))
                                          tools)))
              (funcall (provider-chat-fn provider) model messages temperature :tools formatted-tools))
            ;; Regular chat without tools
            (funcall (provider-chat-fn provider) model messages temperature))
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

;;; Tool support functions
(defun set-provider-tool-manager (provider-name tool-manager)
  "Set tool manager for a provider"
  (let ((provider (get-provider provider-name)))
    (when provider
      (setf (provider-tool-manager provider) tool-manager)
      t)))

(defun provider-supports-tools-p (provider-name)
  "Check if provider supports tools"
  (let ((provider (get-provider provider-name)))
    (and provider 
         (provider-tool-manager provider)
         (not (null (provider-tool-manager provider))))))

(defun provider-call-tool (provider-name tool-name &rest arguments)
  "Call a tool through the provider's tool manager"
  (let ((provider (get-provider provider-name)))
    (if (and provider (provider-tool-manager provider))
        (apply (intern "EXECUTE-TOOL" :lantae-tools)
               (provider-tool-manager provider)
               tool-name
               arguments)
        (failure (format nil "Provider ~A does not support tools" provider-name)))))

(defun handle-tool-response (provider-name response model messages options)
  "Handle tool use response from provider"
  (let ((provider (get-provider provider-name)))
    (when (and provider (provider-tool-manager provider))
      ;; This will be implemented per-provider based on their tool format
      (case (intern (string-upcase provider-name) :keyword)
        (:anthropic (handle-anthropic-tool-response provider response model messages options))
        (:openai (handle-openai-tool-response provider response model messages options))
        (t (failure "Tool handling not implemented for this provider"))))))

;;; Provider creation helpers
(defun create-provider (&key name chat-fn stream-fn models-fn tool-manager config)
  "Create a new provider structure"
  (make-provider :name name
                 :chat-fn chat-fn
                 :stream-fn stream-fn
                 :models-fn models-fn
                 :tool-manager tool-manager
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
(defun check-api-key (provider-name)
  "Check if API key is available for provider"
  (let ((env-var (format nil "~A_API_KEY" (string-upcase provider-name))))
    (or #+sbcl (sb-ext:posix-getenv env-var)
        #-sbcl nil
        (get-secret-key provider-name))))

(defun detect-available-providers ()
  "Detect which providers can be initialized"
  (let ((available '()))
    ;; Ollama is always available (local)
    (push '("ollama" . :local) available)
    
    ;; Check for cloud providers with API keys
    (dolist (provider '("openai" "anthropic" "gemini" "mistral" "perplexity"))
      (when (check-api-key provider)
        (push (cons provider :cloud) available)))
    
    available))

(defun initialize-providers ()
  "Initialize all available providers with better error handling"
  (let ((initialized-count 0))
    (handler-case
        (progn
          ;; Always register Ollama provider first
          (when (find-package :lantae-providers-ollama)
            (handler-case
                (progn
                  (register-provider (funcall (intern "MAKE-OLLAMA-PROVIDER" :lantae-providers-ollama)))
                  (incf initialized-count)
                  (format t "✓ Ollama provider registered~%"))
              (error (e)
                (format t "✗ Failed to register Ollama: ~A~%" e))))
          
          ;; Register cloud providers with proper API key detection
          (let ((openai-key (check-api-key "openai"))
                (anthropic-key (check-api-key "anthropic"))
                (gemini-key (check-api-key "gemini"))
                (mistral-key (check-api-key "mistral"))
                (perplexity-key (check-api-key "perplexity"))
                (aws-access-key (or #+sbcl (sb-ext:posix-getenv "AWS_ACCESS_KEY_ID")
                                    #-sbcl nil)))
            
            ;; OpenAI provider
            (when (find-package :lantae-providers-openai)
              (if openai-key
                  (handler-case
                      (progn
                        (register-provider (funcall (intern "MAKE-OPENAI-PROVIDER" :lantae-providers-openai) 
                                                   :api-key openai-key))
                        (incf initialized-count)
                        (format t "✓ OpenAI provider registered~%"))
                    (error (e)
                      (format t "✗ OpenAI provider registration failed: ~A~%" e)))
                  (format t "OpenAI provider not registered: OpenAI API key not found. Set OPENAI_API_KEY environment variable.~%")))
            
            ;; Anthropic provider
            (when (find-package :lantae-providers-anthropic)
              (if anthropic-key
                  (handler-case
                      (progn
                        (register-provider (funcall (intern "MAKE-ANTHROPIC-PROVIDER" :lantae-providers-anthropic)
                                                   :api-key anthropic-key))
                        (incf initialized-count)
                        (format t "✓ Anthropic provider registered~%"))
                    (error (e)
                      (format t "✗ Anthropic provider registration failed: ~A~%" e)))
                  (format t "Anthropic provider not registered: Anthropic API key not found. Set ANTHROPIC_API_KEY environment variable.~%")))
            
            ;; Gemini provider
            (when (find-package :lantae-providers-gemini)
              (if gemini-key
                  (handler-case
                      (progn
                        (register-provider (funcall (intern "MAKE-GEMINI-PROVIDER" :lantae-providers-gemini)
                                                   :api-key gemini-key))
                        (incf initialized-count)
                        (format t "✓ Gemini provider registered~%"))
                    (error (e)
                      (format t "✗ Gemini provider registration failed: ~A~%" e)))
                  (format t "Gemini provider not registered: Gemini API key not found. Set GEMINI_API_KEY environment variable.~%")))
            
            ;; Mistral provider
            (when (find-package :lantae-providers-mistral)
              (if mistral-key
                  (handler-case
                      (progn
                        (register-provider (funcall (intern "MAKE-MISTRAL-PROVIDER" :lantae-providers-mistral)
                                                   :api-key mistral-key))
                        (incf initialized-count)
                        (format t "✓ Mistral provider registered~%"))
                    (error (e)
                      (format t "✗ Mistral provider registration failed: ~A~%" e)))
                  (format t "Mistral provider not registered: Mistral API key not found. Set MISTRAL_API_KEY environment variable.~%")))
            
            ;; Perplexity provider
            (when (find-package :lantae-providers-perplexity)
              (if perplexity-key
                  (handler-case
                      (progn
                        (register-provider (funcall (intern "MAKE-PERPLEXITY-PROVIDER" :lantae-providers-perplexity)
                                                   :api-key perplexity-key))
                        (incf initialized-count)
                        (format t "✓ Perplexity provider registered~%"))
                    (error (e)
                      (format t "✗ Perplexity provider registration failed: ~A~%" e)))
                  (format t "Perplexity provider not registered: Perplexity API key not found. Set PERPLEXITY_API_KEY environment variable.~%")))
            
            ;; Bedrock provider
            (when (find-package :lantae-providers-bedrock)
              (if aws-access-key
                  (handler-case
                      (progn
                        (register-provider (funcall (intern "MAKE-BEDROCK-PROVIDER" :lantae-providers-bedrock)))
                        (incf initialized-count)
                        (format t "✓ Bedrock provider registered~%"))
                    (error (e)
                      (format t "✗ Bedrock provider registration failed: ~A~%" e)))
                  (format t "Bedrock provider not registered: AWS credentials not found. Set AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY.~%")))
            
            (format t "Initialized ~A providers~%" initialized-count)))
      (error (e)
        (format t "Error initializing providers: ~A~%" e)
        ;; Ensure at least Ollama is available as fallback
        (unless (get-provider "ollama")
          (register-provider (make-ollama-provider))
          (format t "Registered fallback Ollama provider~%"))))))

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