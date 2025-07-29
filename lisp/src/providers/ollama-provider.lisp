;;;; ollama-provider.lisp - Ollama provider implementation
;;;;
;;;; Implements the Ollama provider with:
;;;; - Chat completion
;;;; - Model listing
;;;; - Streaming support
;;;; - Error handling

(defpackage :lantae-providers-ollama
  (:use :cl :lantae-http :lantae-providers)
  (:export #:make-ollama-provider
           #:ollama-chat
           #:ollama-stream
           #:ollama-list-models
           #:ollama-pull-model))

(in-package :lantae-providers-ollama)

;;; Ollama API endpoints
(defparameter *ollama-endpoints*
  '(:chat "api/chat"
    :generate "api/generate"
    :models "api/tags"
    :pull "api/pull"
    :show "api/show"))

(defun get-endpoint (base-url endpoint)
  "Get full URL for Ollama endpoint"
  (build-url base-url (getf *ollama-endpoints* endpoint)))

;;; Chat implementation
(defun ollama-chat (base-url model messages temperature)
  "Send chat request to Ollama"
  (let* ((url (get-endpoint base-url :chat))
         (request-body `((:model . ,model)
                        (:messages . ,(mapcar #'format-message messages))
                        (:temperature . ,temperature)
                        (:stream . nil)))
         (result (http-post url request-body)))
    
    (if (http-result-success-p result)
        (let* ((response (parse-json-response (http-result-data result)))
               (message (cdr (assoc :message response))))
          (if message
              (success `(:role ,(cdr (assoc :role message))
                        :content ,(cdr (assoc :content message))))
              (failure "No message in response")))
        (failure (http-result-error result)))))

(defun ollama-stream (base-url model messages temperature callback)
  "Stream chat response from Ollama"
  (let* ((url (get-endpoint base-url :chat))
         (request-body `((:model . ,model)
                        (:messages . ,(mapcar #'format-message messages))
                        (:temperature . ,temperature)
                        (:stream . t)))
         (accumulated-response ""))
    
    (let ((stream-result 
           (http-stream url
                        :method :post
                        :content request-body
                        :callback (when (find-package :lantae-streaming)
                                   (funcall (intern "CREATE-STREAMING-CALLBACK" :lantae-streaming)
                                           "ollama"
                                           :on-token (lambda (token)
                                                      (when callback
                                                        (funcall callback token))
                                                      (setf accumulated-response 
                                                            (concatenate 'string accumulated-response token)))
                                           :on-complete (lambda ()
                                                         ;; Return the complete accumulated response
                                                         nil)
                                           :on-error (lambda (error)
                                                      (format t "~%Streaming error: ~A~%" error)))))))
      (if (http-result-success-p stream-result)
          (success `(:role "assistant" :content ,accumulated-response))
          (failure (http-result-error stream-result))))))

;;; Model management
(defun ollama-list-models (base-url)
  "List available Ollama models"
  (let* ((url (get-endpoint base-url :models))
         (result (http-get url)))
    
    (if (http-result-success-p result)
        (let* ((response (parse-json-response (http-result-data result)))
               (models (cdr (assoc :models response))))
          (success (mapcar (lambda (model)
                            (cdr (assoc :name model)))
                          models)))
        (failure (http-result-error result)))))

(defun ollama-pull-model (base-url model-name)
  "Pull a model from Ollama registry"
  (let* ((url (get-endpoint base-url :pull))
         (request-body `((:name . ,model-name)
                        (:stream . nil)))
         (result (http-post url request-body :timeout 300))) ; Long timeout for downloads
    
    (if (http-result-success-p result)
        (success t)
        (failure (http-result-error result)))))

(defun ollama-model-info (base-url model-name)
  "Get information about a model"
  (let* ((url (get-endpoint base-url :show))
         (request-body `((:name . ,model-name)))
         (result (http-post url request-body)))
    
    (if (http-result-success-p result)
        (let ((response (parse-json-response (http-result-data result))))
          (success response))
        (failure (http-result-error result)))))

;;; Helper functions
(defun format-message (message)
  "Format message for Ollama API"
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

;;; Provider factory
(defun make-ollama-provider (&key (base-url "http://localhost:11434"))
  "Create Ollama provider instance"
  (lantae-providers:create-provider
   :name "ollama"
   :chat-fn (lambda (model messages temperature)
             (ollama-chat base-url model messages temperature))
   :stream-fn (lambda (model messages temperature callback)
               (ollama-stream base-url model messages temperature callback))
   :models-fn (lambda ()
               (ollama-list-models base-url))
   :config `(:base-url ,base-url
            :supports-streaming t
            :supports-tools nil)))

;;; Connection testing
(defun test-ollama-connection (&key (base-url "http://localhost:11434"))
  "Test if Ollama is running and accessible"
  (let ((result (ollama-list-models base-url)))
    (if (result-success-p result)
        (format t "✓ Ollama is running at ~A~%" base-url)
        (format t "✗ Cannot connect to Ollama at ~A: ~A~%" 
                base-url 
                (result-error result)))
    (result-success-p result)))

;;; Ollama-specific features
(defun format-for-code-generation (prompt language)
  "Format prompt for code generation"
  `((:role . "system")
    (:content . ,(format nil "You are an expert ~A programmer. Generate clean, idiomatic code with proper error handling." language)))
  `((:role . "user")
    (:content . ,prompt)))

(defun format-for-analysis (code language)
  "Format prompt for code analysis"
  `((:role . "system")
    (:content . "You are a code review expert. Analyze the code for bugs, security issues, and improvements."))
  `((:role . "user")
    (:content . ,(format nil "Analyze this ~A code:~%~%```~A~%~A~%```" language language code))))

;;; Model selection helpers
(defparameter *model-capabilities*
  '(("cogito:latest" . (:reasoning t :code t :fast t))
    ("llama3:latest" . (:general t :code t :fast t))
    ("mistral:latest" . (:general t :code t :fast t))
    ("codellama:latest" . (:code t :specialized t))
    ("deepseek-coder:latest" . (:code t :specialized t))
    ("qwen2.5-coder:latest" . (:code t :reasoning t))))

(defun suggest-model-for-task (task-type)
  "Suggest best model for a given task type"
  (case task-type
    (:code '("codellama:latest" "deepseek-coder:latest" "qwen2.5-coder:latest"))
    (:reasoning '("cogito:latest" "qwen2.5-coder:latest"))
    (:general '("llama3:latest" "mistral:latest"))
    (t '("cogito:latest"))))