;;;; config.lisp - S-expression based configuration system
;;;;
;;;; Functional configuration management with:
;;;; - S-expression native format
;;;; - Environment variable integration
;;;; - Hot-reload capabilities
;;;; - Validation and type checking
;;;; - Nested configuration access

(defpackage :lantae-config
  (:use :cl)
  (:import-from :lantae-utils
                #:split-string
                #:merge-plists
                #:format-time-string
                #:parse-value)
  (:export #:*config*
           #:load-config
           #:get-config
           #:set-config
           #:merge-config
           #:validate-config
           #:watch-config
           #:reload-config
           #:config-to-plist
           #:config-from-plist
           #:with-config-binding
           #:defconfig))

(in-package :lantae-config)

;;; Configuration state
(defvar *config* '()
  "Global configuration property list")

(defvar *config-watchers* '()
  "List of configuration change watchers")

(defvar *config-file* nil
  "Currently loaded configuration file")

;;; Forward declarations for functions used before definition
(declaim (ftype (function (t t) t) notify-watchers))
(declaim (ftype (function () t) find-config-file setup-environment-overrides))
(declaim (ftype (function (t) t) config-valid-p parse-env-value))

;;; Configuration access utilities
(defun get-config (key &optional default)
  "Get configuration value with dotted key support"
  (labels ((get-nested (keys plist)
             (if (null keys)
                 plist
                 (let ((value (getf plist (first keys))))
                   (if (and value (listp value) (not (keywordp value)) (rest keys))
                       (get-nested (rest keys) value)
                       (or value default))))))
    (cond
      ;; Simple keyword lookup
      ((keywordp key)
       (getf *config* key default))
      ;; String with dots - nested lookup
      ((and (stringp key) (find #\. key))
       (let ((keys (mapcar (lambda (s) (intern (string-upcase s) :keyword))
                           (split-string key #\.))))
         (get-nested keys *config*)))
      ;; Symbol or string - simple lookup
      (t (getf *config* (intern (string-upcase (string key)) :keyword) default)))))

(defun set-config (key value)
  "Set configuration value with nested key support"
  (labels ((set-nested (keys plist value)
             (if (null (rest keys))
                 (setf (getf plist (first keys)) value)
                 (let ((sublist (getf plist (first keys))))
                   (unless (and sublist (listp sublist) (not (keywordp sublist)))
                     (setf (getf plist (first keys)) '())
                     (setf sublist (getf plist (first keys))))
                   (set-nested (rest keys) sublist value)))))
    (cond
      ;; Simple keyword set
      ((keywordp key)
       (setf (getf *config* key) value))
      ;; String with dots - nested set
      ((and (stringp key) (find #\. key))
       (let ((keys (mapcar (lambda (s) (intern (string-upcase s) :keyword))
                           (split-string key #\.))))
         (set-nested keys *config* value)))
      ;; Symbol or string - simple set
      (t (setf (getf *config* (intern (string-upcase (string key)) :keyword)) value)))
    
    ;; Notify watchers
    (notify-watchers key value)
    value))

(defun merge-config (new-config)
  "Merge new configuration with existing"
  (setf *config* (merge-plists *config* new-config))
  (notify-watchers :config-merged new-config))

;;; Configuration watching
(defun watch-config (pattern callback)
  "Register a configuration change watcher"
  (let ((watcher (list pattern callback)))
    (push watcher *config-watchers*)
    watcher))

(defun unwatch-config (watcher)
  "Unregister a configuration watcher"
  (setf *config-watchers* (remove watcher *config-watchers*)))

(defun notify-watchers (key value)
  "Notify all matching watchers of configuration change"
  (dolist (watcher *config-watchers*)
    (let ((pattern (first watcher))
          (callback (second watcher)))
      (when (or (null pattern)
                (eq pattern key)
                (and (stringp pattern) (stringp key) (search pattern key)))
        (handler-case
            (funcall callback key value)
          (error (e)
            (format t "Config watcher error: ~A~%" e)))))))

;;; Configuration file operations
(defun find-config-file ()
  "Find configuration file in standard locations"
  (let ((possible-files
         (list "lantae.lisp"
               "config/lantae.lisp"
               (merge-pathnames ".lantae/config.lisp" (user-homedir-pathname))
               "/etc/lantae/config.lisp")))
    (find-if #'probe-file possible-files)))

(defun config-valid-p (config)
  "Check if configuration is valid S-expression format"
  (and (listp config)
       (evenp (length config))
       (every (lambda (item) (keywordp item))
               (loop for i from 0 below (length config) by 2
                     collect (nth i config)))))

(defun parse-env-value (value)
  "Parse environment variable value to appropriate type"
  (cond
    ((string-equal value "true") t)
    ((string-equal value "false") nil)
    ((string-equal value "nil") nil)
    ((every #'digit-char-p value) (parse-integer value))
    ((and (find #\. value)
          (every (lambda (c) (or (digit-char-p c) (char= c #\.))) value))
     (read-from-string value))
    (t value)))

(defun setup-environment-overrides ()
  "Override configuration with environment variables"
  (let ((env-mappings
         '(("LANTAE_MODEL" . :model)
           ("LANTAE_PROVIDER" . :provider)
           ("LANTAE_TEMPERATURE" . :temperature)
           ("LANTAE_AUTO_ACCEPT" . :auto-accept)
           ("LANTAE_ENABLE_MCP" . :enable-mcp)
           ("LANTAE_ENABLE_LSP" . :enable-lsp)
           ("AWS_REGION" . :region))))
    
    (dolist (mapping env-mappings)
      (let ((env-value #+sbcl (sb-ext:posix-getenv (car mapping))
                       #-sbcl nil))
        (when env-value
          (set-config (cdr mapping) (parse-env-value env-value)))))))

(defun load-config (&optional config-file)
  "Load configuration from file"
  (let ((file (or config-file (find-config-file))))
    (when (and file (probe-file file))
      (handler-case
          (with-open-file (stream file :direction :input)
            (let ((config-data (read stream)))
              (when (config-valid-p config-data)
                (setf *config* config-data)
                (setf *config-file* file)
                (setup-environment-overrides)
                (notify-watchers :config-loaded file)
                t)))
        (error (e)
          (format t "Warning: Failed to load config from ~A: ~A~%" file e)
          nil)))))

(defun save-config (&optional (file *config-file*))
  "Save current configuration to file"
  (when file
    (handler-case
        (with-open-file (stream file :direction :output :if-exists :supersede)
          (format stream ";;;; Lantae Configuration~%")
          (format stream ";;;; Generated on ~A~%~%" (format-time-string))
          (write *config* :stream stream :pretty t)
          (format stream "~%")
          (notify-watchers :config-saved file)
          t)
      (error (e)
        (format t "Error saving config to ~A: ~A~%" file e)
        nil))))

(defun reload-config ()
  "Reload configuration from file"
  (when *config-file*
    (let ((old-config (copy-list *config*)))
      (when (load-config *config-file*)
        (notify-watchers :config-reloaded (list :old old-config :new *config*))
        t))))

;;; Configuration validation
(defun validate-config (&optional (config *config*))
  "Validate current configuration"
  (declare (ignore config))
  (let ((errors '()))
    
    ;; Check required fields
    (unless (member (get-config :provider) '("ollama" "openai" "anthropic" "bedrock" "gemini" "mistral" "perplexity") :test #'string=)
      (push "Invalid or missing provider" errors))
    
    ;; Check temperature range
    (let ((temp (get-config :temperature)))
      (when (and temp (not (and (numberp temp) (<= 0 temp 2))))
        (push "Temperature must be a number between 0 and 2" errors)))
    
    ;; Check performance settings
    (let ((cache-ttl (get-config :performance.cache-ttl))
          (pool-size (get-config :performance.pool-size)))
      (when (and cache-ttl (not (and (numberp cache-ttl) (> cache-ttl 0))))
        (push "Cache TTL must be positive number" errors))
      (when (and pool-size (not (and (integerp pool-size) (> pool-size 0))))
        (push "Pool size must be positive integer" errors)))
    
    ;; Check security settings
    (let ((rpm (get-config :security.requests-per-minute)))
      (when (and rpm (not (and (integerp rpm) (> rpm 0))))
        (push "Requests per minute must be positive integer" errors)))
    
    (if errors
        (values nil errors)
        (values t nil))))

;;; Configuration macros
(defmacro defconfig (name value &optional documentation)
  "Define a configuration parameter with default value"
  `(progn
     (set-config ,(intern (string name) :keyword) ,value)
     ,@(when documentation
         `((setf (documentation ',name 'variable) ,documentation)))
     ',name))

(defmacro with-config-binding (bindings &body body)
  "Temporarily bind configuration values"
  `(let ((*config* (copy-list *config*)))
     ,@(loop for (key value) in bindings
             collect `(set-config ,key ,value))
     ,@body))

;;; Configuration templates
(defun default-config ()
  "Return default configuration template"
  '(:model "cogito:latest"
    :provider "ollama"
    :url "http://localhost:11434"
    :region "us-east-1"
    :secret "lantae/api-keys"
    :temperature 0.1
    :auto-accept nil
    :planning-mode nil
    :agent-mode nil
    :no-banner nil
    :enable-mcp nil
    :enable-lsp nil
    :max-retries 3
    :retry-delay 1.0
    :performance (:enable-caching t
                  :cache-ttl 300
                  :enable-pooling t
                  :pool-size 5
                  :request-timeout 30)
    :security (:enable-rate-limiting t
               :requests-per-minute 60
               :enable-audit-log nil
               :audit-log-path "logs/audit.log")))

(defun config-to-plist ()
  "Convert current configuration to property list"
  (copy-list *config*))

(defun config-from-plist (plist)
  "Load configuration from property list"
  (when (config-valid-p plist)
    (setf *config* (copy-list plist))
    t))

;;; Example configuration file format
(defun generate-example-config ()
  "Generate example configuration file"
  '(:model "cogito:latest"
    :provider "ollama"
    :url "http://localhost:11434"
    :temperature 0.1
    :auto-accept nil
    :planning-mode nil
    :agent-mode nil
    :no-banner nil
    :enable-mcp nil
    :enable-lsp nil
    :max-retries 3
    :retry-delay 1.0
    :performance (:enable-caching t
                  :cache-ttl 300
                  :enable-pooling t
                  :pool-size 5
                  :request-timeout 30)
    :security (:enable-rate-limiting t
               :requests-per-minute 60
               :enable-audit-log nil
               :audit-log-path "logs/audit.log")
    :providers ((:name "ollama"
                 :url "http://localhost:11434"
                 :models ("cogito:latest" "llama2:latest" "mistral:latest"))
                (:name "openai"
                 :api-key-env "OPENAI_API_KEY"
                 :models ("gpt-4" "gpt-3.5-turbo"))
                (:name "anthropic"
                 :api-key-env "ANTHROPIC_API_KEY"
                 :models ("claude-3-opus" "claude-3-sonnet")))))