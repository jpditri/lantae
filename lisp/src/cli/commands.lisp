;;;; commands.lisp - Command system implementation for Lantae LISP
;;;;
;;;; Implements slash commands with:
;;;; - Command registry and dispatch
;;;; - Built-in commands (/help, /model, /provider, etc.)
;;;; - Tab completion support
;;;; - Command history

(defpackage :lantae-commands
  (:use :cl)
  (:export #:*command-registry*
           #:register-command
           #:execute-command
           #:register-all-commands
           #:list-commands
           #:get-command-completions))

(in-package :lantae-commands)

;;; Command registry
(defvar *command-registry* (make-hash-table :test 'equal)
  "Registry of available commands")

(defstruct command
  name
  function
  description
  usage
  completions-fn)

;;; Command registration
(defun register-command (name function &key description usage completions-fn)
  "Register a command in the registry"
  (setf (gethash name *command-registry*)
        (make-command :name name
                     :function function
                     :description description
                     :usage usage
                     :completions-fn completions-fn)))

(defun get-command (name)
  "Get command by name"
  (gethash name *command-registry*))

(defun list-commands ()
  "List all registered commands"
  (loop for name being the hash-keys of *command-registry*
        collect name into names
        finally (return (sort names #'string<))))

;;; Command execution
(defun parse-command-line (input)
  "Parse command line into command and arguments"
  (let* ((parts (lantae-utils:split-string input))
         (command (first parts))
         (args (rest parts)))
    (values command args)))

(defun execute-command (input)
  "Execute a slash command"
  (multiple-value-bind (command-name args) (parse-command-line input)
    (let ((command (get-command command-name)))
      (if command
          (handler-case
              (funcall (command-function command) args)
            (error (e)
              (format t "Error executing command: ~A~%" e)))
          (format t "Unknown command: /~A. Type /help for available commands.~%" 
                  command-name)))))

;;; Tab completion
(defun get-command-completions (prefix)
  "Get command completions for prefix"
  (loop for name being the hash-keys of *command-registry*
        when (and (>= (length name) (length prefix))
                  (string= prefix name :end2 (length prefix)))
        collect name into matches
        finally (return (sort matches #'string<))))

(defun get-argument-completions (command-name partial-arg position)
  "Get argument completions for a command"
  (let ((command (get-command command-name)))
    (when (and command (command-completions-fn command))
      (funcall (command-completions-fn command) partial-arg position))))

;;; Built-in commands implementation

(defun cmd-help (args)
  "Show help for commands"
  (declare (ignore args))
  (format t "~%Available commands:~%~%")
  (loop for name being the hash-keys of *command-registry*
        using (hash-value command)
        do (format t "  /~A~@[~30T~A~]~%" 
                   name 
                   (command-description command)))
  (format t "~%Type /help <command> for detailed help on a specific command.~%"))

(defun cmd-provider (args)
  "Switch provider or show current"
  (cond
    ;; No args - show current
    ((null args)
     (format t "Current provider: ~A~%" (lantae:get-config :provider))
     (format t "Available providers: ~{~A~^, ~}~%" 
             (lantae-providers:list-providers)))
    
    ;; Switch provider
    (t
     (let ((provider-name (first args)))
       (if (lantae-providers:get-provider provider-name)
           (progn
             (lantae:set-config :provider provider-name)
             (format t "Switched to provider: ~A~%" provider-name))
           (format t "Unknown provider: ~A~%" provider-name))))))

(defun cmd-model (args)
  "Switch model or show current"
  (cond
    ;; No args - show current
    ((null args)
     (format t "Current model: ~A~%" (lantae:get-config :model))
     (let ((models (lantae-providers:list-provider-models 
                   (lantae:get-config :provider))))
       (when models
         (format t "Available models: ~{~A~^, ~}~%" models))))
    
    ;; Switch model
    (t
     (let ((model-name (first args)))
       (lantae:set-config :model model-name)
       (format t "Switched to model: ~A~%" model-name)))))

(defun cmd-models (args)
  "List available models for current provider"
  (declare (ignore args))
  (let* ((provider (lantae:get-config :provider))
         (models (lantae-providers:list-provider-models provider)))
    (if models
        (progn
          (format t "~%Available models for ~A:~%" provider)
          (dolist (model models)
            (format t "  - ~A~%" model)))
        (format t "No models available or provider not connected.~%"))))

(defun cmd-clear (args)
  "Clear conversation history"
  (declare (ignore args))
  (setf lantae:*conversation-history* nil)
  (format t "Conversation history cleared.~%"))

(defun cmd-info (args)
  "Show current configuration"
  (declare (ignore args))
  (format t "~%Current Configuration:~%")
  (format t "  Provider: ~A~%" (lantae:get-config :provider))
  (format t "  Model: ~A~%" (lantae:get-config :model))
  (format t "  Temperature: ~A~%" (lantae:get-config :temperature))
  (format t "  Auto-accept: ~A~%" (lantae:get-config :auto-accept))
  (format t "  Planning mode: ~A~%" (lantae:get-config :planning-mode))
  (format t "  Agent mode: ~A~%" (lantae:get-config :agent-mode))
  (format t "~%"))

(defun cmd-env (args)
  "Check environment variables"
  (declare (ignore args))
  (format t "~%Environment Variables:~%")
  (dolist (var '("OPENAI_API_KEY" "ANTHROPIC_API_KEY" "GEMINI_API_KEY"
                "MISTRAL_API_KEY" "PERPLEXITY_API_KEY" "AWS_PROFILE"))
    (let ((value #+sbcl (sb-ext:posix-getenv var)
                 #-sbcl nil))
      (format t "  ~A: ~A~%" 
              var 
              (if value 
                  (if (> (length value) 3)
                      (format nil "~A...~A" 
                              (subseq value 0 3)
                              (subseq value (- (length value) 3)))
                      "***")
                  "Not set"))))
  (format t "~%"))

(defun cmd-history (args)
  "Show conversation history"
  (declare (ignore args))
  (if lantae:*conversation-history*
      (progn
        (format t "~%Conversation History:~%")
        (loop for message in (reverse lantae:*conversation-history*)
              for i from 1
              do (format t "~%[~A] ~A:~%~A~%"
                        i
                        (getf message :role)
                        (getf message :content))))
      (format t "No conversation history.~%")))

(defun cmd-temperature (args)
  "Set response temperature"
  (if args
      (handler-case
          (let ((temp (parse-float (first args))))
            (if (and (>= temp 0.0) (<= temp 2.0))
                (progn
                  (lantae:set-config :temperature temp)
                  (format t "Temperature set to: ~A~%" temp))
                (format t "Temperature must be between 0.0 and 2.0~%")))
        (error ()
          (format t "Invalid temperature value~%")))
      (format t "Current temperature: ~A~%" (lantae:get-config :temperature))))

(defun cmd-quit (args)
  "Exit the REPL"
  (declare (ignore args))
  (format t "Goodbye!~%")
  (lantae-utils:quit))

;;; Command completion functions

(defun provider-completions (partial position)
  "Get provider name completions"
  (declare (ignore position))
  (loop for provider in (lantae-providers:list-providers)
        when (and (>= (length provider) (length partial))
                  (string= partial provider :end2 (length partial)))
        collect provider))

(defun model-completions (partial position)
  "Get model name completions"
  (declare (ignore position))
  (let ((models (lantae-providers:list-provider-models 
                (lantae:get-config :provider))))
    (when models
      (loop for model in models
            when (and (>= (length model) (length partial))
                      (string= partial model :end2 (length partial)))
            collect model))))

;;; Register all built-in commands

(defun register-all-commands ()
  "Register all built-in commands"
  (register-command "help" #'cmd-help
                   :description "Show available commands"
                   :usage "/help [command]")
  
  (register-command "provider" #'cmd-provider
                   :description "Switch provider or show current"
                   :usage "/provider [name]"
                   :completions-fn #'provider-completions)
  
  (register-command "model" #'cmd-model
                   :description "Switch model or show current"
                   :usage "/model [name]"
                   :completions-fn #'model-completions)
  
  (register-command "models" #'cmd-models
                   :description "List available models"
                   :usage "/models")
  
  (register-command "clear" #'cmd-clear
                   :description "Clear conversation history"
                   :usage "/clear")
  
  (register-command "info" #'cmd-info
                   :description "Show current configuration"
                   :usage "/info")
  
  (register-command "env" #'cmd-env
                   :description "Check environment variables"
                   :usage "/env")
  
  (register-command "history" #'cmd-history
                   :description "Show conversation history"
                   :usage "/history")
  
  (register-command "temperature" #'cmd-temperature
                   :description "Set response temperature (0.0-2.0)"
                   :usage "/temperature [value]")
  
  (register-command "quit" #'cmd-quit
                   :description "Exit the REPL"
                   :usage "/quit")
  
  (register-command "exit" #'cmd-quit
                   :description "Exit the REPL"
                   :usage "/exit")
  
  ;; Return success
  t)

;;; Utility for pretty printing
(defun parse-float (string)
  "Parse string to float"
  (handler-case
      (read-from-string string)
    (error ()
      (error "Invalid number: ~A" string))))