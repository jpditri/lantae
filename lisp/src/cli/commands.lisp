;;;; commands.lisp - Macro-based command system for Lantae LISP
;;;;
;;;; Features:
;;;; - S-expression command definitions
;;;; - Macro-based command registration
;;;; - Tab completion support
;;;; - Help generation
;;;; - Command composition

(defpackage :lantae-commands
  (:use :cl)
  (:export #:defcommand
           #:register-command
           #:execute-command
           #:list-commands
           #:get-command-help
           #:complete-command
           #:*command-registry*
           #:command-exists-p
           #:with-command-context))

(in-package :lantae-commands)

;;; Command registry
(defvar *command-registry* (make-hash-table :test 'equal)
  "Global command registry")

(defvar *command-context* '()
  "Current command execution context")

;;; Command structure
(defstruct command
  name
  description
  usage
  function
  completion-function
  examples)

;;; Command registration macros
(defmacro defcommand (name (&rest args) description &key usage examples completion &body body)
  "Define a new command with macro support"
  (let ((cmd-name (string-downcase (string name)))
        (function-name (intern (format nil "COMMAND-~A" (string-upcase name)))))
    `(progn
       ;; Define the command function
       (defun ,function-name ,args
         ,@body)
       
       ;; Register the command
       (register-command
        (make-command
         :name ,cmd-name
         :description ,description
         :usage ,(or usage (format nil "~A~@[ ~{~A~^ ~}~]" cmd-name args))
         :function #',function-name
         :completion-function ,(when completion `#',completion)
         :examples ,examples))
       
       ;; Return the command name
       ',name)))

(defun register-command (command)
  "Register a command in the global registry"
  (setf (gethash (command-name command) *command-registry*) command)
  (format t "Registered command: ~A~%" (command-name command)))

;;; Command execution
(defun execute-command (command-line &optional context)
  "Execute a command from command line string"
  (let* ((*command-context* (append context *command-context*))
         (parts (split-command-line command-line))
         (cmd-name (first parts))
         (args (rest parts)))
    
    (if (string= cmd-name "")
        nil
        (let ((command (gethash cmd-name *command-registry*)))
          (if command
              (handler-case
                  (apply (command-function command) args)
                (error (e)
                  (format t "Error executing command '~A': ~A~%" cmd-name e)
                  nil))
              (progn
                (format t "Unknown command: ~A~%" cmd-name)
                (format t "Type 'help' for available commands.~%")
                nil))))))

(defun split-command-line (line)
  "Split command line into command and arguments"
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (if (string= trimmed "")
        '("")
        (split-string trimmed #\Space))))

(defun split-string (string delimiter)
  "Split string by delimiter"
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

;;; Command completion
(defun complete-command (partial-line)
  "Complete command or arguments"
  (let* ((parts (split-string partial-line #\Space))
         (cmd-name (first parts))
         (args (rest parts)))
    
    (if (and (= (length parts) 1) (not (char= (char partial-line (1- (length partial-line))) #\Space)))
        ;; Complete command name
        (complete-command-name cmd-name)
        ;; Complete command arguments
        (let ((command (gethash cmd-name *command-registry*)))
          (if (and command (command-completion-function command))
              (funcall (command-completion-function command) args)
              '())))))

(defun complete-command-name (partial)
  "Complete command name"
  (let ((matches '()))
    (maphash (lambda (name command)
               (declare (ignore command))
               (when (and (>= (length name) (length partial))
                         (string= partial name :end2 (length partial)))
                 (push name matches)))
             *command-registry*)
    (sort matches #'string<)))

;;; Command information
(defun list-commands ()
  "List all registered commands"
  (let ((commands '()))
    (maphash (lambda (name command)
               (push (list name (command-description command)) commands))
             *command-registry*)
    (sort commands (lambda (a b) (string< (first a) (first b))))))

(defun get-command-help (command-name)
  "Get help for specific command"
  (let ((command (gethash command-name *command-registry*)))
    (if command
        (format nil "~A~%~%~A~%~@[~%Usage: ~A~]~@[~%~%Examples:~{~%  ~A~}~]"
                (command-name command)
                (command-description command)
                (command-usage command)
                (command-examples command))
        (format nil "Command '~A' not found." command-name))))

(defun command-exists-p (name)
  "Check if command exists"
  (gethash name *command-registry*))

;;; Context management
(defmacro with-command-context (bindings &body body)
  "Execute body with additional command context"
  `(let ((*command-context* (append ,bindings *command-context*)))
     ,@body))

(defun get-context (key &optional default)
  "Get value from command context"
  (getf *command-context* key default))

;;; Default commands
(defcommand help (&optional command-name)
  "Show help for commands"
  :usage "help [command-name]"
  :examples '("help" "help provider" "help model")
  :completion (lambda (args)
                (when (= (length args) 1)
                  (complete-command-name (first args))))
  (if command-name
      (format t "~A~%" (get-command-help command-name))
      (progn
        (format t "Available commands:~%~%")
        (dolist (cmd-info (list-commands))
          (format t "  ~A~20T- ~A~%" (first cmd-info) (second cmd-info)))
        (format t "~%Type 'help <command>' for detailed help on a specific command.~%"))))

(defcommand provider (&optional provider-name model)
  "Switch provider or show current provider"
  :usage "provider [provider-name] [model]"
  :examples '("provider" "provider ollama" "provider openai gpt-4o")
  :completion (lambda (args)
                (cond
                  ((= (length args) 1)
                   ;; Complete provider names
                   (let ((providers '("ollama" "openai" "anthropic" "bedrock" "gemini" "mistral" "perplexity")))
                     (remove-if-not (lambda (p) (search (first args) p :test #'char-equal)) providers)))
                  ((= (length args) 2)
                   ;; Complete model names based on provider
                   (case (intern (string-upcase (first args)) :keyword)
                     (:ollama '("cogito:latest" "qwq:32b" "llama3.1-intuitive-thinker"))
                     (:openai '("gpt-4o" "gpt-4o-mini" "o1-preview"))
                     (:anthropic '("claude-3-5-sonnet-20241022" "claude-3-5-haiku-20241022"))
                     (t '())))
                  (t '())))
  (if provider-name
      (progn
        ;; Switch provider
        (lantae-config:set-config :provider provider-name)
        (when model
          (lantae-config:set-config :model model))
        (format t "Switched to provider: ~A~@[, model: ~A~]~%" 
                provider-name model)
        
        ;; Show provider info
        (show-provider-info provider-name))
      ;; Show current provider
      (let ((current-provider (lantae-config:get-config :provider))
            (current-model (lantae-config:get-config :model)))
        (format t "Current provider: ~A~%" current-provider)
        (format t "Current model: ~A~%" current-model))))

(defun show-provider-info (provider-name)
  "Show information about a provider"
  (let ((provider (lantae-providers:get-provider provider-name)))
    (if provider
        (let ((capabilities (lantae-providers:provider-capabilities provider)))
          (format t "~%Provider capabilities:~%")
          (format t "  Streaming: ~A~%" (getf capabilities :streaming))
          (format t "  Tools: ~A~%" (getf capabilities :tools))
          (format t "  Max tokens: ~A~%" (getf capabilities :max-tokens)))
        (format t "Provider ~A not available~%" provider-name))))

(defcommand model (&rest model-parts)
  "Switch to a different model"
  :usage "model <model-name>"
  :examples '("model cogito:latest" "model gpt-4o" "model claude-3-5-sonnet-20241022")
  (if model-parts
      (let ((model-name (format nil "~{~A~^ ~}" model-parts)))
        (lantae-config:set-config :model model-name)
        (format t "Switched to model: ~A~%" model-name))
      (format t "Current model: ~A~%" (lantae-config:get-config :model))))

(defcommand config (&optional key value)
  "Show or set configuration values"
  :usage "config [key] [value]"
  :examples '("config" "config temperature" "config temperature 0.5")
  (cond
    ((and key value)
     ;; Set configuration value
     (lantae-config:set-config key (parse-config-value value))
     (format t "Set ~A = ~A~%" key value))
    (key
     ;; Show specific configuration value
     (format t "~A = ~A~%" key (lantae-config:get-config key)))
    (t
     ;; Show all configuration
     (format t "Current configuration:~%")
     (let ((config (lantae-config:config-to-plist)))
       (loop for (k v) on config by #'cddr
             do (format t "  ~A: ~A~%" k v))))))

(defun parse-config-value (string)
  "Parse configuration value from string"
  (cond
    ((string-equal string "true") t)
    ((string-equal string "false") nil)
    ((string-equal string "nil") nil)
    ((every #'digit-char-p string) (parse-integer string))
    ((and (find #\. string)
          (every (lambda (c) (or (digit-char-p c) (char= c #\.))) string))
     (read-from-string string))
    (t string)))

(defcommand clear ()
  "Clear conversation history"
  :usage "clear"
  (setf lantae:*conversation-history* '())
  (format t "Conversation cleared.~%"))

(defcommand info ()
  "Show system information"
  :usage "info"
  (format t "Lantae LISP v~A~%" lantae:*version*)
  (format t "Provider: ~A~%" (lantae-config:get-config :provider))
  (format t "Model: ~A~%" (lantae-config:get-config :model))
  (format t "Temperature: ~A~%" (lantae-config:get-config :temperature))
  (format t "Conversation length: ~A messages~%" (length lantae:*conversation-history*)))

(defcommand quit ()
  "Exit the REPL"
  :usage "quit"
  (format t "Goodbye!~%")
  (quit))

;;; Command aliases
(defun register-alias (alias command-name)
  "Register command alias"
  (let ((original-command (gethash command-name *command-registry*)))
    (when original-command
      (setf (gethash alias *command-registry*) original-command))))

;; Register some common aliases
(register-alias "h" "help")
(register-alias "p" "provider")
(register-alias "m" "model")
(register-alias "c" "config")
(register-alias "exit" "quit")
(register-alias "q" "quit")

;;; Advanced command features
(defmacro defcommand-group (name commands)
  "Define a group of related commands"
  `(progn
     ,@(loop for (cmd-name args desc . body) in commands
             collect `(defcommand ,cmd-name ,args ,desc ,@body))
     ',name))

(defun command-pipeline (&rest command-specs)
  "Create a command pipeline"
  (lambda (&rest args)
    (loop for spec in command-specs
          for result = args then (list result)
          do (setf result (apply #'execute-command spec result))
          finally (return result))))

;;; Export command system interface
(export '(defcommand register-command execute-command list-commands
          get-command-help complete-command command-exists-p
          with-command-context get-context))