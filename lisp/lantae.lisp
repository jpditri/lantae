;;;; lantae.lisp - Main entry point for Lantae LISP implementation
;;;; 
;;;; A functional programming implementation of the Lantae multi-provider LLM CLI
;;;; Features:
;;;; - S-expression based configuration
;;;; - Macro-based command system
;;;; - Functional provider abstraction
;;;; - Native LISP REPL integration

(defpackage :lantae
  (:use :cl)
  (:import-from :lantae-utils
                #:split-string
                #:join-strings
                #:parse-number
                #:parse-value
                #:quit)
  (:export #:main
           #:start-repl
           #:send-single-prompt
           #:*version*
           #:*default-config*
           #:*conversation-history*))

(in-package :lantae)

;;; Version and constants
(defparameter *version* "1.0.0-lisp")

(defparameter *default-config*
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

;;; Global state management using functional approach
(defvar *current-config* (copy-list *default-config*))
(defvar *current-provider* nil)
(defvar *conversation-history* '())
(defvar *command-registry* (make-hash-table :test 'equal))

;;; Configuration access functions
(defun get-config (key &optional default)
  "Retrieve configuration value using dotted key notation"
  (labels ((get-nested (keys plist)
             (if (null keys)
                 plist
                 (let ((value (getf plist (first keys))))
                   (if (and value (listp value) (rest keys))
                       (get-nested (rest keys) value)
                       (or value default))))))
    (if (symbolp key)
        (getf *current-config* key default)
        (get-nested (mapcar #'intern (split-string (string key) #\.)) *current-config*))))

(defun set-config (key value)
  "Set configuration value"
  (setf (getf *current-config* (intern (string key))) value))

(defun merge-config (config-plist)
  "Merge configuration plist with current config"
  (setf *current-config* (append config-plist *current-config*)))

;;; Banner and help functions
(defun print-banner ()
  "Print colorful ASCII banner"
  (unless (get-config :no-banner)
    (format t "~%")
    (format t "╔══════════════════════════════════════════════════════════════╗~%")
    (format t "║  ██╗      █████╗ ███╗   ██╗████████╗ █████╗ ███████╗  ║~%")
    (format t "║  ██║     ██╔══██╗████╗  ██║╚══██╔══╝██╔══██╗██╔════╝  ║~%")
    (format t "║  ██║     ███████║██╔██╗ ██║   ██║   ███████║█████╗    ║~%")
    (format t "║  ██║     ██╔══██║██║╚██╗██║   ██║   ██╔══██║██╔══╝    ║~%")
    (format t "║  ███████╗██║  ██║██║ ╚████║   ██║   ██║  ██║███████╗  ║~%")
    (format t "║  ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝╚══════╝  ║~%")
    (format t "║                                                              ║~%")
    (format t "║  🚀 Multi-Provider LLM Interface v~A (LISP Edition)  ║~%" *version*)
    (format t "║  ⚡ Powered by Functional Programming Paradigms            ║~%")
    (format t "║  🔗 S-expressions • Macros • Pure Functions               ║~%")
    (format t "╚══════════════════════════════════════════════════════════════╝~%")
    (format t "~%")))

(defun print-help ()
  "Print help information"
  (format t "Lantae LISP v~A - Multi-Provider LLM Interface~%" *version*)
  (format t "~%Usage: lantae [options] [prompt]~%")
  (format t "~%Options:~%")
  (format t "  -m, --model MODEL        Specify the model to use~%")
  (format t "  -p, --provider PROVIDER  Specify the provider~%")
  (format t "  -t, --temperature TEMP   Temperature for responses~%")
  (format t "  -y, --auto-accept       Auto-accept all prompts~%")
  (format t "      --planning-mode     Enable planning mode~%")
  (format t "      --agent            Enable agent mode~%")
  (format t "      --no-banner        Disable startup banner~%")
  (format t "      --enable-mcp       Enable MCP support~%")
  (format t "      --enable-lsp       Enable LSP support~%")
  (format t "  -h, --help              Show this help~%")
  (format t "  -v, --version           Show version~%")
  (format t "~%Examples:~%")
  (format t "  lantae                           # Start interactive REPL~%")
  (format t "  lantae \"Explain LISP macros\"     # Single prompt~%")
  (format t "  lantae --agent \"Build a web app\" # Agent mode~%"))

;;; Command line argument parsing
(defun parse-arguments (args)
  "Parse command line arguments into configuration overrides"
  (let ((config-overrides '())
        (prompt-args '())
        (skip-next nil))
    (loop for (arg . rest-args) on args
          unless skip-next
          do (cond
               ;; Long options
               ((string= arg "--auto-accept") 
                (push :auto-accept config-overrides)
                (push t config-overrides))
               ((string= arg "--planning-mode")
                (push :planning-mode config-overrides)
                (push t config-overrides))
               ((string= arg "--agent")
                (push :agent-mode config-overrides)
                (push t config-overrides))
               ((string= arg "--no-banner")
                (push :no-banner config-overrides)
                (push t config-overrides))
               ((string= arg "--enable-mcp")
                (push :enable-mcp config-overrides)
                (push t config-overrides))
               ((string= arg "--enable-lsp")
                (push :enable-lsp config-overrides)
                (push t config-overrides))
               ((string= arg "--help")
                (print-help)
                (quit))
               ((string= arg "--version")
                (format t "Lantae LISP v~A~%" *version*)
                (quit))
               ;; Options with values
               ((string= arg "--model")
                (when rest-args
                  (push :model config-overrides)
                  (push (first rest-args) config-overrides)
                  (setf skip-next t)))
               ((string= arg "--provider")
                (when rest-args
                  (push :provider config-overrides)
                  (push (first rest-args) config-overrides)
                  (setf skip-next t)))
               ((string= arg "--temperature")
                (when rest-args
                  (push :temperature config-overrides)
                  (push (parse-number (first rest-args)) config-overrides)
                  (setf skip-next t)))
               ;; --key=value format
               ((and (> (length arg) 2) (string= (subseq arg 0 2) "--"))
                (let ((eq-pos (position #\= arg)))
                  (when eq-pos
                    (let ((key (intern (string-upcase (subseq arg 2 eq-pos))))
                          (value (subseq arg (1+ eq-pos))))
                      (push key config-overrides)
                      (push (parse-value value) config-overrides)))))
               ;; Short options
               ((and (> (length arg) 1) (string= (subseq arg 0 1) "-"))
                (case (char arg 1)
                  (#\m (when rest-args
                         (push :model config-overrides)
                         (push (first rest-args) config-overrides)
                         (setf skip-next t)))
                  (#\p (when rest-args
                         (push :provider config-overrides)
                         (push (first rest-args) config-overrides)
                         (setf skip-next t)))
                  (#\t (when rest-args
                         (push :temperature config-overrides)
                         (push (parse-number (first rest-args)) config-overrides)
                         (setf skip-next t)))
                  (#\y (push :auto-accept config-overrides) (push t config-overrides))
                  (#\h (print-help) (quit))
                  (#\v (format t "Lantae LISP v~A~%" *version*) (quit))))
               ;; Regular arguments
               (t (push arg prompt-args)))
          else do (setf skip-next nil))
    (values (nreverse config-overrides) (nreverse prompt-args))))

;;; System initialization
(defun load-configuration ()
  "Load configuration from file"
  (let ((config-files '("lantae.lisp" "config/lantae.lisp" 
                       "~/.lantae/config.lisp" "/etc/lantae/config.lisp")))
    (dolist (file config-files)
      (when (probe-file file)
        (handler-case
            (let ((file-config (with-open-file (stream file)
                                 (read stream))))
              (when (listp file-config)
                (merge-config file-config))
              (return))
          (error (e)
            (format t "Warning: Failed to load config from ~A: ~A~%" file e)))))))

(defun register-default-commands ()
  "Register default REPL commands"
  ;; This will be implemented by loading the commands module
  t)

(defun setup-signal-handlers ()
  "Setup signal handlers for graceful shutdown"
  ;; This would require a signal handling library
  ;; For now, just a placeholder
  nil)

(defun initialize-system ()
  "Initialize the Lantae system"
  ;; Load configuration from file if exists
  (load-configuration)
  
  ;; Initialize providers
  (when (find-package :lantae-providers)
    (funcall (intern "INITIALIZE-PROVIDERS" :lantae-providers)))
  
  ;; Register default commands
  (when (find-package :lantae-commands)
    (funcall (intern "REGISTER-ALL-COMMANDS" :lantae-commands)))
  
  ;; Setup signal handlers (if available)
  #+unix (setup-signal-handlers))

;;; REPL functions
(defun process-chat-message (message)
  "Process regular chat message"
  (push (list :user message) *conversation-history*)
  ;; TODO: Send to provider and get response
  (format t "Chat response would appear here for: ~A~%" message))

(defun process-slash-command (command)
  "Process slash command"
  (if (find-package :lantae-commands)
      (funcall (intern "EXECUTE-COMMAND" :lantae-commands) command)
      (format t "Commands not loaded~%")))

(defun process-repl-input (input)
  "Process REPL input - either LISP expression or chat message"
  (cond
    ;; LISP expression (starts with parenthesis)
    ((and (> (length input) 0) (char= (char input 0) #\())
     (handler-case
         (let ((expr (read-from-string input)))
           (eval expr))
       (error (e)
         (format t "Error evaluating expression: ~A~%" e))))
    
    ;; Slash command
    ((and (> (length input) 0) (char= (char input 0) #\/))
     (process-slash-command (subseq input 1)))
    
    ;; Regular chat message
    (t (process-chat-message input))))

(defun start-repl ()
  "Start the interactive REPL"
  (print-banner)
  
  (format t "Provider: ~A | Model: ~A~%" 
          (get-config :provider) (get-config :model))
  (format t "Type \"(help)\" for commands, \"(quit)\" to exit~%~%")
  
  (loop
    (format t "> ")
    (force-output)
    (let ((input (handler-case (read-line)
                   (end-of-file () "(quit)"))))
      (when (or (string= input "(quit)") (string= input "quit"))
        (format t "Goodbye!~%")
        (return))
      
      (handler-case
          (process-repl-input input)
        (error (e)
          (format t "Error: ~A~%" e))))))

(defun send-single-prompt (prompt)
  "Send a single prompt and exit"
  (format t "Processing prompt: ~A~%" prompt)
  ;; TODO: Implement provider interaction
  (format t "Response would appear here~%"))

;;; Main entry point
(defun main (&optional args)
  "Main entry point for Lantae LISP"
  (handler-case
      (multiple-value-bind (config-overrides prompt-args) (parse-arguments args)
        ;; Apply configuration overrides
        (when config-overrides
          (merge-config config-overrides))
        
        ;; Initialize system
        (initialize-system)
        
        ;; Execute based on arguments
        (if prompt-args
            (send-single-prompt (join-strings prompt-args))
            (start-repl)))
    (error (e)
      (format *error-output* "Fatal error: ~A~%" e)
      (quit 1))))

;;; LISP-specific utilities for S-expression configuration
(defun sexp-config-p (form)
  "Check if form is a valid S-expression configuration"
  (and (listp form)
       (evenp (length form))
       (every #'keywordp (loop for item in form by #'cddr collect item))))

(defmacro with-config (bindings &body body)
  "Temporarily bind configuration values"
  `(let ((*current-config* (copy-list *current-config*)))
     ,@(loop for (key value) in bindings
             collect `(set-config ,key ,value))
     ,@body))

;;; Export main function for command line usage
(export 'main)