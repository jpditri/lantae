;;;; commands.lisp - Command system using macros and higher-order functions
;;;;
;;;; Implements:
;;;; - Macro-based command definition
;;;; - Command registry and dispatch
;;;; - Tab completion support
;;;; - Command aliases
;;;; - Command history

(defpackage :lantae-commands
  (:use :cl)
  (:import-from :lantae-utils
                #:split-string
                #:trim-string
                #:parse-value
                #:quit
                #:join-strings)
  (:import-from :lantae-config
                #:get-config
                #:set-config)
  (:import-from :lantae-providers
                #:list-providers
                #:list-provider-models
                #:get-provider)
  (:export #:*command-registry*
           #:defcommand
           #:register-command
           #:execute-command
           #:list-commands
           #:complete-command
           #:command-help
           #:register-all-commands))

(in-package :lantae-commands)

;;; Command registry
(defvar *command-registry* (make-hash-table :test 'equal)
  "Registry of available commands")

(defvar *command-aliases* (make-hash-table :test 'equal)
  "Command aliases")

(defvar *command-history* '()
  "Command execution history")

;;; Command structure
(defstruct command
  name
  function
  help
  args-spec
  aliases)

;;; Command registration
(defun register-command (name function &key help args-spec aliases)
  "Register a command in the registry"
  (let ((cmd (make-command :name name
                          :function function
                          :help help
                          :args-spec args-spec
                          :aliases aliases)))
    (setf (gethash name *command-registry*) cmd)
    ;; Register aliases
    (dolist (alias aliases)
      (setf (gethash alias *command-aliases*) name)))
  (format t "Registered command: ~A~%" name))

(defmacro defcommand (name args &body body)
  "Define a new command"
  (let ((docstring (when (stringp (first body))
                     (first body)))
        (actual-body (if (stringp (first body))
                        (rest body)
                        body)))
    `(register-command ,(string-downcase (string name))
                      (lambda ,args
                        ,@actual-body)
                      :help ,docstring)))

;;; Command execution
(defun split-command-line (command-line)
  "Split command line into command and arguments"
  (let ((trimmed (trim-string command-line)))
    (if (string= trimmed "")
        '()
        (split-string trimmed #\Space))))

(defun find-command (name)
  "Find command by name or alias"
  (or (gethash name *command-registry*)
      (let ((real-name (gethash name *command-aliases*)))
        (when real-name
          (gethash real-name *command-registry*)))))

(defun execute-command (command-line)
  "Execute a command line"
  (let ((parts (split-command-line command-line)))
    (if (null parts)
        (format t "No command specified~%")
        (let* ((cmd-name (first parts))
               (args (rest parts))
               (cmd (find-command cmd-name)))
          (if cmd
              (handler-case
                  (progn
                    (push command-line *command-history*)
                    (apply (command-function cmd) args))
                (error (e)
                  (format t "Command error: ~A~%" e)))
              (format t "Unknown command: ~A~%" cmd-name))))))

;;; Command utilities
(defun list-commands ()
  "List all available commands"
  (let ((commands '()))
    (maphash (lambda (name cmd)
               (push (cons name cmd) commands))
             *command-registry*)
    (sort commands #'string< :key #'car)))

(defun complete-command-name (partial)
  "Complete command name"
  (let ((matches '()))
    (maphash (lambda (name cmd)
               (declare (ignore cmd))
               (when (and (>= (length name) (length partial))
                         (string= partial name :end2 (length partial)))
                 (push name matches)))
             *command-registry*)
    matches))

(defun complete-command (partial-line)
  "Complete command line"
  (let ((parts (split-command-line partial-line)))
    (cond
      ;; Complete command name
      ((= (length parts) 0)
       (list-commands))
      ((= (length parts) 1)
       (complete-command-name (first parts)))
      ;; Complete arguments (command-specific)
      (t nil))))

(defun command-help (command-name)
  "Get help for a command"
  (let ((cmd (find-command command-name)))
    (if cmd
        (or (command-help cmd) "No help available")
        "Unknown command")))

;;; Built-in commands
(defun register-all-commands ()
  "Register all built-in commands"
  
  (defcommand help ()
    "Show available commands"
    (format t "Available commands:~%~%")
    (dolist (cmd-pair (list-commands))
      (let* ((name (car cmd-pair))
             (cmd (cdr cmd-pair))
             (help (command-help cmd)))
        (format t "  ~A~:[~;~:*  - ~A~]~%" name help))))
  
  (defcommand provider (&rest args)
    "Switch provider or show current provider"
    (cond
      ;; No args - show current provider
      ((null args)
       (format t "Current provider: ~A~%" (get-config :provider))
       (format t "Current model: ~A~%" (get-config :model)))
      ;; One arg - switch provider
      ((= (length args) 1)
       (let ((provider-name (first args)))
         (if (member provider-name (list-providers) :test #'string=)
             (progn
               (set-config :provider provider-name)
               (format t "Switched to provider: ~A~%" provider-name))
             (format t "Unknown provider: ~A~%" provider-name))))
      ;; Two args - switch provider and model
      ((= (length args) 2)
       (let ((provider-name (first args))
             (model-name (second args)))
         (if (member provider-name (list-providers) :test #'string=)
             (progn
               (set-config :provider provider-name)
               (set-config :model model-name)
               (format t "Switched to ~A with model ~A~%" provider-name model-name))
             (format t "Unknown provider: ~A~%" provider-name))))
      (t (format t "Usage: provider [name] [model]~%"))))
  
  (defcommand model (&rest args)
    "Switch to a different model"
    (if args
        (let ((model-name (join-strings args)))
          (set-config :model model-name)
          (format t "Switched to model: ~A~%" model-name))
        (format t "Current model: ~A~%" (get-config :model))))
  
  (defcommand config (&optional key value)
    "Show or set configuration values"
    (cond
      ;; No args - show all config
      ((null key)
       (format t "Current configuration:~%")
       (lantae-config:with-config-binding nil
         (let ((config (lantae-config:config-to-plist)))
           (loop for (k v) on config by #'cddr
                 do (format t "  ~A: ~A~%" k v)))))
      ;; One arg - show specific config
      ((null value)
       (format t "~A = ~A~%" key (get-config key)))
      ;; Two args - set config
      (t
       (set-config key (parse-value value))
       (format t "Set ~A = ~A~%" key value))))
  
  (defcommand clear ()
    "Clear conversation history"
    ;; Clear conversation history in main package
    (when (find-package :lantae)
      (setf (symbol-value (intern "*CONVERSATION-HISTORY*" :lantae)) '()))
    (format t "Conversation history cleared.~%"))
  
  (defcommand info ()
    "Show system information"
    (format t "Lantae LISP v~A~%" 
            (if (find-package :lantae)
                (symbol-value (intern "*VERSION*" :lantae))
                "1.0.0-lisp"))
    (format t "Provider: ~A~%" (get-config :provider))
    (format t "Model: ~A~%" (get-config :model))
    (format t "Available providers: ~{~A~^, ~}~%" (list-providers))
    (format t "LISP implementation: ~A ~A~%" 
            (lisp-implementation-type) 
            (lisp-implementation-version)))
  
  (defcommand quit ()
    "Exit the REPL"
    (format t "Goodbye!~%")
    (quit)))


(defun show-provider-info (provider-name)
  "Show information about a provider"
  (let ((provider (get-provider provider-name)))
    (if provider
        (progn
          (format t "Provider: ~A~%" provider-name)
          (let ((models (list-provider-models provider-name)))
            (when models
              (format t "Available models:~%")
              (dolist (model models)
                (format t "  - ~A~%" model)))))
        (format t "Provider ~A not found~%" provider-name))))

;;; Tab completion support
(defun setup-readline-completion ()
  "Setup readline-style completion (if available)"
  ;; This would integrate with readline library
  ;; For now, just a placeholder
  nil)

;;; Command history
(defun add-to-history (command)
  "Add command to history"
  (push command *command-history*)
  ;; Limit history size
  (when (> (length *command-history*) 100)
    (setf *command-history* (subseq *command-history* 0 100))))

(defun get-history-item (n)
  "Get nth item from history"
  (when (and (>= n 0) (< n (length *command-history*)))
    (nth n *command-history*)))

(defun search-history (pattern)
  "Search command history"
  (remove-if-not (lambda (cmd) (search pattern cmd :test #'string-equal))
                 *command-history*))

;;; Command macros for advanced usage
(defmacro with-command-context (&body body)
  "Execute body with command context"
  `(let ((*print-pretty* t)
         (*print-case* :downcase))
     ,@body))

(defmacro define-command-alias (alias command)
  "Define a command alias"
  `(setf (gethash ,(string-downcase (string alias)) *command-aliases*)
         ,(string-downcase (string command))))

;;; Initialize default aliases
(defun setup-default-aliases ()
  "Setup default command aliases"
  (define-command-alias h help)
  (define-command-alias p provider)
  (define-command-alias m model)
  (define-command-alias c config)
  (define-command-alias q quit)
  (define-command-alias exit quit))

;; Setup aliases when loaded
(setup-default-aliases)