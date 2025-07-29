;;;; function-calling.lisp - Function calling integration for providers
;;;;
;;;; This module provides:
;;;; - Tool definition and registration
;;;; - Function calling execution
;;;; - Provider-agnostic tool management
;;;; - Tool result formatting

(defpackage :lantae-function-calling
  (:use :cl :lantae-providers)
  (:export #:*tool-registry*
           #:register-tool
           #:get-tool
           #:list-tools
           #:execute-tool
           #:format-tool-for-provider
           #:handle-function-calls
           #:create-tool-definition
           #:tool-definition
           #:tool-definition-name
           #:tool-definition-description
           #:tool-definition-parameters
           #:tool-definition-function))

(in-package :lantae-function-calling)

;;; Tool registry
(defvar *tool-registry* (make-hash-table :test 'equal)
  "Registry of available tools")

;;; Tool definition structure
(defstruct tool-definition
  "Tool definition with metadata and execution function"
  name
  description
  parameters
  function)

;;; Tool registration
(defun register-tool (tool-definition)
  "Register a tool in the global registry"
  (setf (gethash (tool-definition-name tool-definition) *tool-registry*) 
        tool-definition))

(defun get-tool (name)
  "Get tool by name from registry"
  (gethash name *tool-registry*))

(defun list-tools ()
  "List all registered tool names"
  (loop for name being the hash-keys of *tool-registry*
        collect name))

;;; Tool execution
(defun execute-tool (tool-name arguments)
  "Execute a tool with given arguments"
  (let ((tool (get-tool tool-name)))
    (if tool
        (handler-case
            (let ((args (if (stringp arguments)
                           (decode-json arguments)
                           arguments)))
              (funcall (tool-definition-function tool) args))
          (error (e)
            (format nil "Tool execution error: ~A" e)))
        (format nil "Tool '~A' not found" tool-name))))

;;; Provider-specific formatting
(defun format-tool-for-provider (provider-name tool-definition)
  "Format tool definition for specific provider"
  (case (intern (string-upcase provider-name) :keyword)
    (:openai (format-tool-openai tool-definition))
    (:anthropic (format-tool-anthropic tool-definition))
    (t (format-tool-generic tool-definition))))

(defun format-tool-openai (tool-def)
  "Format tool for OpenAI API"
  `((:type . "function")
    (:function . ((:name . ,(tool-definition-name tool-def))
                  (:description . ,(tool-definition-description tool-def))
                  (:parameters . ,(tool-definition-parameters tool-def))))))

(defun format-tool-anthropic (tool-def)
  "Format tool for Anthropic API"
  `((:name . ,(tool-definition-name tool-def))
    (:description . ,(tool-definition-description tool-def))
    (:input_schema . ,(tool-definition-parameters tool-def))))

(defun format-tool-generic (tool-def)
  "Generic tool format"
  `((:name . ,(tool-definition-name tool-def))
    (:description . ,(tool-definition-description tool-def))
    (:parameters . ,(tool-definition-parameters tool-def))))

;;; Function calling workflow
(defun handle-function-calls (provider-name response)
  "Handle function calls in provider response"
  (let ((tool-calls (extract-tool-calls provider-name response)))
    (when tool-calls
      (execute-tool-calls tool-calls))))

(defun extract-tool-calls (provider-name response)
  "Extract tool calls from provider response"
  (case (intern (string-upcase provider-name) :keyword)
    (:openai (getf response :tool_calls))
    (:anthropic (getf response :tool_calls))
    (t nil)))

(defun execute-tool-calls (tool-calls)
  "Execute multiple tool calls and return results"
  (mapcar #'execute-single-tool-call tool-calls))

(defun execute-single-tool-call (tool-call)
  "Execute a single tool call"
  (let* ((function-info (cdr (assoc :function tool-call)))
         (function-name (cdr (assoc :name function-info)))
         (function-args (cdr (assoc :arguments function-info)))
         (tool-id (cdr (assoc :id tool-call))))
    
    (let ((result (execute-tool function-name function-args)))
      `((:id . ,tool-id)
        (:name . ,function-name)
        (:result . ,result)))))

;;; Built-in tools
(defun register-builtin-tools ()
  "Register built-in tools"
  ;; System information tool
  (register-tool 
   (make-tool-definition
    :name "get_system_info"
    :description "Get system information including OS, platform, and environment"
    :parameters '((:type . "object")
                  (:properties . ((:info_type . ((:type . "string")
                                                 (:enum . ("os" "platform" "env" "all"))
                                                 (:description . "Type of system information to retrieve")))))
                  (:required . ("info_type")))
    :function (lambda (args)
                (let ((info-type (cdr (assoc :info_type args))))
                  (case (intern (string-upcase info-type) :keyword)
                    (:os #+sbcl (software-type) #-sbcl "Unknown")
                    (:platform #+sbcl (machine-type) #-sbcl "Unknown")
                    (:env (format nil "LISP: ~A ~A" 
                                 (lisp-implementation-type)
                                 (lisp-implementation-version)))
                    (:all (format nil "OS: ~A, Platform: ~A, LISP: ~A ~A"
                                 #+sbcl (software-type) #-sbcl "Unknown"
                                 #+sbcl (machine-type) #-sbcl "Unknown"
                                 (lisp-implementation-type)
                                 (lisp-implementation-version)))
                    (t "Invalid info type"))))))
  
  ;; Current time tool
  (register-tool
   (make-tool-definition
    :name "get_current_time"
    :description "Get the current date and time"
    :parameters '((:type . "object")
                  (:properties . ((:format . ((:type . "string")
                                              (:enum . ("iso" "unix" "readable"))
                                              (:description . "Time format to return")))))
                  (:required . ()))
    :function (lambda (args)
                (let* ((format-type (or (cdr (assoc :format args)) "readable"))
                       (now (get-universal-time)))
                  (case (intern (string-upcase format-type) :keyword)
                    (:unix (format nil "~A" now))
                    (:iso (multiple-value-bind (sec min hour date month year)
                              (decode-universal-time now)
                            (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
                                   year month date hour min sec)))
                    (:readable (multiple-value-bind (sec min hour date month year day)
                                   (decode-universal-time now)
                                 (declare (ignore sec))
                                 (format nil "~A, ~A ~D, ~D at ~2,'0D:~2,'0D"
                                        (nth day '("Monday" "Tuesday" "Wednesday" "Thursday" 
                                                  "Friday" "Saturday" "Sunday"))
                                        (nth (1- month) '("January" "February" "March" "April"
                                                         "May" "June" "July" "August" "September"
                                                         "October" "November" "December"))
                                        date year hour min)))
                    (t "Invalid format type"))))))
  
  ;; Math calculator tool
  (register-tool
   (make-tool-definition
    :name "calculate"
    :description "Perform basic mathematical calculations"
    :parameters '((:type . "object")
                  (:properties . ((:expression . ((:type . "string")
                                                  (:description . "Mathematical expression to evaluate")))
                                 (:operation . ((:type . "string")
                                               (:enum . ("add" "subtract" "multiply" "divide" "power"))
                                               (:description . "Basic operation type")))
                                 (:a . ((:type . "number")
                                       (:description . "First number")))
                                 (:b . ((:type . "number")
                                       (:description . "Second number")))))
                  (:required . ()))
    :function (lambda (args)
                (handler-case
                    (cond
                      ;; Simple expression evaluation would go here
                      ;; For safety, we'll just handle basic operations
                      ((cdr (assoc :operation args))
                       (let ((op (intern (string-upcase (cdr (assoc :operation args))) :keyword))
                             (a (cdr (assoc :a args)))
                             (b (cdr (assoc :b args))))
                         (case op
                           (:add (format nil "~A" (+ a b)))
                           (:subtract (format nil "~A" (- a b)))
                           (:multiply (format nil "~A" (* a b)))
                           (:divide (if (zerop b) 
                                       "Error: Division by zero"
                                       (format nil "~A" (/ a b))))
                           (:power (format nil "~A" (expt a b)))
                           (t "Unknown operation"))))
                      (t "Please specify operation and numbers"))
                  (error (e)
                    (format nil "Calculation error: ~A" e))))))
  
  (format t "✓ Built-in tools registered: ~{~A~^, ~}~%" (list-tools)))

;;; Initialize tools
(register-builtin-tools)