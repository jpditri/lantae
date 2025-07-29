;;;; tools.lisp - Tool management system for Lantae LISP
;;;;
;;;; Implements tool registration, execution, and integration with providers
;;;; Supports both simple function-based tools and MCP client tools

(defpackage :lantae-tools
  (:use :cl)
  (:export #:tool-manager
           #:make-tool-manager
           #:register-tool
           #:execute-tool
           #:list-tools
           #:has-tool-p
           #:tool-result
           #:tool-result-success-p
           #:tool-result-value
           #:tool-result-error
           #:tool-success
           #:tool-failure
           #:format-tool-output
           #:validate-tool-arguments
           #:*default-tool-manager*
           #:tool-execute
           #:tool-register
           #:tool-list
           #:tool-exists-p))

(in-package :lantae-tools)

;;; Tool result type
(defstruct tool-result
  success-p
  value
  error
  tool-name
  execution-time)

(defun tool-success (value &key tool-name execution-time)
  "Create successful tool result"
  (make-tool-result :success-p t 
                    :value value 
                    :tool-name tool-name
                    :execution-time execution-time))

(defun tool-failure (error &key tool-name execution-time)
  "Create failed tool result"
  (make-tool-result :success-p nil 
                    :error error
                    :tool-name tool-name
                    :execution-time execution-time))

;;; Tool structure
(defstruct tool
  name
  function
  description
  parameters
  validate-fn)

;;; Tool Manager
(defclass tool-manager ()
  ((tools :initform (make-hash-table :test 'equal)
          :accessor tools
          :documentation "Hash table of registered tools")
   (mcp-clients :initform '()
                :accessor mcp-clients
                :documentation "List of MCP clients")
   (security-enabled :initform t
                     :accessor security-enabled
                     :documentation "Enable security checks for tool execution")))

(defun make-tool-manager (&key (security-enabled t))
  "Create a new tool manager instance"
  (let ((manager (make-instance 'tool-manager)))
    (setf (security-enabled manager) security-enabled)
    manager))

;;; Global tool manager
(defvar *default-tool-manager* (make-tool-manager)
  "Default global tool manager instance")

;;; Tool registration
(defmethod register-tool ((tm tool-manager) name function &key description parameters validate-fn)
  "Register a tool with the tool manager"
  (let ((tool-obj (make-tool :name name
                             :function function
                             :description description
                             :parameters parameters
                             :validate-fn validate-fn)))
    (setf (gethash name (tools tm)) tool-obj)
    (format t "Registered tool: ~A~%" name)
    t))

(defmethod has-tool-p ((tm tool-manager) name)
  "Check if tool exists"
  (multiple-value-bind (tool exists-p) (gethash name (tools tm))
    (declare (ignore tool))
    exists-p))

(defmethod list-tools ((tm tool-manager))
  "List all registered tools"
  (loop for name being the hash-keys of (tools tm)
        collect name))

;;; Tool execution
(defmethod execute-tool ((tm tool-manager) name &rest arguments)
  "Execute a tool with given arguments"
  (let ((start-time (get-internal-real-time))
        (tool (gethash name (tools tm))))
    
    (if tool
        (handler-case
            (progn
              ;; Validate arguments if validator exists
              (when (and (security-enabled tm) (tool-validate-fn tool))
                (unless (apply (tool-validate-fn tool) arguments)
                  (error "Tool argument validation failed")))
              
              ;; Execute tool
              (let* ((result (apply (tool-function tool) arguments))
                     (end-time (get-internal-real-time))
                     (execution-time (/ (- end-time start-time) 
                                       internal-time-units-per-second)))
                (tool-success result 
                             :tool-name name 
                             :execution-time execution-time)))
          (error (e)
            (let* ((end-time (get-internal-real-time))
                   (execution-time (/ (- end-time start-time) 
                                     internal-time-units-per-second)))
              (tool-failure (format nil "~A" e)
                           :tool-name name
                           :execution-time execution-time))))
        (tool-failure (format nil "Tool not found: ~A" name)
                     :tool-name name))))

;;; Tool output formatting
(defun format-tool-output (result &key (max-width 80))
  "Format tool execution result for display"
  (let* ((success (tool-result-success-p result))
         (tool-name (tool-result-tool-name result))
         (content (if success 
                     (format nil "~A" (tool-result-value result))
                     (tool-result-error result)))
         (execution-time (tool-result-execution-time result))
         (status-text (if success "✓ Success" "✗ Failed"))
         (lines (split-lines content))
         (wrapped-lines (mapcan (lambda (line) (wrap-text line (- max-width 4))) lines)))
    
    (with-output-to-string (stream)
      ;; Header
      (if (find-package :lantae-colors)
          (funcall (intern "PRINT-COLORED" :lantae-colors)
                   (format nil "~%╭─ Tool: ~A ~A~@[ (~,3Fs)~] ─╮~%" 
                           tool-name status-text execution-time)
                   (if success :green :red)
                   stream)
          (format stream "~%╭─ Tool: ~A ~A~@[ (~,3Fs)~] ─╮~%" 
                  tool-name status-text execution-time))
      
      ;; Content
      (dolist (line wrapped-lines)
        (let ((padding (max 0 (- max-width (length line) 4))))
          (if (find-package :lantae-colors)
              (funcall (intern "PRINT-COLORED" :lantae-colors)
                       (format nil "│ ~A~A │~%" line (make-string padding :initial-element #\Space))
                       (if success :green :red)
                       stream)
              (format stream "│ ~A~A │~%" line (make-string padding :initial-element #\Space)))))
      
      ;; Footer
      (if (find-package :lantae-colors)
          (funcall (intern "PRINT-COLORED" :lantae-colors) "╰─╯~%" 
                   (if success :green :red) stream)
          (format stream "╰─╯~%")))))

;;; Utility functions
(defun split-lines (text)
  "Split text into lines"
  (loop for start = 0 then (1+ pos)
        for pos = (position #\Newline text :start start)
        collect (subseq text start pos)
        while pos))

(defun wrap-text (text max-width)
  "Wrap text to fit within max-width"
  (if (<= (length text) max-width)
      (list text)
      (let ((break-pos (or (position #\Space text :from-end t :end max-width)
                          max-width)))
        (cons (subseq text 0 break-pos)
              (wrap-text (string-left-trim " " (subseq text break-pos)) max-width)))))

;;; Argument validation
(defun validate-tool-arguments (parameters arguments)
  "Basic argument validation"
  (when parameters
    (let ((required (getf parameters :required))
          (types (getf parameters :types)))
      
      ;; Check required arguments
      (when (and required (< (length arguments) (length required)))
        (error "Missing required arguments: ~{~A~^, ~}" 
               (subseq required (length arguments))))
      
      ;; Check argument types
      (when types
        (loop for arg in arguments
              for expected-type in types
              unless (typep arg expected-type)
              do (error "Argument type mismatch: expected ~A, got ~A" 
                       expected-type (type-of arg))))
      
      t)))

;;; Built-in tools
(defun register-built-in-tools (&optional (tm *default-tool-manager*))
  "Register basic built-in tools"
  
  ;; Echo tool
  (register-tool tm "echo" 
                 (lambda (&rest args) 
                   (format nil "~{~A~^ ~}" args))
                 :description "Echo arguments back as a string"
                 :parameters '(:required ("message")))
  
  ;; Current time tool
  (register-tool tm "current-time"
                 (lambda () 
                   (multiple-value-bind (sec min hour date month year)
                       (get-decoded-time)
                     (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                             year month date hour min sec)))
                 :description "Get current date and time")
  
  ;; Math tools
  (register-tool tm "add"
                 (lambda (a b) (+ a b))
                 :description "Add two numbers"
                 :parameters '(:required ("a" "b") :types (number number)))
  
  (register-tool tm "multiply"
                 (lambda (a b) (* a b))
                 :description "Multiply two numbers"
                 :parameters '(:required ("a" "b") :types (number number)))
  
  ;; System info tool
  (register-tool tm "system-info"
                 (lambda ()
                   (format nil "OS: ~A~%Lisp: ~A ~A~%"
                           #+unix "Unix" #+windows "Windows" #-(or unix windows) "Unknown"
                           (lisp-implementation-type)
                           (lisp-implementation-version)))
                 :description "Get system information")
  
  (format t "Registered ~A built-in tools~%" (hash-table-count (tools tm))))

;;; Tool manager convenience functions
(defun tool-execute (name &rest arguments)
  "Execute tool using default tool manager"
  (apply #'execute-tool *default-tool-manager* name arguments))

(defun tool-register (name function &key description parameters validate-fn)
  "Register tool using default tool manager"
  (register-tool *default-tool-manager* name function
                 :description description
                 :parameters parameters
                 :validate-fn validate-fn))

(defun tool-list ()
  "List tools using default tool manager"
  (list-tools *default-tool-manager*))

(defun tool-exists-p (name)
  "Check if tool exists using default tool manager"
  (has-tool-p *default-tool-manager* name))