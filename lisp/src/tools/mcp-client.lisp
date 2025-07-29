;;;; mcp-client.lisp - Model Context Protocol client for Lantae LISP
;;;;
;;;; Implements MCP client for connecting to external tool servers
;;;; Supports JSON-RPC 2.0 over stdio transport

(defpackage :lantae-mcp
  (:use :cl)
  (:export #:mcp-client
           #:make-mcp-client
           #:connect-mcp-client
           #:disconnect-mcp-client
           #:call-mcp-tool
           #:list-mcp-tools
           #:mcp-client-connected-p
           #:register-mcp-tools-with-manager
           #:load-mcp-servers-config
           #:*mcp-clients*))

(in-package :lantae-mcp)

;;; MCP Client structure
(defclass mcp-client ()
  ((name :initarg :name
         :accessor client-name
         :documentation "Name of the MCP server")
   (command :initarg :command
            :accessor client-command
            :documentation "Command to start the server")
   (args :initarg :args
         :accessor client-args
         :initform '()
         :documentation "Arguments for the server command")
   (description :initarg :description
                :accessor client-description
                :initform ""
                :documentation "Description of the server")
   (enabled :initarg :enabled
            :accessor client-enabled
            :initform t
            :documentation "Whether this client is enabled")
   (process :initform nil
            :accessor client-process
            :documentation "External process handle")
   (input-stream :initform nil
                 :accessor client-input-stream
                 :documentation "Input stream to server")
   (output-stream :initform nil
                  :accessor client-output-stream
                  :documentation "Output stream from server")
   (connected :initform nil
            :accessor client-connected
            :documentation "Connection status")
   (tools :initform (make-hash-table :test 'equal)
          :accessor client-tools
          :documentation "Available tools from this server")
   (request-id :initform 0
               :accessor client-request-id
               :documentation "Current request ID counter")))

(defun make-mcp-client (&key name command args description (enabled t))
  "Create a new MCP client"
  (make-instance 'mcp-client
                 :name name
                 :command command
                 :args args
                 :description description
                 :enabled enabled))

;;; Global registry of MCP clients
(defvar *mcp-clients* '()
  "List of active MCP clients")

;;; JSON-RPC utilities
(defun next-request-id (client)
  "Get next request ID"
  (incf (client-request-id client)))

(defun make-json-rpc-request (method &key params id)
  "Create JSON-RPC 2.0 request"
  (let ((request `(("jsonrpc" . "2.0")
                   ("method" . ,method))))
    (when id
      (push `("id" . ,id) request))
    (when params
      (push `("params" . ,params) request))
    request))

(defun encode-json-message (data)
  "Encode message as JSON for MCP transport"
  (cl-json:encode-json-to-string data))

(defun decode-json-message (json-string)
  "Decode JSON message from MCP transport"
  (handler-case
      (cl-json:decode-json-from-string json-string)
    (error (e)
      (format t "JSON decode error: ~A~%" e)
      nil)))

;;; MCP Client connection management
(defmethod connect-mcp-client ((client mcp-client))
  "Connect to MCP server"
  (when (client-enabled client)
    (handler-case
        (progn
          (format t "Connecting to MCP server: ~A~%" (client-name client))
          
          ;; Start external process
          #+sbcl
          (let ((process (sb-ext:run-program 
                         (client-command client)
                         (client-args client)
                         :input :stream
                         :output :stream
                         :error :stream
                         :wait nil)))
            (setf (client-process client) process)
            (setf (client-input-stream client) (sb-ext:process-input process))
            (setf (client-output-stream client) (sb-ext:process-output process)))
          
          #-sbcl
          (error "MCP client not supported on this Lisp implementation")
          
          ;; Initialize connection
          (initialize-mcp-connection client)
          
          ;; Discover tools
          (discover-mcp-tools client)
          
          (setf (client-connected client) t)
          (pushnew client *mcp-clients*)
          (format t "✓ Connected to MCP server: ~A~%" (client-name client))
          t)
      (error (e)
        (format t "✗ Failed to connect to MCP server ~A: ~A~%" 
                (client-name client) e)
        nil))))

(defmethod disconnect-mcp-client ((client mcp-client))
  "Disconnect from MCP server"
  (when (client-connected client)
    (handler-case
        (progn
          ;; Close streams
          (when (client-input-stream client)
            (close (client-input-stream client)))
          (when (client-output-stream client)
            (close (client-output-stream client)))
          
          ;; Terminate process
          #+sbcl
          (when (client-process client)
            (sb-ext:process-kill (client-process client) sb-unix:sigterm))
          
          (setf (client-connected client) nil)
          (setf *mcp-clients* (remove client *mcp-clients*))
          (format t "Disconnected from MCP server: ~A~%" (client-name client)))
      (error (e)
        (format t "Error disconnecting from ~A: ~A~%" (client-name client) e)))))

(defmethod mcp-client-connected-p ((client mcp-client))
  "Check if client is connected"
  (client-connected client))

;;; MCP Protocol implementation
(defmethod initialize-mcp-connection ((client mcp-client))
  "Initialize MCP connection with handshake"
  (let* ((init-request (make-json-rpc-request 
                       "initialize"
                       :params `(("protocolVersion" . "2024-11-05")
                               ("capabilities" . (("tools" . t))))
                       :id (next-request-id client)))
         (response (send-mcp-request client init-request)))
    
    (if (and response (not (assoc "error" response)))
        (format t "MCP initialization successful~%")
        (error "MCP initialization failed: ~A" response))))

(defmethod discover-mcp-tools ((client mcp-client))
  "Discover available tools from MCP server"
  (let* ((tools-request (make-json-rpc-request 
                        "tools/list"
                        :id (next-request-id client)))
         (response (send-mcp-request client tools-request)))
    
    (when (and response (not (assoc "error" response)))
      (let ((tools (cdr (assoc "result" response))))
        (when (assoc "tools" tools)
          (dolist (tool-info (cdr (assoc "tools" tools)))
            (let ((tool-name (cdr (assoc "name" tool-info)))
                  (tool-desc (cdr (assoc "description" tool-info))))
              (setf (gethash tool-name (client-tools client)) tool-info)
              (format t "Discovered tool: ~A - ~A~%" tool-name tool-desc))))))))

(defmethod send-mcp-request ((client mcp-client) request)
  "Send JSON-RPC request to MCP server and wait for response"
  (handler-case
      (let* ((json-message (encode-json-message request))
             (input-stream (client-input-stream client))
             (output-stream (client-output-stream client)))
        
        ;; Send request
        (write-line json-message input-stream)
        (force-output input-stream)
        
        ;; Read response
        (let ((response-line (read-line output-stream nil nil)))
          (when response-line
            (decode-json-message response-line))))
    (error (e)
      (format t "MCP request error: ~A~%" e)
      nil)))

;;; Tool execution
(defmethod call-mcp-tool ((client mcp-client) tool-name arguments)
  "Call a tool on the MCP server"
  (if (gethash tool-name (client-tools client))
      (let* ((tool-request (make-json-rpc-request
                           "tools/call"
                           :params `(("name" . ,tool-name)
                                   ("arguments" . ,arguments))
                           :id (next-request-id client)))
             (response (send-mcp-request client tool-request)))
        
        (if (and response (not (assoc "error" response)))
            (let* ((result (cdr (assoc "result" response)))
                   (content (cdr (assoc "content" result))))
              (if content
                  (lantae-tools:tool-success content :tool-name tool-name)
                  (lantae-tools:tool-failure "No content in response" :tool-name tool-name)))
            (lantae-tools:tool-failure 
             (format nil "MCP call failed: ~A" 
                    (cdr (assoc "error" response)))
             :tool-name tool-name)))
      (lantae-tools:tool-failure 
       (format nil "Tool ~A not found on server ~A" tool-name (client-name client))
       :tool-name tool-name)))

(defmethod list-mcp-tools ((client mcp-client))
  "List all tools available from this MCP client"
  (loop for tool-name being the hash-keys of (client-tools client)
        collect tool-name))

;;; Integration with tool manager
(defun register-mcp-tools-with-manager (client tool-manager)
  "Register all MCP tools from client with the tool manager"
  (loop for tool-name being the hash-keys of (client-tools client)
        for tool-info being the hash-values of (client-tools client)
        do (let ((description (cdr (assoc "description" tool-info))))
             (lantae-tools:register-tool 
              tool-manager
              tool-name
              (lambda (&rest args)
                (let ((result (call-mcp-tool client tool-name args)))
                  (if (lantae-tools:tool-result-success-p result)
                      (lantae-tools:tool-result-value result)
                      (error (lantae-tools:tool-result-error result)))))
              :description description))))

;;; Configuration loading
(defun load-mcp-servers-config (&optional (config-file "mcp_servers.yml"))
  "Load MCP server configuration from YAML file"
  ;; This is a placeholder - would need a YAML parser
  ;; For now, return some default configurations
  (declare (ignore config-file))
  (list
   (make-mcp-client :name "filesystem"
                    :command "npx"
                    :args '("@modelcontextprotocol/server-filesystem" ".")
                    :description "File system operations via MCP")
   (make-mcp-client :name "sqlite"
                    :command "npx"
                    :args '("@modelcontextprotocol/server-sqlite" "--db-path" "./data.db")
                    :description "SQLite database operations")))

;;; Convenience functions
(defun connect-all-mcp-clients ()
  "Connect to all configured MCP clients"
  (let ((clients (load-mcp-servers-config)))
    (dolist (client clients)
      (when (client-enabled client)
        (connect-mcp-client client)))
    (length *mcp-clients*)))

(defun disconnect-all-mcp-clients ()
  "Disconnect from all MCP clients"
  (dolist (client (copy-list *mcp-clients*))
    (disconnect-mcp-client client)))

(defun mcp-tool-exists-p (tool-name)
  "Check if any MCP client provides the given tool"
  (some (lambda (client)
          (gethash tool-name (client-tools client)))
        *mcp-clients*))

(defun find-mcp-client-for-tool (tool-name)
  "Find MCP client that provides the given tool"
  (find-if (lambda (client)
             (gethash tool-name (client-tools client)))
           *mcp-clients*))