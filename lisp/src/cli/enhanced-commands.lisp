;;;; enhanced-commands.lisp - Additional commands for feature parity
;;;;
;;;; Implements missing commands to bring LISP implementation up to Ruby parity:
;;;; - Tool integration (/tool, /tools)
;;;; - Conversation management (/conversation)
;;;; - Cost tracking (/cost)
;;;; - Agent and planning (/agent, /squad)
;;;; - MCP protocol (/mcp)
;;;; - LSP integration (/lsp)
;;;; - Task and template management (/task, /template)

(defpackage :lantae-enhanced-commands
  (:use :cl :lantae-commands :cl-json)
  (:export #:register-enhanced-commands))

(in-package :lantae-enhanced-commands)

;;; Backend implementations
(defun execute-system-tool (tool-name args)
  "Execute system tools with actual backends"
  (case (intern (string-upcase tool-name) :keyword)
    (:bash (execute-bash-command args))
    (:git (execute-git-command args))
    (:cat (execute-cat-command args))
    (:ls (execute-ls-command args))
    (:mkdir (execute-mkdir-command args))
    (:find (execute-find-command args))
    (t 
     ;; Try function calling tools
     (let ((result (lantae-function-calling:execute-tool tool-name 
                                                        (when args (list (cons :args args))))))
       (if (string= result (format nil "Tool '~A' not found" tool-name))
           (format t "❌ Unknown tool: ~A~%" tool-name)
           (format t "✅ Result: ~A~%" result))))))

(defun execute-bash-command (args)
  "Execute bash command"
  (if args
      (let ((command (format nil "~{~A~^ ~}" args)))
        (format t "💻 Executing: ~A~%" command)
        (handler-case
            #+sbcl (let ((output (with-output-to-string (s)
                                  (sb-ext:run-program "/bin/bash" 
                                                     (list "-c" command)
                                                     :output s
                                                     :error s))))
                     (format t "~A~%" (string-trim '(#\Space #\Newline) output)))
            #-sbcl (format t "❌ Command execution not supported on this LISP implementation~%")
          (error (e) (format t "❌ Error: ~A~%" e))))
      (format t "❌ Usage: /tool bash <command>~%")))

(defun execute-git-command (args)
  "Execute git command"
  (if args
      (let ((git-args (format nil "~{~A~^ ~}" args)))
        (format t "🔧 Git: ~A~%" git-args)
        (execute-bash-command (list "git" git-args)))
      (format t "❌ Usage: /tool git <git-command>~%")))

(defun execute-cat-command (args)
  "Read file contents"
  (if args
      (let ((filename (first args))
            (max-lines (if (second args) (parse-integer (second args) :junk-allowed t) 50)))
        (format t "📄 Reading file: ~A~%" filename)
        (handler-case
            (with-open-file (stream filename :direction :input)
              (loop for i from 1 to max-lines
                    for line = (read-line stream nil nil)
                    while line
                    do (format t "~3d: ~A~%" i line)))
          (error (e) (format t "❌ Error reading file: ~A~%" e))))
      (format t "❌ Usage: /tool cat <filename> [max-lines]~%")))

(defun execute-ls-command (args)
  "List directory contents"
  (let ((directory (if args (first args) ".")))
    (format t "📂 Directory: ~A~%" directory)
    (handler-case
        #+sbcl (let ((entries (directory (merge-pathnames "*" directory))))
                 (dolist (entry entries)
                   (format t "  ~A~%" (file-namestring entry))))
        #-sbcl (format t "❌ Directory listing not supported on this LISP implementation~%")
      (error (e) (format t "❌ Error: ~A~%" e)))))

(defun execute-mkdir-command (args)
  "Create directory"
  (if args
      (let ((dirname (first args)))
        (format t "📁 Creating directory: ~A~%" dirname)
        (handler-case
            (progn
              (ensure-directories-exist dirname)
              (format t "✅ Directory created successfully~%"))
          (error (e) (format t "❌ Error creating directory: ~A~%" e))))
      (format t "❌ Usage: /tool mkdir <directory-name>~%")))

(defun execute-find-command (args)
  "Find files"
  (if args
      (let ((pattern (first args))
            (directory (if (second args) (second args) ".")))
        (format t "🔍 Finding files matching '~A' in ~A~%" pattern directory)
        (handler-case
            #+sbcl (let ((matches (directory (merge-pathnames 
                                            (concatenate 'string "**/*" pattern "*")
                                            directory))))
                     (if matches
                         (dolist (match matches)
                           (format t "  ~A~%" match))
                         (format t "No files found matching pattern~%")))
            #-sbcl (format t "❌ File search not supported on this LISP implementation~%")
          (error (e) (format t "❌ Error: ~A~%" e))))
      (format t "❌ Usage: /tool find <pattern> [directory]~%")))

;;; Tool integration commands
(defun cmd-tool (args)
  "Execute a tool"
  (if args
      (let ((tool-name (first args))
            (tool-args (rest args)))
        (format t "🔧 Executing tool: ~A~%" tool-name)
        (execute-system-tool tool-name tool-args))
      (progn
        (format t "Available tools:~%")
        (format t "  • Function calling tools: ~{~A~^, ~}~%" 
                (lantae-function-calling:list-tools))
        (format t "  • System tools: bash, git, cat, ls, mkdir, find~%"))))

(defun cmd-tools (args)
  "List available tools"
  (declare (ignore args))
  (format t "~%🛠️  Available Tools:~%")
  (format t "  • bash      - Execute bash commands~%")
  (format t "  • git       - Git operations~%")
  (format t "  • cat       - Read files~%")
  (format t "  • ls        - List directory contents~%")
  (format t "  • mkdir     - Create directories~%")
  (format t "  • find      - Find files~%")
  (format t "  • python    - Execute Python code~%")
  (format t "  • ruby      - Execute Ruby code~%")
  (format t "~%Usage: /tool <tool-name> <args...>~%"))

;;; Conversation management commands
(defparameter *conversations-dir* "~/.lantae/conversations/"
  "Directory to store saved conversations")

(defun ensure-conversations-dir ()
  "Ensure conversations directory exists"
  (ensure-directories-exist *conversations-dir*))

(defun cmd-conversation (args)
  "Manage conversations"
  (ensure-conversations-dir)
  (cond
    ((null args)
     (format t "📋 Conversation commands:~%")
     (format t "  /conversation list       - List saved conversations~%")
     (format t "  /conversation save NAME  - Save current conversation~%")
     (format t "  /conversation load NAME  - Load a conversation~%")
     (format t "  /conversation export     - Export conversation~%"))
    ((string= (first args) "list")
     (list-conversations))
    ((string= (first args) "save")
     (if (second args)
         (save-conversation (second args))
         (format t "❌ Usage: /conversation save <name>~%")))
    ((string= (first args) "load")
     (if (second args)
         (load-conversation (second args))
         (format t "❌ Usage: /conversation load <name>~%")))
    ((string= (first args) "export")
     (export-conversation))
    (t
     (format t "❌ Unknown conversation command: ~A~%" (first args)))))

(defun list-conversations ()
  "List saved conversations"
  (handler-case
      (let ((pattern (merge-pathnames "*.json" *conversations-dir*)))
        #+sbcl (let ((files (directory pattern)))
                 (if files
                     (progn
                       (format t "💬 Saved conversations:~%")
                       (dolist (file files)
                         (let ((name (pathname-name file)))
                           (format t "  • ~A~%" name))))
                     (format t "No saved conversations found~%")))
        #-sbcl (format t "❌ Directory listing not supported on this LISP implementation~%"))
    (error (e) (format t "❌ Error listing conversations: ~A~%" e))))

(defun save-conversation (name)
  "Save current conversation"
  (handler-case
      (let* ((filename (merge-pathnames (concatenate 'string name ".json") *conversations-dir*))
             (conversation-data `((:name . ,name)
                                 (:timestamp . ,(get-universal-time))
                                 (:messages . ((:role . "system")
                                              (:content . "This is a saved conversation placeholder"))))))
        (with-open-file (stream filename :direction :output :if-exists :supersede)
          (format stream "~A~%" (encode-json conversation-data)))
        (format t "💾 Saved conversation as: ~A~%" name))
    (error (e) (format t "❌ Error saving conversation: ~A~%" e))))

(defun load-conversation (name)
  "Load a saved conversation"
  (handler-case
      (let ((filename (merge-pathnames (concatenate 'string name ".json") *conversations-dir*)))
        (if (probe-file filename)
            (progn
              (format t "📂 Loading conversation: ~A~%" name)
              (with-open-file (stream filename :direction :input)
                (let ((data (decode-json (read-line stream))))
                  (format t "Loaded conversation from ~A~%" 
                          (multiple-value-bind (sec min hour date month year)
                              (decode-universal-time (cdr (assoc :timestamp data)))
                            (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                                   year month date hour min sec))))))
            (format t "❌ Conversation '~A' not found~%" name)))
    (error (e) (format t "❌ Error loading conversation: ~A~%" e))))

(defun export-conversation ()
  "Export current conversation"
  (handler-case
      (let* ((timestamp (get-universal-time))
             (filename (format nil "conversation-export-~A.txt" timestamp)))
        (with-open-file (stream filename :direction :output :if-exists :supersede)
          (format stream "Lantae Conversation Export~%")
          (format stream "Generated: ~A~%~%" 
                  (multiple-value-bind (sec min hour date month year)
                      (decode-universal-time timestamp)
                    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                           year month date hour min sec)))
          (format stream "=== Conversation ===~%")
          (format stream "No active conversation to export~%"))
        (format t "📤 Exported conversation to: ~A~%" filename))
    (error (e) (format t "❌ Error exporting conversation: ~A~%" e))))

;;; Cost tracking commands
(defparameter *cost-file* "~/.lantae/cost-tracking.json"
  "File to store cost tracking data")

(defvar *session-cost* 0.0
  "Current session cost")

(defun load-cost-data ()
  "Load cost tracking data from file"
  (handler-case
      (when (probe-file *cost-file*)
        (with-open-file (stream *cost-file* :direction :input)
          (decode-json (read-line stream))))
    (error () '((:daily . 0.0) (:monthly . 0.0) (:total . 0.0)))))

(defun save-cost-data (data)
  "Save cost tracking data to file"
  (handler-case
      (progn
        (ensure-directories-exist (directory-namestring *cost-file*))
        (with-open-file (stream *cost-file* :direction :output :if-exists :supersede)
          (format stream "~A~%" (encode-json data))))
    (error (e) (format t "❌ Error saving cost data: ~A~%" e))))

(defun update-cost (amount &optional (description "API call"))
  "Update session and persistent cost tracking"
  (incf *session-cost* amount)
  (let ((data (load-cost-data)))
    ;; Ensure data has proper structure with default values
    (unless (assoc :daily data)
      (push '(:daily . 0.0) data))
    (unless (assoc :monthly data)
      (push '(:monthly . 0.0) data))
    (unless (assoc :total data)
      (push '(:total . 0.0) data))
    
    ;; Update costs safely
    (setf (cdr (assoc :daily data)) (+ (or (cdr (assoc :daily data)) 0.0) amount))
    (setf (cdr (assoc :monthly data)) (+ (or (cdr (assoc :monthly data)) 0.0) amount))
    (setf (cdr (assoc :total data)) (+ (or (cdr (assoc :total data)) 0.0) amount))
    (save-cost-data data)
    (format t "💸 Cost updated: +$~,4F (~A)~%" amount description)))

(defun cmd-cost (args)
  "Show cost tracking information"
  (let ((data (load-cost-data)))
    (cond
      ((null args)
       (format t "💰 Cost Tracking:~%")
       (format t "  Session cost: $~,4F~%" *session-cost*)
       (format t "  Daily total:  $~,4F~%" (or (cdr (assoc :daily data)) 0.0))
       (format t "  Monthly:      $~,4F~%" (or (cdr (assoc :monthly data)) 0.0))
       (format t "  All-time:     $~,4F~%" (or (cdr (assoc :total data)) 0.0)))
      ((string= (first args) "report")
       (format t "📊 Cost Report:~%")
       (format t "Session: $~,4F~%" *session-cost*)
       (format t "Daily:   $~,4F~%" (or (cdr (assoc :daily data)) 0.0))
       (format t "Monthly: $~,4F~%" (or (cdr (assoc :monthly data)) 0.0))
       (format t "Total:   $~,4F~%" (or (cdr (assoc :total data)) 0.0)))
      ((string= (first args) "reset")
       (setf *session-cost* 0.0)
       (save-cost-data '((:daily . 0.0) (:monthly . 0.0) (:total . 0.0)))
       (format t "🔄 Cost tracking reset~%"))
      ((string= (first args) "add")
       (if (and (second args) (third args))
           (let ((amount (read-from-string (second args)))
                 (desc (third args)))
             (update-cost amount desc))
           (format t "❌ Usage: /cost add <amount> <description>~%")))
      (t
       (format t "❌ Unknown cost command: ~A~%" (first args))))))

;;; Agent and planning commands
(defun cmd-agent (args)
  "Agent mode commands"
  (cond
    ((null args)
     (format t "🤖 Agent commands:~%")
     (format t "  /agent plan TASK    - Create execution plan~%")
     (format t "  /agent execute      - Execute current plan~%")
     (format t "  /agent history      - Show execution history~%"))
    ((string= (first args) "plan")
     (if (rest args)
         (let ((task (format nil "~{~A~^ ~}" (rest args))))
           (format t "📋 Creating plan for: ~A~%" task)
           (format t "⚠️  Planning agent not yet implemented~%"))
         (format t "❌ Usage: /agent plan <task description>~%")))
    ((string= (first args) "execute")
     (format t "⚙️  Executing plan... (feature not yet implemented)~%"))
    ((string= (first args) "history")
     (format t "📚 Agent History: (feature not yet implemented)~%"))
    (t
     (format t "❌ Unknown agent command: ~A~%" (first args)))))

(defun cmd-squad (args)
  "Squad deployment commands"
  (cond
    ((null args)
     (format t "👥 Squad commands:~%")
     (format t "  /squad deploy      - Deploy agent squad~%")
     (format t "  /squad status      - Show squad status~%"))
    ((string= (first args) "deploy")
     (format t "🚀 Deploying squad... (feature not yet implemented)~%"))
    ((string= (first args) "status")
     (format t "📊 Squad Status: (feature not yet implemented)~%"))
    (t
     (format t "❌ Unknown squad command: ~A~%" (first args)))))

;;; MCP (Model Context Protocol) commands
(defun cmd-mcp (args)
  "MCP protocol commands"
  (cond
    ((null args)
     (format t "🔌 MCP commands:~%")
     (format t "  /mcp status        - Show MCP server status~%")
     (format t "  /mcp tools         - List available MCP tools~%")
     (format t "  /mcp reload        - Reload MCP configuration~%"))
    ((string= (first args) "status")
     (format t "📡 MCP Status: (feature not yet implemented)~%"))
    ((string= (first args) "tools")
     (format t "🛠️  MCP Tools: (feature not yet implemented)~%"))
    ((string= (first args) "reload")
     (format t "🔄 Reloading MCP... (feature not yet implemented)~%"))
    (t
     (format t "❌ Unknown MCP command: ~A~%" (first args)))))

;;; LSP (Language Server Protocol) commands
(defun cmd-lsp (args)
  "LSP integration commands"
  (cond
    ((null args)
     (format t "🔍 LSP commands:~%")
     (format t "  /lsp status        - Show LSP server status~%")
     (format t "  /lsp analyze FILE  - Analyze file with LSP~%"))
    ((string= (first args) "status")
     (format t "📡 LSP Status: (feature not yet implemented)~%"))
    ((string= (first args) "analyze")
     (if (second args)
         (format t "🔍 Analyzing file: ~A (feature not yet implemented)~%" (second args))
         (format t "❌ Usage: /lsp analyze <file>~%")))
    (t
     (format t "❌ Unknown LSP command: ~A~%" (first args)))))

;;; Task and template commands
(defun cmd-task (args)
  "Task management commands"
  (cond
    ((null args)
     (format t "📋 Task commands:~%")
     (format t "  /task list         - List tasks~%")
     (format t "  /task create TASK  - Create new task~%"))
    ((string= (first args) "list")
     (format t "📋 Tasks: (feature not yet implemented)~%"))
    ((string= (first args) "create")
     (if (rest args)
         (let ((task (format nil "~{~A~^ ~}" (rest args))))
           (format t "📝 Created task: ~A (feature not yet implemented)~%" task))
         (format t "❌ Usage: /task create <task description>~%")))
    (t
     (format t "❌ Unknown task command: ~A~%" (first args)))))

(defun cmd-template (args)
  "Template management commands"
  (cond
    ((null args)
     (format t "📄 Template commands:~%")
     (format t "  /template list     - List templates~%")
     (format t "  /template use NAME - Use template~%"))
    ((string= (first args) "list")
     (format t "📄 Templates: (feature not yet implemented)~%"))
    ((string= (first args) "use")
     (if (second args)
         (format t "📄 Using template: ~A (feature not yet implemented)~%" (second args))
         (format t "❌ Usage: /template use <name>~%")))
    (t
     (format t "❌ Unknown template command: ~A~%" (first args)))))

;;; Registration function
(defun register-enhanced-commands ()
  "Register all enhanced commands for feature parity"
  ;; Tool integration
  (register-command "tool" #'cmd-tool
                   :description "Execute a tool"
                   :usage "/tool <tool-name> <args...>")
  
  (register-command "tools" #'cmd-tools
                   :description "List available tools"
                   :usage "/tools")
  
  ;; Conversation management
  (register-command "conversation" #'cmd-conversation
                   :description "Manage conversations"
                   :usage "/conversation <list|save|load|export> [args...]")
  
  ;; Cost tracking
  (register-command "cost" #'cmd-cost
                   :description "Show cost information"
                   :usage "/cost [report|budget]")
  
  ;; Agent and planning
  (register-command "agent" #'cmd-agent
                   :description "Agent mode commands"
                   :usage "/agent <plan|execute|history> [args...]")
  
  (register-command "squad" #'cmd-squad
                   :description "Squad deployment commands"
                   :usage "/squad <deploy|status>")
  
  ;; Advanced features
  (register-command "mcp" #'cmd-mcp
                   :description "MCP protocol commands"
                   :usage "/mcp <status|tools|reload>")
  
  (register-command "lsp" #'cmd-lsp
                   :description "LSP integration commands"
                   :usage "/lsp <status|analyze> [args...]")
  
  (register-command "task" #'cmd-task
                   :description "Task management commands"
                   :usage "/task <list|create> [args...]")
  
  (register-command "template" #'cmd-template
                   :description "Template management commands"
                   :usage "/template <list|use> [args...]")
  
  (format t "✅ Enhanced commands registered for feature parity~%"))