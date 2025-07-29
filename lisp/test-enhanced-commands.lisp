;;;; test-enhanced-commands.lisp - Test enhanced command backends
;;;;
;;;; Test the actual implementations of enhanced commands

(in-package :cl-user)

;; Load the system if not already loaded
(unless (find-package :lantae)
  (load "load-lantae.lisp"))

(defun test-enhanced-commands ()
  "Test enhanced command implementations"
  (format t "~%=== Testing Enhanced Command Backends ===~%")
  
  ;; Test tool command with function calling
  (format t "~%1. Testing /tool command with function calling tools:~%")
  (lantae-enhanced-commands::cmd-tool '("get_current_time"))
  
  ;; Test cost tracking
  (format t "~%2. Testing /cost command:~%")
  (lantae-enhanced-commands::cmd-cost '())
  (lantae-enhanced-commands::cmd-cost '("add" "0.05" "test-charge"))
  (lantae-enhanced-commands::cmd-cost '("report"))
  
  ;; Test conversation management
  (format t "~%3. Testing /conversation command:~%")
  (lantae-enhanced-commands::cmd-conversation '("save" "test-conversation"))
  (lantae-enhanced-commands::cmd-conversation '("list"))
  
  ;; Test system tool
  (format t "~%4. Testing system tools:~%")
  (lantae-enhanced-commands::cmd-tool '("ls" "."))
  
  ;; Test file operations
  (format t "~%5. Testing file operations:~%")
  (lantae-enhanced-commands::cmd-tool '("cat" "load-lantae.lisp" "10"))
  
  (format t "~%Enhanced commands backend test completed!~%"))

;; Run the test
(test-enhanced-commands)