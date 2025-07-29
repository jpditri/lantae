;;;; test-function-calling.lisp - Test function calling integration
;;;;
;;;; Quick test to verify function calling works with providers

(in-package :cl-user)

;; Load the system if not already loaded
(unless (find-package :lantae)
  (load "load-lantae.lisp"))

(defun test-function-calling ()
  "Test function calling with built-in tools"
  (format t "~%=== Testing Function Calling ===~%")
  
  ;; Test tool registry
  (format t "Available tools: ~{~A~^, ~}~%" 
          (lantae-function-calling:list-tools))
  
  ;; Test tool execution
  (format t "~%Testing get_current_time tool:~%")
  (let ((result (lantae-function-calling:execute-tool 
                 "get_current_time" 
                 '((:format . "readable")))))
    (format t "Result: ~A~%" result))
  
  ;; Test calculator tool
  (format t "~%Testing calculate tool:~%")
  (let ((result (lantae-function-calling:execute-tool 
                 "calculate" 
                 '((:operation . "add") (:a . 5) (:b . 3)))))
    (format t "Result: ~A~%" result))
  
  ;; Test system info tool
  (format t "~%Testing get_system_info tool:~%")
  (let ((result (lantae-function-calling:execute-tool 
                 "get_system_info" 
                 '((:info_type . "env")))))
    (format t "Result: ~A~%" result))
  
  ;; Test tool formatting for different providers
  (format t "~%Testing tool formatting:~%")
  (let* ((tool (lantae-function-calling:get-tool "get_current_time"))
         (openai-format (lantae-function-calling:format-tool-for-provider "openai" tool))
         (anthropic-format (lantae-function-calling:format-tool-for-provider "anthropic" tool)))
    (format t "OpenAI format: ~A~%" openai-format)
    (format t "Anthropic format: ~A~%" anthropic-format))
  
  (format t "~%Function calling test completed!~%"))

;; Run the test
(test-function-calling)