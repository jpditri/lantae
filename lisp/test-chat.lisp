;;;; test-chat.lisp - Test the Ollama provider with a simple chat

(load "load-lantae.lisp")

(format t "~%Testing Lantae LISP implementation...~%~%")

;; Test listing models (which also checks connection)
(format t "Listing available models...~%")
(let ((models-result (lantae-providers:provider-models "ollama")))
  (if (lantae-providers:result-success-p models-result)
      (progn
        (format t "✓ Ollama is connected~%")
        (format t "Available models:~%")
        (dolist (model (lantae-providers:result-value models-result))
          (format t "  - ~A~%" model))
        (format t "~%"))
      (progn
        (format t "✗ Cannot connect to Ollama: ~A~%" (lantae-providers:result-error models-result))
        (format t "Please ensure Ollama is running with: ollama serve~%")
        (lantae-utils:quit 1))))

;; Test a simple chat
(format t "Testing chat functionality...~%")
(let* ((messages '((:role "user" :content "Say hello in one sentence")))
       (result (lantae-providers:provider-chat "ollama" "cogito:latest" messages :temperature 0.1)))
  
  (if (lantae-providers:result-success-p result)
      (progn
        (format t "✓ Chat successful~%")
        (format t "Response: ~A~%~%" (lantae-providers:result-value result)))
      (format t "✗ Chat failed: ~A~%~%" (lantae-providers:result-error result))))

(format t "Test completed!~%")
(lantae-utils:quit 0)