;;;; test-openai.lisp - Test the OpenAI provider

(load "load-lantae.lisp")

(format t "~%Testing OpenAI Provider...~%~%")

;; Check if OpenAI API key is available
(let ((api-key #+sbcl (sb-ext:posix-getenv "OPENAI_API_KEY")
               #-sbcl nil))
  (if api-key
      (progn
        (format t "✓ OpenAI API key found~%")
        
        ;; Test provider registration
        (let ((provider (lantae-providers:get-provider "openai")))
          (if provider
              (progn
                (format t "✓ OpenAI provider registered~%")
                
                ;; Test model listing
                (format t "~%Testing model listing...~%")
                (let ((models-result (lantae-providers:provider-models "openai")))
                  (if (lantae-providers:result-success-p models-result)
                      (progn
                        (format t "✓ Model listing successful~%")
                        (format t "Available models:~%")
                        (dolist (model (lantae-providers:result-value models-result))
                          (format t "  - ~A~%" model)))
                      (format t "✗ Model listing failed: ~A~%" 
                              (lantae-providers:result-error models-result))))
                
                ;; Test a simple chat (using cheapest model)
                (format t "~%Testing chat functionality...~%")
                (let* ((messages '((:role "user" :content "Say 'Hello from LISP!' in one sentence")))
                       (result (lantae-providers:provider-chat "openai" "gpt-4o-mini" messages :temperature 0.1)))
                  
                  (if (lantae-providers:result-success-p result)
                      (progn
                        (format t "✓ Chat successful~%")
                        (format t "Response: ~A~%~%" 
                                (getf (lantae-providers:result-value result) :content)))
                      (format t "✗ Chat failed: ~A~%~%" 
                              (lantae-providers:result-error result)))))
              (format t "✗ OpenAI provider not registered~%")))
        
        ;; Test direct provider creation
        (format t "~%Testing direct provider creation...~%")
        (handler-case
            (let ((direct-provider (lantae-providers-openai:make-openai-provider :api-key api-key)))
              (format t "✓ Direct provider creation successful~%")
              (format t "Provider name: ~A~%" (lantae-providers:provider-name direct-provider)))
          (error (e)
            (format t "✗ Direct provider creation failed: ~A~%" e))))
      
      (progn
        (format t "✗ OpenAI API key not found~%")
        (format t "Set OPENAI_API_KEY environment variable to test OpenAI functionality~%")
        (format t "~%Testing provider creation without API key...~%")
        (handler-case
            (lantae-providers-openai:make-openai-provider)
          (error (e)
            (format t "✓ Correctly failed without API key: ~A~%" e))))))

(format t "~%OpenAI provider test completed!~%")
(lantae-utils:quit 0)