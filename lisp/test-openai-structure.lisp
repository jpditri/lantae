;;;; test-openai-structure.lisp - Test OpenAI provider structure without API calls

(load "load-lantae.lisp")

(format t "~%Testing OpenAI Provider Structure...~%~%")

;; Test that the package loaded correctly
(if (find-package :lantae-providers-openai)
    (format t "✓ OpenAI provider package loaded~%")
    (format t "✗ OpenAI provider package not loaded~%"))

;; Test that functions are defined
(let ((functions '("MAKE-OPENAI-PROVIDER" "OPENAI-CHAT" "OPENAI-LIST-MODELS" 
                  "FORMAT-OPENAI-HEADERS" "HANDLE-OPENAI-ERROR")))
  (dolist (func functions)
    (if (fboundp (intern func :lantae-providers-openai))
        (format t "✓ Function ~A defined~%" func)
        (format t "✗ Function ~A not defined~%" func))))

;; Test model list without API key
(format t "~%Testing model list (hardcoded)...~%")
(handler-case
    (let ((models (funcall (intern "OPENAI-LIST-MODELS" :lantae-providers-openai) nil)))
      (if (lantae-providers:result-success-p models)
          (progn
            (format t "✓ Model list retrieved~%")
            (format t "Models: ~{~A~^, ~}~%" (lantae-providers:result-value models)))
          (format t "✗ Failed to get model list~%")))
  (error (e)
    (format t "✗ Error getting model list: ~A~%" e)))

;; Test error handling
(format t "~%Testing error handling...~%")
(let ((error-msg (funcall (intern "HANDLE-OPENAI-ERROR" :lantae-providers-openai) 401 "Unauthorized")))
  (format t "401 error: ~A~%" error-msg))

(let ((error-msg (funcall (intern "HANDLE-OPENAI-ERROR" :lantae-providers-openai) 429 "Rate limit")))
  (format t "429 error: ~A~%" error-msg))

;; Test message formatting
(format t "~%Testing message formatting...~%")
(let ((formatted (funcall (intern "FORMAT-OPENAI-MESSAGE" :lantae-providers-openai) 
                         '(:role "user" :content "Hello"))))
  (format t "Formatted message: ~A~%" formatted))

;; Test provider creation (should fail without API key)
(format t "~%Testing provider creation without API key...~%")
(handler-case
    (funcall (intern "MAKE-OPENAI-PROVIDER" :lantae-providers-openai))
  (error (e)
    (format t "✓ Correctly failed: ~A~%" e)))

;; Test with fake API key
(format t "~%Testing provider creation with fake API key...~%")
(handler-case
    (let ((provider (funcall (intern "MAKE-OPENAI-PROVIDER" :lantae-providers-openai) 
                            :api-key "fake-key")))
      (format t "✓ Provider created with fake key~%")
      (format t "Provider name: ~A~%" (lantae-providers:provider-name provider)))
  (error (e)
    (format t "✗ Failed to create provider: ~A~%" e)))

(format t "~%OpenAI provider structure test completed!~%")
(lantae-utils:quit 0)