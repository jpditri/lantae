;;;; test-anthropic.lisp - Test the Anthropic provider

(load "load-lantae.lisp")

(format t "~%Testing Anthropic Provider Structure...~%~%")

;; Test that the package loaded correctly
(if (find-package :lantae-providers-anthropic)
    (format t "✓ Anthropic provider package loaded~%")
    (format t "✗ Anthropic provider package not loaded~%"))

;; Test that functions are defined
(let ((functions '("MAKE-ANTHROPIC-PROVIDER" "ANTHROPIC-CHAT" "ANTHROPIC-LIST-MODELS" 
                  "FORMAT-ANTHROPIC-HEADERS" "HANDLE-ANTHROPIC-ERROR")))
  (dolist (func functions)
    (if (fboundp (intern func :lantae-providers-anthropic))
        (format t "✓ Function ~A defined~%" func)
        (format t "✗ Function ~A not defined~%" func))))

;; Test model list without API key
(format t "~%Testing model list (hardcoded)...~%")
(handler-case
    (let ((models (funcall (intern "ANTHROPIC-LIST-MODELS" :lantae-providers-anthropic) nil)))
      (if (lantae-providers:result-success-p models)
          (progn
            (format t "✓ Model list retrieved~%")
            (format t "Models: ~{~A~^, ~}~%" (lantae-providers:result-value models)))
          (format t "✗ Failed to get model list~%")))
  (error (e)
    (format t "✗ Error getting model list: ~A~%" e)))

;; Test error handling
(format t "~%Testing error handling...~%")
(let ((error-msg (funcall (intern "HANDLE-ANTHROPIC-ERROR" :lantae-providers-anthropic) 401 "Unauthorized")))
  (format t "401 error: ~A~%" error-msg))

(let ((error-msg (funcall (intern "HANDLE-ANTHROPIC-ERROR" :lantae-providers-anthropic) 429 "Rate limit")))
  (format t "429 error: ~A~%" error-msg))

;; Test message formatting
(format t "~%Testing message formatting...~%")
(let ((formatted (funcall (intern "FORMAT-ANTHROPIC-MESSAGE" :lantae-providers-anthropic) 
                         '(:role "user" :content "Hello"))))
  (format t "Formatted message: ~A~%" formatted))

;; Test provider creation (should fail without API key)
(format t "~%Testing provider creation without API key...~%")
(handler-case
    (funcall (intern "MAKE-ANTHROPIC-PROVIDER" :lantae-providers-anthropic))
  (error (e)
    (format t "✓ Correctly failed: ~A~%" e)))

;; Test with fake API key
(format t "~%Testing provider creation with fake API key...~%")
(handler-case
    (let ((provider (funcall (intern "MAKE-ANTHROPIC-PROVIDER" :lantae-providers-anthropic) 
                            :api-key "fake-key")))
      (format t "✓ Provider created with fake key~%")
      (format t "Provider name: ~A~%" (lantae-providers:provider-name provider)))
  (error (e)
    (format t "✗ Failed to create provider: ~A~%" e)))

;; Test provider registry
(format t "~%Testing provider registration...~%")
(let ((provider (lantae-providers:get-provider "anthropic")))
  (if provider
      (format t "✓ Anthropic provider found in registry~%")
      (format t "✗ Anthropic provider not found in registry~%")))

;; List all available providers
(format t "~%Available providers:~%")
(dolist (provider (lantae-providers:list-providers))
  (format t "  - ~A~%" provider))

(format t "~%Anthropic provider structure test completed!~%")
(lantae-utils:quit 0)