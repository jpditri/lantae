;;;; test-provider-switching.lisp - Test provider switching functionality

(load "load-lantae.lisp")

(format t "~%Testing Provider Switching...~%~%")

;; Test initial provider listing
(format t "Available providers:~%")
(dolist (provider (lantae-providers:list-providers))
  (format t "  - ~A~%" provider))

;; Test switching to OpenAI (should work even without API key for listing)
(format t "~%Testing provider switch to OpenAI...~%")
(lantae:set-config :provider "openai")
(format t "Current provider: ~A~%" (lantae:get-config :provider))

;; Try to list models (should work since it's hardcoded)
(format t "~%Testing OpenAI model listing...~%")
(let ((models-result (lantae-providers:provider-models "openai")))
  (if (lantae-providers:result-success-p models-result)
      (progn
        (format t "✓ OpenAI models listed successfully~%")
        (format t "Models: ~{~A~^, ~}~%" (lantae-providers:result-value models-result)))
      (format t "✗ Failed to list OpenAI models: ~A~%" 
              (lantae-providers:result-error models-result))))

;; Switch back to Ollama
(format t "~%Testing provider switch back to Ollama...~%")
(lantae:set-config :provider "ollama")
(format t "Current provider: ~A~%" (lantae:get-config :provider))

;; Test Ollama models
(format t "~%Testing Ollama model listing...~%")
(let ((models-result (lantae-providers:provider-models "ollama")))
  (if (lantae-providers:result-success-p models-result)
      (progn
        (format t "✓ Ollama models listed successfully~%")
        (format t "Models: ~{~A~^, ~}~%" (lantae-providers:result-value models-result)))
      (format t "✗ Failed to list Ollama models: ~A~%" 
              (lantae-providers:result-error models-result))))

;; Test invalid provider
(format t "~%Testing invalid provider...~%")
(let ((models-result (lantae-providers:provider-models "invalid")))
  (if (lantae-providers:result-success-p models-result)
      (format t "✗ Unexpected success with invalid provider~%")
      (format t "✓ Correctly failed with invalid provider: ~A~%" 
              (lantae-providers:result-error models-result))))

;; Test model switching
(format t "~%Testing model switching...~%")
(lantae:set-config :model "gpt-4o")
(format t "Current model: ~A~%" (lantae:get-config :model))

(lantae:set-config :model "cogito:latest")
(format t "Current model: ~A~%" (lantae:get-config :model))

;; Test temperature setting
(format t "~%Testing temperature setting...~%")
(lantae:set-config :temperature 0.5)
(format t "Current temperature: ~A~%" (lantae:get-config :temperature))

(lantae:set-config :temperature 0.1)
(format t "Current temperature: ~A~%" (lantae:get-config :temperature))

(format t "~%Provider switching test completed!~%")
(lantae-utils:quit 0)