;;;; start-repl.lisp - Development REPL for Lantae LISP
;;;; 
;;;; Usage: sbcl --load start-repl.lisp

;; Load the main system
(load "lantae.lisp")
(load "src/config/config.lisp")
(load "src/providers/providers.lisp")
(load "src/cli/commands.lisp")

;; Switch to the main package
(in-package :lantae)

;; Initialize the system
(format t "~%Initializing Lantae LISP...~%")

;; Set up some convenience functions for development
(defun reload ()
  "Reload all Lantae modules"
  (load "lantae.lisp")
  (load "src/config/config.lisp")
  (load "src/providers/providers.lisp")  
  (load "src/cli/commands.lisp")
  (format t "Reloaded all modules.~%"))

(defun test-provider ()
  "Test provider functionality"
  (lantae-providers:initialize-providers)
  (let ((models (lantae-providers:list-provider-models "ollama")))
    (format t "Ollama models: ~A~%" models)))

(defun test-config ()
  "Test configuration system"
  (lantae-config:set-config :test-key "test-value")
  (format t "Config test: ~A~%" (lantae-config:get-config :test-key)))

(defun test-commands ()
  "Test command system"
  (lantae-commands:execute-command "help"))

;; Print welcome message
(format t "~%Welcome to Lantae LISP Development REPL!~%")
(format t "~%Available functions:~%")
(format t "  (start-repl)    - Start the interactive REPL~%")
(format t "  (reload)        - Reload all modules~%")
(format t "  (test-provider) - Test provider system~%")
(format t "  (test-config)   - Test configuration system~%")
(format t "  (test-commands) - Test command system~%")
(format t "~%Type (start-repl) to begin or test individual components.~%~%")

;; Optionally start REPL immediately
;; (start-repl)