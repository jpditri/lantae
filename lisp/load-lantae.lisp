;;;; load-lantae.lisp - Load all Lantae LISP modules in correct order
;;;;
;;;; This file ensures all dependencies are loaded properly

(in-package :cl-user)

;;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Load required libraries
(handler-case
    (progn
      (ql:quickload '(:drakma :cl-json :flexi-streams :bordeaux-threads :cl-ppcre) :silent t)
      (format t "✓ External dependencies loaded~%"))
  (error (e)
    (format t "Warning: Could not load some dependencies: ~A~%" e)
    (format t "Please run: (ql:quickload '(:drakma :cl-json :flexi-streams))~%")))

;;; Define load order for Lantae modules
(defparameter *lantae-modules*
  '(;; Utilities first
    "src/utils/utils.lisp"
    "src/utils/http-client.lisp"
    "src/utils/colors.lisp"
    "src/utils/mission-abort.lisp"
    
    ;; Configuration
    "src/config/config.lisp"
    
    ;; Providers
    "src/providers/providers.lisp"
    "src/providers/ollama-provider.lisp"
    
    ;; CLI components
    "src/cli/commands.lisp"
    
    ;; Main entry point
    "lantae.lisp"))

;;; Load modules
(defun load-lantae-modules ()
  "Load all Lantae modules in order"
  (let ((base-path (or *load-pathname* *default-pathname-defaults*)))
    (dolist (module *lantae-modules*)
      (let ((module-path (merge-pathnames module base-path)))
        (if (probe-file module-path)
            (handler-case
                (progn
                  (load module-path)
                  (format t "✓ Loaded ~A~%" module))
              (error (e)
                (format t "✗ Error loading ~A: ~A~%" module e)))
            (format t "✗ Module not found: ~A~%" module-path))))))

;;; Load everything
(format t "~%Loading Lantae LISP...~%")
(load-lantae-modules)

;;; Initialize system
(when (find-package :lantae)
  (format t "~%Initializing Lantae...~%")
  (funcall (intern "INITIALIZE-SYSTEM" :lantae))
  (format t "~%Lantae LISP loaded successfully!~%")
  (format t "Run (lantae:start-repl) to begin.~%~%"))