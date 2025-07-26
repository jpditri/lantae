;;;; run-interactive.lisp - Interactive runner for Lantae LISP
;;;;
;;;; This script properly sets up the LISP environment for interactive use

;; Load the system
(load "lantae.lisp")
(load "src/config/config.lisp")
(load "src/providers/providers.lisp")
(load "src/cli/commands.lisp")

;; Start REPL
(in-package :lantae)
(start-repl)