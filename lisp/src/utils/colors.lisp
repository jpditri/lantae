;;;; colors.lisp - ANSI color output utilities for Lantae LISP
;;;;
;;;; Provides cross-platform colored terminal output with:
;;;; - ANSI escape code support
;;;; - Color detection
;;;; - Styled text formatting
;;;; - Progress indicators

(defpackage :lantae-colors
  (:use :cl)
  (:export #:*colors-enabled*
           #:with-color
           #:colorize
           #:print-colored
           #:red #:green #:yellow #:blue #:purple #:cyan #:white
           #:bold #:dim #:italic #:underline
           #:success #:error #:warning #:info #:prompt
           #:progress-bar
           #:spinner
           #:clear-line
           #:move-cursor))

(in-package :lantae-colors)

;;; Configuration
(defparameter *colors-enabled* t
  "Whether to use colored output")

(defparameter *force-colors* nil
  "Force colors even if terminal doesn't support them")

;;; ANSI escape codes
(defparameter *ansi-codes*
  '(;; Colors
    :black 30
    :red 31
    :green 32
    :yellow 33
    :blue 34
    :purple 35
    :magenta 35
    :cyan 36
    :white 37
    :default 39
    ;; Bright colors
    :bright-black 90
    :bright-red 91
    :bright-green 92
    :bright-yellow 93
    :bright-blue 94
    :bright-purple 95
    :bright-cyan 96
    :bright-white 97
    ;; Background colors
    :bg-black 40
    :bg-red 41
    :bg-green 42
    :bg-yellow 43
    :bg-blue 44
    :bg-purple 45
    :bg-cyan 46
    :bg-white 47
    ;; Styles
    :reset 0
    :bold 1
    :dim 2
    :italic 3
    :underline 4
    :blink 5
    :reverse 7
    :hidden 8
    :strikethrough 9))

;;; Terminal capability detection
(defun terminal-supports-color-p ()
  "Check if terminal supports colors"
  (or *force-colors*
      (and (not (equal #+sbcl (sb-ext:posix-getenv "NO_COLOR") 
                       #-sbcl nil
                       "1"))
           (or (member #+sbcl (sb-ext:posix-getenv "TERM")
                       #-sbcl nil
                       '("xterm" "xterm-color" "xterm-256color" 
                         "screen" "screen-256color" "tmux" "tmux-256color"
                         "rxvt-unicode" "rxvt-unicode-256color"
                         "linux" "cygwin")
                       :test #'string=)
               #+sbcl (sb-ext:posix-getenv "COLORTERM")
               #-sbcl nil))))

(defun update-colors-enabled ()
  "Update colors enabled based on terminal"
  (setf *colors-enabled* (terminal-supports-color-p)))

;; Initialize on load
(update-colors-enabled)

;;; ANSI escape sequence generation
(defun escape-sequence (&rest codes)
  "Generate ANSI escape sequence"
  (format nil "~C[~{~A~^;~}m" #\Escape codes))

(defun get-code (style)
  "Get ANSI code for style"
  (or (getf *ansi-codes* style) 0))

;;; Color application
(defun apply-styles (text styles)
  "Apply multiple styles to text"
  (if (and *colors-enabled* styles)
      (let ((codes (mapcar #'get-code styles))
            (reset (get-code :reset)))
        (format nil "~A~A~A" 
                (apply #'escape-sequence codes)
                text
                (escape-sequence reset)))
      text))

(defun colorize (text &rest styles)
  "Colorize text with given styles"
  (apply-styles text styles))

(defmacro with-color (styles &body body)
  "Execute body with color styles"
  `(let ((output (with-output-to-string (*standard-output*)
                   ,@body)))
     (princ (apply-styles output ',styles))))

(defun print-colored (text &rest styles)
  "Print colored text"
  (princ (apply #'colorize text styles)))

;;; Convenience functions for common colors
(defun red (text) (colorize text :red))
(defun green (text) (colorize text :green))
(defun yellow (text) (colorize text :yellow))
(defun blue (text) (colorize text :blue))
(defun purple (text) (colorize text :purple))
(defun cyan (text) (colorize text :cyan))
(defun white (text) (colorize text :white))

(defun bold (text) (colorize text :bold))
(defun dim (text) (colorize text :dim))
(defun italic (text) (colorize text :italic))
(defun underline (text) (colorize text :underline))

;;; Semantic color functions
(defun success (text) 
  (colorize text :green :bold))

(defun error (text) 
  (colorize text :red :bold))

(defun warning (text) 
  (colorize text :yellow :bold))

(defun info (text) 
  (colorize text :blue))

(defun prompt (text) 
  (colorize text :cyan :bold))

;;; Progress indicators
(defparameter *spinner-frames* 
  #("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Spinner animation frames")

(defparameter *current-spinner-frame* 0)

(defun spinner ()
  "Get next spinner frame"
  (let ((frame (aref *spinner-frames* *current-spinner-frame*)))
    (setf *current-spinner-frame* 
          (mod (1+ *current-spinner-frame*) 
               (length *spinner-frames*)))
    (colorize frame :cyan)))

(defun progress-bar (current total &key (width 30) (filled-char "█") (empty-char "░"))
  "Generate a progress bar"
  (let* ((percentage (if (zerop total) 0 (/ current total)))
         (filled (round (* width percentage)))
         (empty (- width filled)))
    (format nil "~A~A~A ~3D%"
            (colorize (make-string filled :initial-element (char filled-char 0)) :green)
            (colorize (make-string empty :initial-element (char empty-char 0)) :dim)
            (colorize "]" :white)
            (round (* 100 percentage)))))

;;; Terminal control
(defun clear-line ()
  "Clear current line"
  (when *colors-enabled*
    (princ (format nil "~C[2K~C[0G" #\Escape #\Escape))
    (force-output)))

(defun move-cursor (direction &optional (count 1))
  "Move cursor (direction: :up :down :left :right)"
  (when *colors-enabled*
    (let ((code (case direction
                  (:up "A")
                  (:down "B")
                  (:right "C")
                  (:left "D")
                  (t ""))))
      (unless (string= code "")
        (princ (format nil "~C[~A~A" #\Escape count code))
        (force-output)))))

(defun clear-screen ()
  "Clear terminal screen"
  (when *colors-enabled*
    (princ (format nil "~C[2J~C[H" #\Escape #\Escape))
    (force-output)))

;;; Styled printing utilities
(defun print-banner (title &key subtitle)
  "Print a styled banner"
  (let ((width 60)
        (border-char "═"))
    (format t "~%")
    (print-colored (format nil "╔~A╗" (make-string width :initial-element (char border-char 0))) :blue :bold)
    (format t "~%")
    (print-colored (format nil "║~A~A║" 
                          (make-string (floor (- width (length title)) 2) :initial-element #\Space)
                          title) :blue :bold)
    (format t "~A~%" (make-string (- width (length title) (floor (- width (length title)) 2)) :initial-element #\Space))
    (when subtitle
      (print-colored (format nil "║~A~A║" 
                            (make-string (floor (- width (length subtitle)) 2) :initial-element #\Space)
                            subtitle) :blue)
      (format t "~A~%" (make-string (- width (length subtitle) (floor (- width (length subtitle)) 2)) :initial-element #\Space)))
    (print-colored (format nil "╚~A╝" (make-string width :initial-element (char border-char 0))) :blue :bold)
    (format t "~%~%")))

(defun print-section (title)
  "Print a section header"
  (format t "~%")
  (print-colored (format nil "── ~A ──" title) :cyan :bold)
  (format t "~%~%"))

(defun print-item (label value &key (label-width 20))
  "Print a labeled item"
  (print-colored (format nil "~VA: " label-width label) :dim)
  (format t "~A~%" value))

(defun print-success (message)
  "Print success message"
  (print-colored "✓ " :green :bold)
  (format t "~A~%" message))

(defun print-error (message)
  "Print error message"
  (print-colored "✗ " :red :bold)
  (format t "~A~%" message))

(defun print-warning (message)
  "Print warning message"
  (print-colored "⚠ " :yellow :bold)
  (format t "~A~%" message))

(defun print-info (message)
  "Print info message"
  (print-colored "ℹ " :blue)
  (format t "~A~%" message))

;;; Box drawing
(defun draw-box (lines &key (padding 2) (color :white))
  "Draw a box around lines of text"
  (let* ((max-length (reduce #'max lines :key #'length :initial-value 0))
         (width (+ max-length (* 2 padding)))
         (top (format nil "┌~A┐" (make-string width :initial-element #\─)))
         (bottom (format nil "└~A┘" (make-string width :initial-element #\─)))
         (padding-str (make-string padding :initial-element #\Space)))
    (print-colored top color)
    (format t "~%")
    (dolist (line lines)
      (print-colored "│" color)
      (format t "~A~A~A" padding-str line 
              (make-string (- width (length line)) :initial-element #\Space))
      (print-colored "│" color)
      (format t "~%"))
    (print-colored bottom color)
    (format t "~%")))