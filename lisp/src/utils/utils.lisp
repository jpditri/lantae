;;;; utils.lisp - Common utility functions used across Lantae
;;;;
;;;; This file contains shared utility functions to avoid duplication
;;;; and circular dependencies between modules

(defpackage :lantae-utils
  (:use :cl)
  (:export #:split-string
           #:join-strings
           #:parse-number
           #:parse-value
           #:format-time-string
           #:quit
           #:merge-plists
           #:trim-string))

(in-package :lantae-utils)

;;; String manipulation utilities
(defun split-string (string delimiter)
  "Split string by delimiter character or string"
  (let ((delimiter-length (if (characterp delimiter) 1 (length delimiter))))
    (loop for i = 0 then (+ j delimiter-length)
          as j = (if (characterp delimiter)
                     (position delimiter string :start i)
                     (search delimiter string :start2 i))
          collect (subseq string i j)
          while j)))

(defun join-strings (strings &optional (separator " "))
  "Join list of strings with separator"
  (if (null strings)
      ""
      (format nil (concatenate 'string "~A~{" separator "~A~}") 
              (first strings) (rest strings))))

(defun trim-string (string)
  "Remove leading and trailing whitespace"
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

;;; Type conversion utilities
(defun parse-number (string)
  "Parse number from string, returns nil on failure"
  (handler-case
      (let ((trimmed (trim-string string)))
        (cond
          ((string= trimmed "") nil)
          ((find #\. trimmed) (read-from-string trimmed))
          (t (parse-integer trimmed))))
    (error () nil)))

(defun parse-value (string)
  "Parse string to appropriate Lisp value"
  (let ((trimmed (trim-string string)))
    (cond
      ((string= trimmed "") nil)
      ((string-equal trimmed "true") t)
      ((string-equal trimmed "false") nil)
      ((string-equal trimmed "nil") nil)
      ;; Try to parse as number
      ((every (lambda (c) (or (digit-char-p c) (member c '(#\. #\- #\+)))) trimmed)
       (handler-case (read-from-string trimmed)
         (error () trimmed)))
      ;; Return as string
      (t trimmed))))

;;; Time utilities
(defun format-time-string (&optional (universal-time (get-universal-time)))
  "Format time as string"
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour min sec)))

;;; System utilities
(defun quit (&optional (code 0))
  "Exit the program portably"
  #+sbcl (sb-ext:exit :code code)
  #+ccl (ccl:quit code)
  #+clisp (ext:quit code)
  #+ecl (ext:quit code)
  #+abcl (ext:quit code)
  #-(or sbcl ccl clisp ecl abcl) 
  (error "Don't know how to quit on this implementation"))

;;; Data structure utilities
(defun merge-plists (plist1 plist2)
  "Merge two property lists, with plist2 taking precedence"
  (let ((result (copy-list plist1)))
    (loop for (key value) on plist2 by #'cddr
          do (setf (getf result key) value))
    result))

;;; Export all symbols
(export '(split-string join-strings parse-number parse-value 
          format-time-string quit merge-plists trim-string))