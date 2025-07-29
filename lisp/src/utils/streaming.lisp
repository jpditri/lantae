;;;; streaming.lisp - Streaming response handler for Lantae LISP
;;;;
;;;; Handles Server-Sent Events (SSE) streaming responses from providers
;;;; with real-time token display and progress indicators

(defpackage :lantae-streaming
  (:use :cl)
  (:export #:streaming-handler
           #:create-streaming-handler
           #:parse-sse-line
           #:extract-token
           #:display-token
           #:streaming-spinner
           #:*spinner-frames*
           #:*current-spinner-frame*))

(in-package :lantae-streaming)

;;; Spinner animation for progress indication
(defparameter *spinner-frames* 
  #("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Spinner animation frames for streaming progress")

(defparameter *current-spinner-frame* 0
  "Current spinner frame index")

(defun streaming-spinner ()
  "Get next spinner frame"
  (let ((frame (aref *spinner-frames* *current-spinner-frame*)))
    (setf *current-spinner-frame* 
          (mod (1+ *current-spinner-frame*) 
               (length *spinner-frames*)))
    frame))

;;; Streaming handler structure
(defstruct streaming-handler
  "Streaming response handler"
  provider
  on-token
  on-complete
  on-error
  buffer
  in-code-block
  code-language)

(defun create-streaming-handler (&key provider on-token on-complete on-error)
  "Create a new streaming handler"
  (make-streaming-handler
   :provider provider
   :on-token (or on-token #'default-token-handler)
   :on-complete (or on-complete #'default-complete-handler)
   :on-error (or on-error #'default-error-handler)
   :buffer ""
   :in-code-block nil
   :code-language nil))

(defun default-token-handler (token)
  "Default token handler - just print the token"
  (when (and token (> (length token) 0))
    (princ token)
    (force-output)))

(defun default-complete-handler ()
  "Default completion handler"
  (format t "~%"))

(defun default-error-handler (error)
  "Default error handler"
  (format t "~%Error: ~A~%" error))

;;; SSE parsing
(defun parse-sse-line (line)
  "Parse Server-Sent Events line"
  (cond
    ;; Data line
    ((and (> (length line) 6) (string= (subseq line 0 6) "data: "))
     (let ((data (string-trim " " (subseq line 6))))
       (values :data data)))
    ;; Event line
    ((and (> (length line) 7) (string= (subseq line 0 7) "event: "))
     (let ((event (string-trim " " (subseq line 7))))
       (values :event event)))
    ;; ID line
    ((and (> (length line) 4) (string= (subseq line 0 4) "id: "))
     (let ((id (string-trim " " (subseq line 4))))
       (values :id id)))
    ;; Empty line (end of event)
    ((string= line "")
     (values :end-event nil))
    ;; Unknown line
    (t
     (values :unknown line))))

(defun extract-token (json-data provider)
  "Extract token from JSON data based on provider"
  (handler-case
      (let ((parsed (decode-json json-data)))
        (case (intern (string-upcase provider) :keyword)
          (:openai
           (or (cdr (assoc :content (cdr (assoc :delta (first (cdr (assoc :choices parsed)))))))
               ""))
          (:anthropic
           (or (cdr (assoc :text (cdr (assoc :delta parsed))))
               ""))
          (:ollama
           (let ((message (cdr (assoc :message parsed)))
                 (response (cdr (assoc :response parsed))))
             (or (when message (cdr (assoc :content message)))
                 response
                 "")))
          (t
           (or (cdr (assoc :content parsed))
               (cdr (assoc :text parsed))
               ""))))
    (error ()
      "")))

(defun decode-json (json-string)
  "Decode JSON string to alist"
  (handler-case
      (cl-json:decode-json-from-string json-string)
    (error ()
      nil)))

;;; Code block detection and highlighting
(defun detect-code-block (text handler)
  "Detect and handle code block markers"
  (when (search "```" text)
    (if (streaming-handler-in-code-block handler)
        ;; End of code block
        (progn
          (setf (streaming-handler-in-code-block handler) nil)
          (setf (streaming-handler-code-language handler) nil))
        ;; Start of code block
        (let ((language (extract-language-from-marker text)))
          (setf (streaming-handler-in-code-block handler) t)
          (setf (streaming-handler-code-language handler) language)))))

(defun extract-language-from-marker (text)
  "Extract language from code block marker like ```python"
  (let ((marker-start (search "```" text)))
    (when marker-start
      (let ((lang-start (+ marker-start 3))
            (newline-pos (or (position #\Newline text :start (+ marker-start 3))
                            (length text))))
        (when (< lang-start newline-pos)
          (string-trim " " (subseq text lang-start newline-pos)))))))

(defun colorize-code-token (token language)
  "Apply syntax highlighting to code token (placeholder)"
  ;; For now, just return the token as-is
  ;; Could be enhanced with actual syntax highlighting
  (declare (ignore language))
  token)

;;; Enhanced token display
(defun display-token (token handler)
  "Display token with optional syntax highlighting"
  (when (and token (> (length token) 0))
    ;; Detect code blocks
    (detect-code-block token handler)
    
    ;; Apply highlighting if in code block
    (let ((display-token (if (streaming-handler-in-code-block handler)
                            (colorize-code-token token 
                                                (streaming-handler-code-language handler))
                            token)))
      ;; Call the token handler
      (funcall (streaming-handler-on-token handler) display-token))))

;;; Main streaming processing function
(defun process-streaming-line (line handler)
  "Process a single streaming line"
  (multiple-value-bind (type data) (parse-sse-line line)
    (case type
      (:data
       (cond
         ;; End marker
         ((string= data "[DONE]")
          (funcall (streaming-handler-on-complete handler)))
         ;; JSON data
         ((and (> (length data) 0) (char= (char data 0) #\{))
          (let ((token (extract-token data (streaming-handler-provider handler))))
            (when (and token (> (length token) 0))
              (display-token token handler))))
         ;; Plain text
         (t
          (when (> (length data) 0)
            (display-token data handler)))))
      (:event
       ;; Handle special events if needed
       nil)
      (:end-event
       ;; End of current event
       nil)
      (t
       ;; Unknown line type - ignore
       nil))))

;;; Streaming callback factory
(defun create-streaming-callback (provider &key on-token on-complete on-error)
  "Create a streaming callback function for HTTP streaming"
  (let ((handler (create-streaming-handler 
                  :provider provider
                  :on-token on-token
                  :on-complete on-complete
                  :on-error on-error)))
    (lambda (line)
      (handler-case
          (process-streaming-line line handler)
        (error (e)
          (funcall (streaming-handler-on-error handler) e))))))

;;; Convenience functions for different display modes
(defun create-simple-streaming-callback (provider)
  "Create a simple streaming callback that just prints tokens"
  (create-streaming-callback provider))

(defun create-colored-streaming-callback (provider)
  "Create a streaming callback with colored output"
  (create-streaming-callback 
   provider
   :on-token (lambda (token)
               (if (find-package :lantae-colors)
                   (funcall (intern "PRINT-COLORED" :lantae-colors) token :white)
                   (princ token))
               (force-output))
   :on-complete (lambda ()
                  (if (find-package :lantae-colors)
                      (funcall (intern "PRINT-COLORED" :lantae-colors) "~%" :green)
                      (format t "~%")))
   :on-error (lambda (error)
               (if (find-package :lantae-colors)
                   (funcall (intern "PRINT-ERROR" :lantae-colors) 
                           (format nil "Streaming error: ~A" error))
                   (format t "~%Error: ~A~%" error)))))

;;; Progress indicator for streaming
(defun show-streaming-progress (provider)
  "Show streaming progress indicator"
  (when (find-package :lantae-colors)
    (funcall (intern "PRINT-COLORED" :lantae-colors) 
             (format nil "~A Streaming from ~A..." (streaming-spinner) provider)
             :cyan))
  (unless (find-package :lantae-colors)
    (format t "~A Streaming from ~A...~%" (streaming-spinner) provider))
  (force-output))