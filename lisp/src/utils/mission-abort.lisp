;;;; mission-abort.lisp - Mission abort strategy for LISP implementation
;;;;
;;;; Provides escalation and error handling mechanisms when models fail

(defpackage :lantae-mission-abort
  (:use :cl)
  (:import-from :lantae-utils
                #:join-strings
                #:current-time-string)
  (:export #:abort-error
           #:abort-strategy
           #:make-abort-strategy
           #:should-abort-p
           #:execute-with-abort-handling
           #:add-escalation-provider
           #:abort-metrics))

(in-package :lantae-mission-abort)

;;; Error conditions
(define-condition abort-error (error)
  ((reason :initarg :reason :reader abort-reason)
   (context :initarg :context :reader abort-context :initform nil)
   (suggestions :initarg :suggestions :reader abort-suggestions :initform nil))
  (:report (lambda (condition stream)
             (format stream "Mission Abort: ~A (reason: ~A)"
                     (call-next-method)
                     (abort-reason condition)))))

;;; Abort strategy structure
(defstruct abort-strategy
  "Strategy for handling mission aborts"
  (conditions (make-default-abort-conditions))
  (escalation-chain '())
  (recovery-options (make-default-recovery-options))
  (history '())
  (metrics (make-abort-metrics)))

(defstruct abort-metrics
  "Metrics for abort handling"
  (total-requests 0)
  (aborted-requests 0)
  (escalated-requests 0)
  (recovered-requests 0))

;;; Default abort conditions
(defun make-default-abort-conditions ()
  "Create default set of abort conditions"
  (list
   (list :name :insufficient-context
         :check (lambda (response context)
                  (declare (ignore context))
                  (and (stringp response)
                       (or (search "insufficient" (string-downcase response))
                           (search "need more" (string-downcase response))
                           (search "cannot understand" (string-downcase response))))))
   
   (list :name :capability-limit
         :check (lambda (response context)
                  (declare (ignore context))
                  (and (stringp response)
                       (or (search "cannot perform" (string-downcase response))
                           (search "unable to complete" (string-downcase response))
                           (search "beyond capabilities" (string-downcase response))))))
   
   (list :name :hallucination-detected
         :check (lambda (response context)
                  (declare (ignore context))
                  (and (stringp response)
                       (or (search "[hallucination]" (string-downcase response))
                           (search "made up" (string-downcase response))
                           (search "fictional" (string-downcase response))))))
   
   (list :name :confidence-threshold
         :check (lambda (response context)
                  (declare (ignore context))
                  (and (consp response)
                       (getf response :confidence)
                       (< (getf response :confidence) 0.7))))
   
   (list :name :repetitive-failure
         :check (lambda (response context)
                  (let ((strategy (getf context :strategy)))
                    (when strategy
                      (let ((recent-errors (subseq (abort-strategy-history strategy) 0 
                                                  (min 3 (length (abort-strategy-history strategy))))))
                        (>= (count response recent-errors :test #'string= :key #'second) 2))))))))

;;; Default recovery options
(defun make-default-recovery-options ()
  "Create default recovery options"
  (list
   (list :name :prompt-enhancement
         :applicable (lambda (error) (eq (abort-reason error) :insufficient-context))
         :action (lambda (request error)
                   (list :type :retry
                         :request (enhance-prompt-with-context request (abort-context error)))))
   
   (list :name :decomposition
         :applicable (lambda (error) (eq (abort-reason error) :capability-limit))
         :action (lambda (request error)
                   (declare (ignore error))
                   (list :type :subtasks
                         :tasks (decompose-request request))))
   
   (list :name :validation-prompt
         :applicable (lambda (error) (eq (abort-reason error) :hallucination-detected))
         :action (lambda (request error)
                   (declare (ignore error))
                   (list :type :validate
                         :request (create-validation-prompt request))))))

;;; Main functions
(defun make-abort-strategy ()
  "Create a new abort strategy instance"
  (make-abort-strategy))

(defun should-abort-p (response &key strategy context)
  "Check if response should trigger an abort"
  (let ((conditions (if strategy
                        (abort-strategy-conditions strategy)
                        (make-default-abort-conditions))))
    (some (lambda (condition)
            (funcall (getf condition :check) response 
                    (append context (when strategy (list :strategy strategy)))))
          conditions)))

(defun execute-with-abort-handling (strategy provider request &key options)
  "Execute request with abort handling"
  (let ((response nil)
        (abort-error nil))
    
    (incf (abort-metrics-total-requests (abort-strategy-metrics strategy)))
    
    (handler-case
        (progn
          (setf response (funcall provider request))
          
          (when (should-abort-p response :strategy strategy :context (list :provider provider :request request))
            (setf abort-error (create-abort-error response provider request :insufficient-context))
            (handle-abort strategy abort-error provider request options)))
        
      (error (e)
        (setf abort-error (create-abort-error e provider request :error))
        (handle-abort strategy abort-error provider request options)))
    
    (or response
        (error 'abort-error
               :reason :execution-failed
               :context (list :provider provider :request request)))))

(defun handle-abort (strategy abort-error provider request options)
  "Handle abort situation with escalation and recovery"
  (incf (abort-metrics-aborted-requests (abort-strategy-metrics strategy)))
  
  ;; Add to history
  (push (list (current-time-string) (abort-reason abort-error) request)
        (abort-strategy-history strategy))
  
  ;; Try escalation chain
  (let ((escalation-result (try-escalation-chain strategy request provider abort-error)))
    (when escalation-result
      (incf (abort-metrics-escalated-requests (abort-strategy-metrics strategy)))
      (return-from handle-abort escalation-result)))
  
  ;; Try recovery options
  (let ((recovery-result (try-recovery-options strategy abort-error request options)))
    (when recovery-result
      (incf (abort-metrics-recovered-requests (abort-strategy-metrics strategy)))
      (return-from handle-abort recovery-result)))
  
  ;; If all else fails, signal enhanced error
  (error 'abort-error
         :reason (abort-reason abort-error)
         :context (abort-context abort-error)
         :suggestions (generate-suggestions (abort-reason abort-error))))

(defun add-escalation-provider (strategy provider &key priority conditions)
  "Add provider to escalation chain"
  (let ((escalation-entry (list :provider provider
                               :priority (or priority 0)
                               :conditions (or conditions '())
                               :attempts 0
                               :failures '())))
    (push escalation-entry (abort-strategy-escalation-chain strategy))
    
    ;; Sort by priority (higher first)
    (setf (abort-strategy-escalation-chain strategy)
          (sort (abort-strategy-escalation-chain strategy)
                (lambda (a b) (> (getf a :priority) (getf b :priority)))))))

;;; Helper functions
(defun create-abort-error (response-or-error provider request reason)
  "Create an abort error from response or exception"
  (let ((message (if (typep response-or-error 'error)
                     (format nil "Provider ~A failed: ~A" provider response-or-error)
                     (format nil "Provider ~A cannot handle request" provider)))
        (context (list :provider provider :request request)))
    
    (if (typep response-or-error 'error)
        (make-condition 'abort-error
                       :reason reason
                       :context context)
        (make-condition 'abort-error
                       :reason (determine-abort-reason response-or-error)
                       :context (append context (list :response response-or-error))))))

(defun determine-abort-reason (content)
  "Determine abort reason from content"
  (let ((content-str (string-downcase (format nil "~A" content))))
    (cond
      ((or (search "insufficient" content-str)
           (search "need more" content-str)
           (search "context" content-str))
       :insufficient-context)
      ((or (search "cannot" content-str)
           (search "unable" content-str)
           (search "beyond" content-str))
       :capability-limit)
      ((or (search "hallucin" content-str)
           (search "made up" content-str)
           (search "fictional" content-str))
       :hallucination-detected)
      ((or (search "timeout" content-str)
           (search "timed out" content-str))
       :timeout)
      ((or (search "rate limit" content-str)
           (search "too many" content-str))
       :rate-limit)
      (t :unknown))))

(defun generate-suggestions (reason)
  "Generate helpful suggestions based on abort reason"
  (case reason
    (:insufficient-context
     '("Provide more context or background information"
       "Break down the request into smaller, specific questions"
       "Use a more capable model or provider"))
    (:capability-limit
     '("Simplify the request"
       "Use a specialized provider for this task"
       "Consider using tools or plugins for enhanced capabilities"))
    (:hallucination-detected
     '("Verify information with reliable sources"
       "Use a model with better factual accuracy"
       "Add validation steps to the workflow"))
    (:timeout
     '("Reduce request complexity"
       "Increase timeout settings"
       "Check network connectivity"))
    (:rate-limit
     '("Wait before retrying"
       "Use a different provider temporarily"
       "Upgrade API plan for higher limits"))
    (t
     '("Try a different provider"
       "Reformulate the request"
       "Check provider status and availability"))))

(defun try-escalation-chain (strategy request failed-provider abort-error)
  "Try providers in escalation chain"
  (dolist (escalation (abort-strategy-escalation-chain strategy))
    (let ((provider (getf escalation :provider)))
      ;; Skip failed provider and providers that have exceeded attempts
      (unless (or (eq provider failed-provider)
                  (>= (getf escalation :attempts) 3))
        
        ;; Check if conditions are met
        (when (conditions-met-p (getf escalation :conditions) abort-error)
          (handler-case
              (progn
                (incf (getf escalation :attempts))
                (let ((enhanced-request (enhance-request-for-escalation request abort-error)))
                  (funcall provider enhanced-request)))
            (error (e)
              (push e (getf escalation :failures))))))))
  nil)

(defun try-recovery-options (strategy abort-error request options)
  "Try recovery options for the error"
  (dolist (recovery (abort-strategy-recovery-options strategy))
    (when (funcall (getf recovery :applicable) abort-error)
      (let ((result (funcall (getf recovery :action) request abort-error)))
        (case (getf result :type)
          (:retry
           (when (getf options :retry-handler)
             (return-from try-recovery-options
               (funcall (getf options :retry-handler) (getf result :request)))))
          (:subtasks
           (when (getf options :subtask-handler)
             (return-from try-recovery-options
               (funcall (getf options :subtask-handler) (getf result :tasks)))))
          (:validate
           (when (getf options :validation-handler)
             (return-from try-recovery-options
               (funcall (getf options :validation-handler) (getf result :request)))))))))
  nil)

(defun conditions-met-p (conditions abort-error)
  "Check if escalation conditions are met"
  (or (null conditions)
      (every (lambda (condition-pair)
               (destructuring-bind (key value) condition-pair
                 (case key
                   (:error-types (member (abort-reason abort-error) value))
                   (:min-priority t) ; Would check against priority system
                   (t t))))
             (loop for (key value) on conditions by #'cddr
                   collect (list key value)))))

(defun enhance-prompt-with-context (request context)
  "Enhance prompt with additional context"
  (if (listp request)
      (append request (list (list :role "system"
                                 :content (format nil "Additional context: ~A" context))))
      (format nil "~A~%~%Additional context: ~A" request context)))

(defun decompose-request (request)
  "Decompose complex request into subtasks"
  ;; Simplified decomposition - could be enhanced with AI
  (list
   (list :task "Understand the main objective" :request request)
   (list :task "Identify required steps" :request request)
   (list :task "Execute each step" :request request)))

(defun create-validation-prompt (request)
  "Create validation prompt to check accuracy"
  (if (listp request)
      (append request (list (list :role "system"
                                 :content "Please validate the previous response for accuracy and cite sources if possible.")))
      (format nil "~A~%~%Please ensure accuracy and avoid speculation." request)))

(defun enhance-request-for-escalation (request abort-error)
  "Enhance request for escalation with error context"
  (let ((error-context (format nil "Previous attempt failed with: ~A" abort-error)))
    (if (listp request)
        (cons (list :role "system" :content error-context) request)
        (format nil "~A~%~%Request: ~A" error-context request))))

;;; Utility functions for metrics and reporting
(defun get-abort-metrics (strategy)
  "Get current abort metrics"
  (let ((metrics (abort-strategy-metrics strategy)))
    (list :total-requests (abort-metrics-total-requests metrics)
          :aborted-requests (abort-metrics-aborted-requests metrics)
          :escalated-requests (abort-metrics-escalated-requests metrics)
          :recovered-requests (abort-metrics-recovered-requests metrics)
          :abort-rate (if (> (abort-metrics-total-requests metrics) 0)
                          (/ (abort-metrics-aborted-requests metrics)
                             (abort-metrics-total-requests metrics))
                          0))))

(defun reset-abort-metrics (strategy)
  "Reset abort metrics"
  (setf (abort-strategy-metrics strategy) (make-abort-metrics)))

(defun print-abort-summary (strategy)
  "Print summary of abort handling"
  (let ((metrics (get-abort-metrics strategy)))
    (format t "~%Abort Strategy Summary:~%")
    (format t "- Total requests: ~A~%" (getf metrics :total-requests))
    (format t "- Aborted requests: ~A~%" (getf metrics :aborted-requests))
    (format t "- Escalated requests: ~A~%" (getf metrics :escalated-requests))
    (format t "- Recovered requests: ~A~%" (getf metrics :recovered-requests))
    (format t "- Abort rate: ~,2F%~%" (* (getf metrics :abort-rate) 100))
    (format t "- Recent history: ~A abort(s)~%" (length (abort-strategy-history strategy)))))