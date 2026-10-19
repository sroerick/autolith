(in-package #:autolith)

;;;; -- Agent and Provider Lifecycles --

(defvar *telemetry-provider-attempt-count* nil
  "The count of per-attempt provider hooks inside the current request.")

(defvar *telemetry-response-model* nil
  "The response model observed in the current provider attempt, never a requested-model fallback.")

(defvar *telemetry-finish-reason* nil
  "The public finish reason observed in the current provider attempt.")

(defvar *telemetry-cache-write-tokens* :unobserved
  "Reported cache writes, NIL for an observed omission, or :UNOBSERVED without wire usage.")

(-> telemetry--cache-write-tokens (model-provider t) (option integer))
(defun telemetry--cache-write-tokens (provider usage)
  "Read creation counters, ignoring ChatGPT's structural zero for unsupported writes."
  (labels ((field (object name)
             (let ((value (and (hash-table-p object) (json-get object name))))
               (and (typep value '(integer 0))
                    (or (plusp value) (not (eq (provider-family provider) ':codex)))
                    value))))
    (or (field usage "cache_creation_input_tokens")
        (field usage "cache_write_tokens")
        (field usage "cache_write_input_tokens")
        (field usage "cacheWriteInputTokens")
        (field (and (hash-table-p usage) (json-get usage "input_tokens_details")) "cache_write_tokens")
        (field (and (hash-table-p usage) (json-get usage "prompt_tokens_details")) "cache_write_tokens"))))

(-> telemetry--stream-cache-write-tokens (model-provider t) null)
(defun telemetry--stream-cache-write-tokens (provider event)
  "Observe raw usage so provider normalization cannot turn an omitted counter into zero."
  (handler-case
      (let ((usage
              (case (provider-wire-protocol provider)
                (:responses-api (json-get (json-get event "response") "usage"))
                (:chat-completions (json-get event "usage"))
                (:anthropic-messages
                 (when (equal (json-get event "type") "message_start")
                   (json-get (json-get event "message") "usage")))
                (:gemini-generate-content
                 (json-get (cl-llm-provider-api:provider-gemini-stream-response provider event)
                           "usageMetadata")))))
        (when (hash-table-p usage)
          (setf *telemetry-cache-write-tokens* (telemetry--cache-write-tokens provider usage))))
    (error () nil))
  nil)

(defun telemetry--stream-finish-reason (provider event)
  "Read terminal protocol metadata without retaining generated content."
  (handler-case
      (case (provider-wire-protocol provider)
        (:responses-api
         (let ((response (json-get event "response")))
           (cond ((equal (json-get event "type") "response.completed") "completed")
                 ((equal (json-get event "type") "response.incomplete")
                  (json-get (json-get response "incomplete_details") "reason"))
                 ((equal (json-get event "type") "response.failed") "failed"))))
        (:chat-completions
         (let ((choices (json-get event "choices")))
           (when (and (vectorp choices) (plusp (length choices)))
             (json-get (aref choices 0) "finish_reason"))))
        (:anthropic-messages
         (json-get (json-get event "delta") "stop_reason"))
        (:gemini-generate-content
         (let ((candidates
                 (json-get (cl-llm-provider-api:provider-gemini-stream-response provider event)
                           "candidates")))
           (when (and (vectorp candidates) (plusp (length candidates)))
             (json-get (aref candidates 0) "finishReason")))))
    (error () nil)))

(-> telemetry--response-model (model-provider t) (option string))
(defun telemetry--response-model (provider event)
  "Read only the model field of a known protocol envelope, ignoring generated output."
  (handler-case
      (let ((model
              (when (hash-table-p event)
                (case (provider-wire-protocol provider)
                  (:responses-api
                   (when (member (json-get event "type")
                                 '("response.created" "response.completed") :test #'equal)
                     (json-get (json-get event "response") "model")))
                  (:chat-completions (json-get event "model"))
                  (:anthropic-messages
                   (when (equal (json-get event "type") "message_start")
                     (json-get (json-get event "message") "model")))
                  (:gemini-generate-content
                   (json-get (cl-llm-provider-api:provider-gemini-stream-response provider event)
                             "modelVersion"))))))
        (when (and (stringp model) (<= 1 (length model) 256)) model))
    (error () nil)))

(defmethod provider-consume-stream :around
    ((provider model-provider) stream headers event-callback)
  "Observe bounded SSE model metadata until the provider API exposes it in results.
The provider's original parser remains authoritative. No stream data is retained
after its event boundary, and disabled telemetry never installs an observer."
  (declare (ignore headers event-callback))
  (if (null *telemetry-run*)
      (call-next-method)
      (let ((reader *sse-read-line-function*)
            (lines nil)
            (size 0)
            (discard-p nil))
        (labels ((finish-event ()
                   (unless discard-p
                     (when lines
                       (handler-case
                           (let* ((event (json-decode (format nil "~{~A~^~%~}" (nreverse lines))))
                                  (model (telemetry--response-model provider event))
                                  (finish (telemetry--stream-finish-reason provider event)))
                             (telemetry--stream-cache-write-tokens provider event)
                             (when model (setf *telemetry-response-model* model))
                             (when (stringp finish) (setf *telemetry-finish-reason* finish)))
                         (error () nil))))
                   (setf lines nil size 0 discard-p nil)))
          (let ((*sse-read-line-function*
                  (lambda (input)
                    (let ((raw (funcall reader input)))
                      (when (eq input stream)
                        (cond
                          ((eq raw *sse-end-of-stream*) (finish-event))
                          ((stringp raw)
                           (let ((line (string-right-trim '(#\Return) raw)))
                             (cond
                               ((zerop (length line)) (finish-event))
                               ((and (>= (length line) 5) (string= line "data:" :end1 5))
                                (let ((data (subseq line (if (and (> (length line) 5)
                                                                 (char= (char line 5) #\Space)) 6 5))))
                                  (incf size (+ (length data) (if lines 1 0)))
                                  (if (> size *sse-maximum-event-characters*)
                                      (setf discard-p t lines nil)
                                      (unless discard-p (push data lines))))))))))
                      raw))))
            (call-next-method))))))

(defmethod agent-run-user-turn :around
    ((agent agent) (content user-message-input) &rest arguments &key &allow-other-keys)
  "Measure one logical agent turn, including failed and cancelled turns."
  (declare (ignore content arguments))
  (telemetry-call-with-run (agent-configuration agent)
                           (lambda () (call-next-method))))

(-> telemetry--call-with-provider-run (model-provider function) t)
(defun telemetry--call-with-provider-run (provider function)
  "Run standalone requests with telemetry only when a provider has valid configuration."
  (if *telemetry-run*
      (funcall function)
      (let ((configuration
              (telemetry--call-safely (lambda () (provider-configuration provider)))))
        (if (typep configuration 'configuration)
            (telemetry-call-with-run configuration function)
            (funcall function)))))

(-> telemetry--provider-name (model-provider) keyword)
(defun telemetry--provider-name (provider)
  "Map a local provider family to a fixed public telemetry category."
  (case (provider-family provider)
    ((:codex :openai :openai-chat :openai-responses) ':openai)
    (:anthropic ':anthropic)
    ((:gemini :gemini-code-assist :google) ':google)
    ((:grok :xai) ':xai)
    (:ollama ':ollama)
    (:deepseek ':deepseek)
    (:mistral ':mistral-ai)
    (:cohere ':cohere)
    (:local ':local)
    (otherwise ':other)))

(defun telemetry--condition-error-type (condition)
  "Classify failures without exporting reports, server bodies, or arbitrary class names."
  (cond ((typep condition 'application-turn-cancelled) ':cancelled)
        ((typep condition 'job-aborted)
         (if (eq (job-aborted-reason condition) ':timeout) ':timeout ':cancelled))
        ((typep condition 'authentication-error) ':authentication)
        ((typep condition 'autolith-control-condition) ':cancelled)
        ((typep condition '(or provider-transport-error stream-error)) ':transport)
        ((typep condition 'cl-llm-provider-api:provider-error)
         (case (cl-llm-provider-api:provider-error-status condition)
           ((401 403) ':authentication)
           (429 ':rate-limit)
           (otherwise ':provider)))
        ((typep condition 'agent-loop-error) ':protocol)
        ((or (typep condition 'tool-authority-denied)
             (typep condition 'command-authorization-unavailable)
             (telemetry--permission-error-code-p (tool-failure-code condition)))
         ':permission)
        ((typep condition 'tool-error) ':tool)
        (t ':internal)))

(-> telemetry--provider-attempt
    (model-provider function &key (:native-compaction-p boolean) (:fallback-p boolean)) t)
(defun telemetry--provider-attempt (provider function &key native-compaction-p fallback-p)
  "Record terminal outcomes of real attempts, preserving values and nonlocal exits."
  (when (and (not fallback-p) *telemetry-provider-attempt-count*)
    (incf *telemetry-provider-attempt-count*))
  (let ((*telemetry-response-model* nil)
        (*telemetry-finish-reason* nil)
        (*telemetry-cache-write-tokens* :unobserved)
        (start (get-internal-real-time))
        (outcome ':cancelled)
        (error-type nil)
        (results nil))
    (unwind-protect
         (handler-bind ((serious-condition
                         (lambda (condition)
                           (when (typep condition 'provider-result-rejected)
                             (setf results (list (provider-result-rejected-result condition))))
                           (setf error-type (telemetry--condition-error-type condition)
                                 outcome (telemetry--outcome :error-type error-type)))))
           (setf results (multiple-value-list (funcall function))
                 outcome ':success error-type nil)
           (values-list results))
      (when (or (not fallback-p) (zerop *telemetry-provider-attempt-count*))
        (telemetry--note-provider-result
         provider start results :native-compaction-p native-compaction-p
         :outcome outcome :error-type error-type)))))

(defmethod provider-attempt-turn :around
    ((provider model-provider) (conversation conversation)
     &rest arguments &key &allow-other-keys)
  "Count authentication recovery and transport retries as separate model attempts."
  (declare (ignore conversation arguments))
  (telemetry--provider-attempt provider (lambda () (call-next-method))))

(defmethod provider-attempt-native-compaction :around
    ((provider model-provider) (conversation conversation)
     &rest arguments &key &allow-other-keys)
  "Measure native compaction attempts without exporting checkpoint material."
  (declare (ignore conversation arguments))
  (telemetry--provider-attempt provider (lambda () (call-next-method))
                              :native-compaction-p t))

;; Mission admission specializes CONVERSATION on its class. Use T here so these
;; wrappers compose with that accounting method instead of replacing it.
(defmethod provider-stream-turn :around
    ((provider model-provider) (conversation t)
     &rest arguments &key &allow-other-keys)
  "Cover standalone recursive requests and providers without a per-attempt protocol."
  (declare (ignore conversation arguments))
  (telemetry--call-with-provider-run
   provider
   (lambda ()
     (let ((*telemetry-provider-attempt-count* 0))
       (telemetry--provider-attempt provider (lambda () (call-next-method)) :fallback-p t)))))

(defmethod provider-native-compact-conversation :around
    ((provider model-provider) (conversation t)
     &rest arguments &key &allow-other-keys)
  "Cover native compaction outside agent turns, preserving all returned values."
  (declare (ignore conversation arguments))
  (telemetry--call-with-provider-run
   provider
   (lambda ()
     (let ((*telemetry-provider-attempt-count* 0))
       (telemetry--provider-attempt provider (lambda () (call-next-method))
                                   :native-compaction-p t :fallback-p t)))))

(-> telemetry--note-provider-result
    (model-provider integer list &key (:native-compaction-p boolean)
     (:outcome keyword) (:error-type (option keyword))) null)
(defun telemetry--note-provider-result (provider start results &key native-compaction-p outcome error-type)
  "Emit a usage-only event for an attempt, even if it failed before returning."
  (when *telemetry-run*
    (telemetry--call-safely
     (lambda ()
       (let* ((result (first results))
              (usage (telemetry--call-safely
                      (lambda ()
                        (if native-compaction-p
                            (second results)
                            (when (typep result 'provider-result)
                              (provider-result-usage result))))))
              (normalized (when usage
                            (telemetry--call-safely
                             (lambda () (provider-usage-normalize usage))))))
         (when (hash-table-p normalized)
           (let ((writes (if (eq *telemetry-cache-write-tokens* ':unobserved)
                             (telemetry--cache-write-tokens provider usage)
                             *telemetry-cache-write-tokens*)))
             (remhash "cache_creation_input_tokens" normalized)
             (when writes
               (setf (gethash "cache_creation_input_tokens" normalized) writes))))
         (telemetry-note-model
          :run *telemetry-run*
          :provider (telemetry--call-safely (lambda () (telemetry--provider-name provider)))
          :request-model (telemetry--call-safely
                          (lambda ()
                            (config :model (provider-configuration provider))))
          :response-model *telemetry-response-model*
          :outcome outcome :error-type error-type
          :role (if native-compaction-p ':compaction *telemetry-request-role*)
          :finish-reason (or *telemetry-finish-reason*
                             (when (typep result 'provider-result)
                               (if (provider-result-tool-calls result) ':tool-calls
                                   (case (provider-result-turn-completion result)
                                     (:end ':stop) (:continue ':continue)))))
          :usage normalized
          :duration-ms (telemetry--elapsed-milliseconds start))))))
  nil)

;;;; -- Process Configuration Ownership --

(defmethod (setf application-configuration) :around
    ((configuration configuration) (application application))
  "Retire telemetry consent when the owning application's configuration changes."
  (let* ((previous (when (slot-boundp application 'configuration)
                     (application-configuration application)))
         (replace-owner-p (and *telemetry-controller*
                               (eq previous (telemetry-controller-owner *telemetry-controller*))
                               (not (eq previous configuration)))))
    (multiple-value-prog1 (call-next-method)
      (when replace-owner-p
        (telemetry--call-safely
         (lambda ()
           (telemetry-shutdown :drain-p nil)
           (telemetry-attach configuration)))))))

;;;; -- Checkpoint State --

(defmethod checkpoint-detach-state :before ((application application))
  "Remove queued telemetry and foreign/process state from a saver child's heap."
  (declare (ignore application))
  (telemetry-detach))
