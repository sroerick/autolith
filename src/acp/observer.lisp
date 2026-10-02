(in-package #:autolith)

;;;; -- ACP Presentation --

(defvar *acp-current-tool-call-id* nil
  "The provider call identifier bound within one ACP tool execution.")

(defparameter *acp-permission-timeout-seconds* 120
  "Maximum wait for an editor permission response.")

(defclass acp-observer (agent-observer)
  ((session :initarg :session :reader acp-observer-session :type acp-session
            :documentation "The session receiving this turn's updates.")
   (turn-sequence :initarg :turn-sequence :accessor acp-observer-turn-sequence
                  :documentation "The durable user-message sequence qualifying tool identifiers.")
   (streamed-text :initform (text-buffer-create) :accessor acp-observer-streamed-text :type string
                  :documentation "Text already emitted for the current provider response.")
   (lock :initform (make-lock "Autolith ACP updates") :reader acp-observer-lock
         :documentation "The lock serializing short presentation updates, never permission waits."))
  (:documentation "An incremental ACP presentation sink with editor authorization."))

(-> acp-observer-create (acp-session) acp-observer)
(defun acp-observer-create (session)
  "Create a fresh presentation sink for SESSION's next user turn."
  (make-instance 'acp-observer :session session
                 :turn-sequence (conversation-next-sequence
                                 (application-conversation (acp-session-application session)))))

(-> acp-observer--send (acp-observer hash-table) null)
(defun acp-observer--send (observer update)
  "Send one update, including terminal tool outcomes during cancellation."
  (let ((session (acp-observer-session observer)))
    (unless (acp-service-closed-p (acp-session-service session))
      (agentcomms:agent-send-update
       (acp-session-service session) (acp-session-identifier session) update)))
  nil)

(-> acp-tool-identifier (integer string) string)
(defun acp-tool-identifier (turn-sequence identifier)
  "Qualify IDENTIFIER by its durable turn, including during session replay."
  (format nil "~D:~A" turn-sequence identifier))

(-> acp-tool-kind (string) keyword)
(defun acp-tool-kind (name)
  "Map a native tool's operation to an ACP presentation category."
  (cond
    ((string= name "resource.read") ':read)
    ((string= name "resource.edit") ':edit)
    ((string= name "shell.run") ':execute)
    ((uiop:string-prefix-p "search." name) ':search)
    ((string= name "web.run") ':search)
    ((string= name "web_extra.gist") ':fetch)
    ((string= name "plan.update") ':think)
    (t
     ':other)))

(-> acp-observer--arguments (acp-observer string) hash-table)
(defun acp-observer--arguments (observer identifier)
  "Return the persisted provider arguments of IDENTIFIER as a JSON object."
  (let* ((application (acp-session-application (acp-observer-session observer)))
         (call (find identifier (reverse (conversation-input-items
                                          (application-conversation application)))
                     :key (lambda (item) (json-get item "call_id")) :test #'equal)))
    (handler-case (json-decode (or (and call (json-get call "arguments")) "{}"))
      (error ()
        (json-object)))))

(-> acp-observer--tool-report
    (acp-observer &key (:identifier string) (:title string) (:status keyword)
                  (:arguments t) (:output (option string))) hash-table)
(defun acp-observer--tool-report (observer &key identifier title status arguments output)
  "Construct a tool report using the same identity for progress and authorization."
  (agentcomms:acp-tool-call
   (acp-tool-identifier (acp-observer-turn-sequence observer) identifier)
   title :name title :kind (acp-tool-kind title) :status status
   :raw-input arguments :raw-output output
   :content (when output
              (list (agentcomms:acp-tool-call-content (agentcomms:acp-text-content output))))))

(defmethod agent-observer-text ((observer acp-observer) text)
  "Stream assistant text and retain its length for nonstreamed response reconciliation."
  (acp-session-check-cancelled (acp-observer-session observer))
  (with-lock-held ((acp-observer-lock observer))
    (text-buffer-append (acp-observer-streamed-text observer) text)
    (acp-observer--send observer
                        (agentcomms:acp-update-agent-message (agentcomms:acp-text-content text))))
  nil)

(defmethod agent-observer-reasoning ((observer acp-observer) text)
  "Stream visible reasoning, never private provider reasoning tokens."
  (acp-session-check-cancelled (acp-observer-session observer))
  (acp-observer--send observer
                      (agentcomms:acp-update-agent-thought (agentcomms:acp-text-content text))))

(-> acp-observer--plan (acp-observer) null)
(defun acp-observer--plan (observer)
  "Project the durable workspace plan after its native publication."
  (let* ((session (acp-observer-session observer))
         (plan (plan-load (application-configuration (acp-session-application session)))))
    (acp-observer--send
     observer
     (agentcomms:acp-update-plan
      (when plan
        (mapcar (lambda (step)
                  (agentcomms:acp-plan-entry
                   (plan-step-text step)
                   :status (ecase (plan-step-status step)
                             (:pending ':pending)
                             (:doing ':in-progress)
                             (:done ':completed))))
                (workspace-plan-steps plan))))))
  nil)

(defmethod agent-observer-status ((observer acp-observer) status details)
  "Translate provider and durable tool lifecycle events to ACP updates."
  ;; Completion events must be sent before the cancelled prompt response.
  (unless (eq status ':tool-call-completed)
    (acp-session-check-cancelled (acp-observer-session observer)))
  (with-lock-held ((acp-observer-lock observer))
    (case status
      (:user-message-persisted
       (setf (acp-observer-turn-sequence observer) (getf details :sequence)))
      (:provider-request-started
       (text-buffer-clear (acp-observer-streamed-text observer)))
      (:assistant-response-persisted
       (let ((text (getf details :text)) (streamed (acp-observer-streamed-text observer)))
         (when (and (stringp text) (uiop:string-prefix-p streamed text)
                    (< (length streamed) (length text)))
           (acp-observer--send
            observer (agentcomms:acp-update-agent-message
                      (agentcomms:acp-text-content (subseq text (length streamed))))))))
      (:tool-call-started
       (acp-observer--send
        observer
        (agentcomms:acp-update-tool-call
         (acp-observer--tool-report
          observer :identifier (getf details :call-id) :title (getf details :tool)
          :status ':in-progress
          :arguments (acp-observer--arguments observer (getf details :call-id))))))
      (:tool-call-completed
       (let ((identifier (acp-tool-identifier (acp-observer-turn-sequence observer)
                                              (getf details :call-id)))
             (output (getf details :output)))
         (acp-observer--send
          observer
          (agentcomms:acp-update-tool-call-progress
           (agentcomms:acp-tool-call-update
            identifier :status (if (getf details :success-p) ':completed ':failed)
            :raw-output output
            :content (list (agentcomms:acp-tool-call-content
                            (agentcomms:acp-text-content output))))))
         (when (string= (getf details :tool) "plan.update")
           (acp-observer--plan observer))))))
  nil)

(-> acp-observer--permission (acp-observer hash-table list) (option string))
(defun acp-observer--permission (observer report options)
  "Request a bounded editor decision and accept only an offered option."
  (let ((session (acp-observer-session observer)))
    (acp-session-check-cancelled session)
    (let ((choice
           (handler-case
               (multiple-value-bind (outcome identifier)
                   (agentcomms:agent-request-permission
                    (acp-session-service session) (acp-session-identifier session)
                    report options :timeout *acp-permission-timeout-seconds*)
                 (when (and (eq outcome ':selected)
                            (find identifier options :test #'equal
                                  :key (lambda (option) (agentcomms:json-get option "optionId"))))
                   identifier))
             (agentcomms:acp-error ()
               nil))))
      (acp-session-check-cancelled session)
      choice)))

(-> acp-observer--approval (acp-observer string hash-table t) boolean)
(defun acp-observer--approval (observer title arguments key)
  "Ask for exact operation approval, retaining allow-always only for this session."
  (let* ((session (acp-observer-session observer))
         (saved (with-lock-held ((acp-session-lock session))
                  (gethash key (acp-session-permissions session)))))
    (acp-session-check-cancelled session)
    (or saved
        (let ((choice
               (acp-observer--permission
                observer
                (acp-observer--tool-report
                 observer :identifier (or *acp-current-tool-call-id* (make-identifier))
                 :title title :status ':pending :arguments arguments)
                (list (agentcomms:acp-permission-option "once" "Allow once" ':allow-once)
                      (agentcomms:acp-permission-option "session" "Allow exact operation this session" ':allow-always)
                      (agentcomms:acp-permission-option "deny" "Reject" ':reject-once)))))
          (when (equal choice "session")
            (with-lock-held ((acp-session-lock session))
              (setf (gethash key (acp-session-permissions session)) t)))
          (and (member choice '("once" "session") :test #'equal) t)))))

(defmethod agent-observer-authorize-command ((observer acp-observer) command directory)
  "Apply native process policy with editor approval in ask mode."
  (let* ((session (acp-observer-session observer))
         (application (acp-session-application session)))
    (acp-session-check-cancelled session)
    (case (acp-session-mode session)
      (:full-access
       ':full-access)
      (:sandboxed
       (if (application--command-sandbox-available-p) ':sandboxed ':deny))
      (:auto
       (application--automatic-command-decision
        (nth-value 0
                   (permissions-model-classify-command
                    command directory :provider (application-provider application)
                    :configuration (application-configuration application)
                    :sandbox-available-p (application--command-sandbox-available-p)))))
      (otherwise
       (if (acp-observer--approval observer "shell.run"
                                   (json-object "command" command "directory" (namestring directory))
                                   (list ':command command (namestring directory)))
           ':full-access
           ':deny)))))

(defmethod agent-observer-authorize-tool ((observer acp-observer) tool arguments)
  "Authorize an external tool through the editor using its exact identity and input."
  (let* ((session (acp-observer-session observer))
         (name (tool-canonical-name tool)))
    (acp-session-check-cancelled session)
    (if (or (eq (acp-session-mode session) ':full-access)
            (acp-observer--approval observer name arguments
                                    (list ':tool (tool-authorization-identity-fields tool)
                                          (agent--tool-signature-value arguments))))
        ':allow
        ':deny)))

(defmethod agent-observer-call-with-tool-execution ((observer acp-observer) identifier function)
  "Own tool workers and propagate ACP session bindings and permission identity."
  (let ((*acp-current-tool-call-id* identifier))
    (acp-session--call-with-tool (acp-observer-session observer) function)))
