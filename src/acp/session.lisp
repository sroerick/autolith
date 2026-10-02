(in-package #:autolith)

;;;; -- ACP Runtime Ownership --

(defparameter *acp-session-maximum-count* 32
  "Maximum live editor sessions in one ACP process.")

(defparameter *acp-session-close-seconds* 10
  "Maximum cooperative shutdown wait before reporting an active session.")

(defclass acp-service (agentcomms:acp-agent)
  ((configuration
    :initarg :configuration :accessor acp-service-configuration :type configuration
    :documentation "The bootstrapped process configuration copied for each session.")
   (permission-mode
    :initarg :permission-mode :initform ':ask :reader acp-service-permission-mode
    :type (member :ask :auto :sandboxed :full-access)
    :documentation "The initial permission mode of new and loaded sessions.")
   (sessions
    :initform (make-hash-table :test #'equal) :reader acp-service-sessions
    :documentation "Live sessions keyed by canonical durable conversation ID.")
   (lock
    :initform (make-lock "Autolith ACP service") :reader acp-service-lock
    :documentation "The lock protecting session admission and process ownership.")
   (busy-owner
    :initform nil :accessor acp-service-busy-owner
    :documentation "The session or setup operation owning the primary runtime.")
   (closed-p
    :initform nil :accessor acp-service-closed-p :type boolean
    :documentation "Whether disconnect has stopped further session admission."))
  (:documentation "A headless ACP endpoint over the existing Autolith agent runtime."))

(defclass acp-session ()
  ((service
    :initarg :service :reader acp-session-service :type acp-service
    :documentation "The process endpoint owning this session.")
   (identifier
    :initarg :identifier :reader acp-session-identifier :type string
    :documentation "The canonical durable conversation identifier.")
   (application
    :initarg :application :reader acp-session-application :type application
    :documentation "The headless application owning its agent, tools, workers, and lease.")
   (mode
    :initarg :mode :initform ':ask :accessor acp-session-mode
    :documentation "The session's current permission policy.")
   (permissions
    :initform (make-hash-table :test #'equal) :reader acp-session-permissions
    :documentation "Exact editor-approved operations, scoped to this live session.")
   (lock
    :initform (make-lock "Autolith ACP session") :reader acp-session-lock
    :documentation "The lock protecting cancellation, ownership, and approvals.")
   (cleanup-lock
    :initform (make-lock "Autolith ACP session cleanup") :reader acp-session-cleanup-lock
    :documentation "The lock serializing complete close attempts.")
   (closing-p
    :initform nil :accessor acp-session-closing-p :type boolean
    :documentation "Whether session shutdown has stopped prompt admission.")
   (epoch
    :initform 0 :accessor acp-session-epoch :type integer
    :documentation "The prompt generation used to reject delayed interrupts.")
   (prompt-thread
    :initform nil :accessor acp-session-prompt-thread
    :documentation "The active prompt owner, or NIL between turns.")
   (prompt-interruptible-p
    :initform nil :accessor acp-session-prompt-interruptible-p :type boolean
    :documentation "Whether cancellation may interrupt the owner before durable finalization.")
   (tool-threads
    :initform (make-hash-table :test #'eq) :reader acp-session-tool-threads
    :documentation "Tool workers admitted within the current prompt generation.")
   (cancelled-p
    :initform nil :accessor acp-session-cancelled-p :type boolean
    :documentation "Whether the current prompt was cancelled.")
   (closed-p
    :initform nil :accessor acp-session-closed-p :type boolean
    :documentation "Whether resources and the conversation lease were released."))
  (:documentation "One durable conversation and its ephemeral editor connection state."))

(-> acp-service--session (acp-service string) acp-session)
(defun acp-service--session (service identifier)
  "Return the live session IDENTIFIER, or signal Invalid Params."
  (with-lock-held ((acp-service-lock service))
    (or (gethash identifier (acp-service-sessions service))
        (agentcomms:acp-invalid-params "Unknown session ~A." identifier))))

(-> acp-service--call-with-operation (acp-service t function) t)
(defun acp-service--call-with-operation (service owner function)
  "Run FUNCTION as the sole primary operation, rejecting overlapping work."
  (let ((claimed-p nil))
    (unwind-protect
         (progn
           (with-lock-held ((acp-service-lock service))
             (when (acp-service-closed-p service)
               (error 'agentcomms:acp-connection-closed))
             (when (acp-service-busy-owner service)
               (error 'agentcomms:acp-method-error :code -32603
                      :message "Another primary operation is active."))
             (setf (acp-service-busy-owner service) owner
                   claimed-p t))
           (funcall function))
      (when claimed-p
        (with-lock-held ((acp-service-lock service))
          (setf (acp-service-busy-owner service) nil))))))

(-> acp-session-check-cancelled (acp-session) null)
(defun acp-session-check-cancelled (session)
  "Signal APPLICATION-TURN-CANCELLED at a provider or tool boundary."
  (when (or (acp-session-cancelled-p session)
            (acp-session-closed-p session)
            (acp-service-closed-p (acp-session-service session))
            (agentcomms:agent-session-cancelled-p
             (acp-session-service session) (acp-session-identifier session)))
    (error 'application-turn-cancelled))
  nil)

(-> acp-session--interrupt (acp-session t integer) null)
(defun acp-session--interrupt (session thread epoch)
  "Deliver cancellation to THREAD only within SESSION's captured EPOCH."
  (when (and thread (not (eq thread (current-thread))) (thread-alive-p thread))
    (handler-case
        (interrupt-thread
         thread
         (lambda ()
           (when (with-lock-held ((acp-session-lock session))
                   (and (= epoch (acp-session-epoch session))
                        (acp-session-cancelled-p session)
                        (or (and (eq thread (acp-session-prompt-thread session))
                                 (acp-session-prompt-interruptible-p session))
                            (eql epoch (gethash thread (acp-session-tool-threads session))))))
             (error 'application-turn-cancelled))))
      (error ()
        nil)))
  nil)

(-> acp-session--call-with-finalization (acp-session function) t)
(defun acp-session--call-with-finalization (session function)
  "Defer queued cancellation while FUNCTION repairs or finalizes durable turn records."
  (sb-sys:without-interrupts
      (with-lock-held ((acp-session-lock session))
        (setf (acp-session-prompt-interruptible-p session) nil))
    (funcall function)))
(-> acp-session-cancel (acp-session) null)
(defun acp-session-cancel (session)
  "Cancel the prompt, concurrent tool workers, and session-owned jobs."
  (let ((threads nil) (epoch nil))
    (with-lock-held ((acp-session-lock session))
      (unless (acp-session-cancelled-p session)
        (setf (acp-session-cancelled-p session) t
              epoch (acp-session-epoch session)
              threads (cons (acp-session-prompt-thread session)
                            (loop for thread being the hash-keys of (acp-session-tool-threads session)
                                  collect thread)))))
    (when epoch
      (let ((orchestrator (application--task-orchestrator (acp-session-application session))))
        (when orchestrator
          (dolist (pool (list (task-orchestrator-pool orchestrator)
                              (task-orchestrator-execution-pool orchestrator)))
            (dolist (job (job-pool-list-jobs pool))
              (unless (job-terminal-p job)
                (job-cancel job))))))
      ;; Wake tool owners first so the prompt can join its tool wave while unwinding.
      (dolist (thread (reverse threads))
        (acp-session--interrupt session thread epoch))))
  nil)

(-> acp-session--call-with-tool (acp-session function) t)
(defun acp-session--call-with-tool (session function)
  "Own one tool execution and install session bindings in its worker thread."
  (let* ((thread (current-thread))
         (application (acp-session-application session))
         (*active-application* application)
         (*configuration* (application-configuration application))
         (*default-pathname-defaults* (config :working-directory *configuration*))
         (*standard-output* *error-output*)
         (*trace-output* *error-output*)
         (registered-p nil))
    (unwind-protect
         (progn
           (with-lock-held ((acp-session-lock session))
             (unless (eq thread (acp-session-prompt-thread session))
               (setf (gethash thread (acp-session-tool-threads session))
                     (acp-session-epoch session)
                     registered-p t)))
           (acp-session-check-cancelled session)
           (funcall function))
      (when registered-p
        (with-lock-held ((acp-session-lock session))
          (remhash thread (acp-session-tool-threads session)))))))

(-> acp-session-close (acp-session) null)
(defun acp-session-close (session)
  "Wait for cancelled work to unwind, then close tools, workers, and the lease."
  (with-lock-held ((acp-session-cleanup-lock session))
    (unless (acp-session-closed-p session)
      (with-lock-held ((acp-session-lock session))
        (setf (acp-session-closing-p session) t))
      (acp-session-cancel session)
      (let ((deadline (+ (get-internal-real-time)
                         (* *acp-session-close-seconds* internal-time-units-per-second))))
        (loop while (with-lock-held ((acp-session-lock session))
                      (or (acp-session-prompt-thread session)
                          (plusp (hash-table-count (acp-session-tool-threads session)))))
              do (when (>= (get-internal-real-time) deadline)
                   (error 'agentcomms:acp-state-error
                          :message "The session has not finished cancelling."))
              (sleep 0.01)))
      (let* ((application (acp-session-application session))
             (failures (application--discard-connection-resources
                        application (application-tool-registry application)
                        (application-worker application))))
        (when failures
          (error 'agentcomms:acp-state-error :message "Session resource cleanup failed."))
        (acp-mcp-release-credentials
         (mcp-tool-registry-manager (application-tool-registry application)))
        (application-release-conversation-lease application))
      (with-lock-held ((acp-session-lock session))
        (clrhash (acp-session-permissions session))
        (setf (acp-session-closed-p session) t))))
  nil)
