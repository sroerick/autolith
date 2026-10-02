(in-package #:autolith)

;;;; -- ACP Observer Behaviour --

(defclass acp-observer-test-client (agentcomms:acp-client)
  ((updates
    :initform nil :accessor acp-observer-test-client-updates
    :documentation "Updates received by the deterministic ACP client.")
   (permission-choice
    :initarg :permission-choice :initform "once"
    :accessor acp-observer-test-client-permission-choice
    :documentation "The permission option selected by the client."))
  (:documentation "A minimal ACP client used by observer protocol tests."))

(defmethod agentcomms:client-session-update
    ((client acp-observer-test-client) session-id update params)
  (declare (ignore session-id params))
  (push update (acp-observer-test-client-updates client))
  nil)

(defmethod agentcomms:client-request-permission
    ((client acp-observer-test-client) session-id tool-call options params)
  (declare (ignore session-id tool-call options params))
  (values ':selected (acp-observer-test-client-permission-choice client)))

(defmethod agentcomms:client-extension-request ((client acp-observer-test-client) method params)
  "Acknowledge a barrier after the client has handled preceding notifications."
  (declare (ignore params))
  (if (equal method "_sync")
      (agentcomms:json-object)
      (call-next-method)))

(defun acp-observer-test-service (configuration)
  "Create a service suitable for direct observer protocol tests."
  (make-instance 'acp-service :configuration configuration))

(-> test-acp-observer-streams-one-response () null)
(defun test-acp-observer-streams-one-response ()
  "Exercise test-acp-observer-streams-one-response."
  (with-test-configuration (configuration root)
    (let* ((service (acp-observer-test-service configuration))
           (session
            (make-instance 'acp-session :service service :identifier "observer" :application
                           (make-instance 'application :configuration configuration)))
           (observer (make-instance 'acp-observer :session session :turn-sequence 7))
           (client (make-instance 'acp-observer-test-client))
           (updates nil))
      (declare (ignore root))
      (multiple-value-bind (server client-channel)
          (agentcomms:make-acp-channel-pair)
        (unwind-protect
             (progn
               (agentcomms:acp-agent-connect service server)
               (agentcomms:acp-client-connect client client-channel)
               (agentcomms:client-initialize client)
               (setf (acp-observer-test-client-updates client) nil)
               (agent-observer-text observer "hello ")
               (agent-observer-text observer "world")
               (agent-observer-status observer ':assistant-response-persisted
                                      (list :text "hello world"))
               (agentcomms:agent-client-request service "_sync" (agentcomms:json-object))
               (setf updates (reverse (acp-observer-test-client-updates client)))
               (test-assert
                (= 2 (count ':agent-message-chunk updates :key #'agentcomms:acp-update-kind))
                "streamed response is not repeated on persistence")
               (test-assert
                (equal "hello world"
                       (apply #'concatenate 'string
                              (mapcar
                               (lambda (update)
                                 (agentcomms:acp-content-text
                                  (agentcomms:json-get update "content")))
                               (remove-if-not
                                (lambda (update)
                                  (eq ':agent-message-chunk (agentcomms:acp-update-kind update)))
                                updates))))
                "streamed response chunks preserve order"))
          (agentcomms:connection-close (agentcomms:acp-client-connection client))))))
  nil)

(-> test-acp-observer-permission-validates-offered-choice () null)
(defun test-acp-observer-permission-validates-offered-choice ()
  "Exercise test-acp-observer-permission-validates-offered-choice."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((service (acp-observer-test-service configuration))
           (session
            (make-instance 'acp-session :service service :identifier "permissions" :application
                           (make-instance 'application :configuration configuration)))
           (observer (make-instance 'acp-observer :session session :turn-sequence 1))
           (client (make-instance 'acp-observer-test-client :permission-choice "bogus"))
           (server nil)
           (client-channel nil))
      (multiple-value-setq (server client-channel) (agentcomms:make-acp-channel-pair))
      (unwind-protect
           (progn
             (agentcomms:acp-agent-connect service server)
             (agentcomms:acp-client-connect client client-channel)
             (agentcomms:client-initialize client)
             (test-assert
              (null
               (acp-observer--approval observer "shell.run" (agentcomms:json-object) '("command")))
              "an unoffered permission choice is denied")
             (setf (acp-observer-test-client-permission-choice client) "session")
             (test-assert
              (acp-observer--approval observer "shell.run" (agentcomms:json-object) '("command"))
              "an offered session choice is accepted"))
        (agentcomms:connection-close (agentcomms:acp-client-connection client)))))
  nil)

(-> test-acp-observer-forwards-serialized-tool-execution () null)
(defun test-acp-observer-forwards-serialized-tool-execution ()
  "Exercise test-acp-observer-forwards-serialized-tool-execution."
  (with-test-configuration (configuration)
    (let* ((service (acp-observer-test-service configuration))
           (application (make-instance 'application :configuration configuration))
           (session
            (make-instance 'acp-session :service service :identifier "forward" :application
                           application))
           (observer (make-instance 'acp-observer :session session :turn-sequence 1))
           (serialized (make-instance 'serialized-agent-observer :delegate observer))
           (seen nil))
      (agent-observer-call-with-tool-execution serialized "call-1"
                                               (lambda ()
                                                 (setf seen
                                                       (and (eq *active-application* application)
                                                            (string= *acp-current-tool-call-id*
                                                                     "call-1")))))
      (test-assert seen "serialized observer forwards ACP tool ownership and bindings")))
  nil)
