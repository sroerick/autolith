(in-package #:autolith)

;;;; -- Localgroup Tests --

(-> test-localgroup--application (configuration) (values application application-input-controller))
(defun test-localgroup--application (configuration)
  "Return a minimal APPLICATION and responsive controller for localgroup tests."
  (let* ((conversation (conversation-create configuration))
         (ui (terminal-ui-create
              :terminal (make-instance 'recording-terminal :columns 80)))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (controller
           (make-instance 'application-input-controller
                          :application application
                          :later-state (make-instance 'later-state)
                          :pending-later-entries nil
                          :main-thread (current-thread))))
    (setf (application-input-controller application) controller)
    (values application controller)))

(-> test-localgroup--read-packet (stream) list)
(defun test-localgroup--read-packet (stream)
  "Return one packet after a bounded wait for STREAM input."
  (test-assert
   (task-tests--wait-until (lambda () (listen stream)) 2)
   "the localgroup attachment produces its next packet promptly")
  (or (localgroup-read-packet stream)
      (error "The localgroup attachment closed before its next packet.")))

(-> test-localgroup--attach
    (localgroup-session keyword)
    (values sb-bsd-sockets:socket stream list))
(defun test-localgroup--attach (session mode)
  "Open one test attachment to SESSION with MODE."
  (multiple-value-bind (socket stream)
      (localgroup-connect (localgroup-session-port session))
    (localgroup-write-packet
     stream
     (list :localgroup-request
           :version *localgroup-protocol-version*
           :token (localgroup-session-token session)
           :operation ':attach
           :arguments
           (list :mode mode :rows 31 :columns 91 :styled-p nil)))
    (values socket stream (test-localgroup--read-packet stream))))

(-> test-localgroup-protocol () null)
(defun test-localgroup-protocol ()
  "Test bounded safe packets, private discovery, status, and control routing."
  (let ((executed-p nil))
    (declare (special executed-p))
    (test-assert
     (handler-case
         (progn
           (localgroup-read-packet
            (make-string-input-stream
             "#.(setf executed-p t)\n"))
           nil)
       (localgroup-error () t))
     "localgroup packet reading disables reader evaluation")
    (test-assert (not executed-p)
                 "rejected localgroup reader syntax never executes"))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil)
         (session nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-setq (application controller)
             (test-localgroup--application configuration))
           (setf session (localgroup-start application))
           (let* ((record-pathname
                    (localgroup-session-registry-pathname session))
                  (record (localgroup--read-endpoint-record record-pathname))
                  (response
                    (localgroup-call
                     (localgroup-session-port session)
                     (localgroup-session-token session)
                     ':status))
                  (status (getf (rest response) :status)))
             (test-assert (localgroup--endpoint-record-p record)
                          "localgroup start publishes one valid private record")
             (test-assert
              (and (eq (first response) ':ok)
                   (string= (getf (rest status) :session-id)
                            (localgroup-session-identifier session))
                   (getf (rest status) :idle-p)
                   (getf (rest status) :waiting-for-input-p)
                   (zerop (getf (rest status) :task-live-count)))
              "an empty controller with no child work is strictly idle")
             (test-assert
              (eq
               (first
                (localgroup-call
                 (localgroup-session-port session)
                 "wrong-token"
                 ':status))
               ':error)
              "an invalid capability token receives no successful status"))
           (let ((identifier (localgroup-session-identifier session))
                 (token (localgroup-session-token session))
                 (created-at (localgroup-session-created-at session)))
             (test-assert
              (eq
               (application-call-with-localgroup-quiesced
                application
                (lambda ()
                  (and (null (application-localgroup-session application))
                       ':quiesced)))
               ':quiesced)
              "checkpoint quiescence removes every localgroup runtime thread")
             (setf session (application-localgroup-session application))
             (test-assert
              (and (string= (localgroup-session-identifier session) identifier)
                   (string= (localgroup-session-token session) token)
                   (= (localgroup-session-created-at session) created-at))
              "checkpoint quiescence preserves the process session identity"))
           (localgroup-call
            (localgroup-session-port session)
            (localgroup-session-token session)
            ':tell
            (list :message "remote input"))
           (with-lock-held ((application-input-controller-lock controller))
             (test-assert
              (equal (application-input-controller-work-items controller)
                     (list (list ':message "remote input")))
              "localgroup tell uses the ordinary submitted-message queue"))
           (let ((status
                   (getf
                    (rest
                     (localgroup-call
                      (localgroup-session-port session)
                      (localgroup-session-token session)
                      ':status))
                    :status)))
             (test-assert
              (and (not (getf (rest status) :idle-p))
                   (= (getf (rest status) :queued-input-count) 1))
              "queued remote input makes strict idle false"))
           (localgroup-call
            (localgroup-session-port session)
            (localgroup-session-token session)
            ':pause)
           (test-assert (application-localgroup-paused-p application)
                        "localgroup pause holds queued primary work")
           (localgroup-call
            (localgroup-session-port session)
            (localgroup-session-token session)
            ':tell
            (list :message "resume input"))
           (test-assert (not (application-localgroup-paused-p application))
                        "new localgroup input resumes a paused session")
           (localgroup-call
            (localgroup-session-port session)
            (localgroup-session-token session)
            ':kill)
           (test-assert (application-input-controller-stopping-p controller)
                        "localgroup kill requests ordinary graceful shutdown"))
      (when application
        (localgroup-stop application))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)

(-> test-localgroup-attachments () null)
(defun test-localgroup-attachments ()
  "Test read-only observation and controlling terminal handoff over the endpoint."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (output (make-string-output-stream))
         (direct
           (stream-terminal-create
            :input-stream (make-string-input-stream "")
            :output-stream output
            :input-file-descriptor 0
            :rows 24
            :columns 80))
         (relay (localgroup-terminal-create direct))
         (conversation (conversation-create configuration))
         (ui (terminal-ui-create :terminal relay))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (controller
           (make-instance 'application-input-controller
                          :application application
                          :later-state (make-instance 'later-state)
                          :pending-later-entries nil
                          :main-thread (current-thread)))
         (session nil)
         (socket nil)
         (stream nil))
    (setf (application-input-controller application) controller)
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (terminal-start relay)
           (terminal--write relay "before attachment\n")
           (setf session (localgroup-start application))
           (multiple-value-bind (read-only-socket read-only-stream response)
               (test-localgroup--attach session ':read-only)
             (unwind-protect
                  (progn
                    (test-assert
                     (and (eq (first response) ':attached)
                          (search "before attachment"
                                  (getf (rest response) :history)))
                     "read-only attach receives bounded existing terminal output")
                    (terminal--write relay "observer output\n")
                    (let ((packet
                            (test-localgroup--read-packet read-only-stream)))
                      (test-assert
                       (and (eq (first packet) ':output)
                            (string= (second packet) "observer output\n"))
                       "read-only attach receives live terminal output"))
                    (localgroup-write-packet
                     read-only-stream (list :event (list :insert "ignored")))
                    (sleep 0.05)
                    (test-assert
                     (string= (line-editor-text (terminal-ui-editor ui)) "")
                     "read-only attachment cannot inject terminal input")
                    (localgroup-write-packet read-only-stream '(:detach)))
               (ignore-errors (close read-only-stream))
               (ignore-errors
                 (sb-bsd-sockets:socket-close read-only-socket))))
           (test-assert
            (localgroup-terminal-release-direct relay)
            "a detached process can release its original foreground terminal")
           (multiple-value-setq (socket stream)
             (multiple-value-bind (control-socket control-stream response)
                 (test-localgroup--attach session ':control)
               (test-assert
                (and (eq (first response) ':attached)
                     (eq (localgroup-terminal-attachment-kind relay) ':remote)
                     (= (terminal-rows relay) 31)
                     (= (terminal-columns relay) 91))
                "control attaches to a detached terminal relay")
               (values control-socket control-stream)))
           (localgroup-write-packet stream (list :event (list :insert "remote")))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (string= (line-editor-text (terminal-ui-editor ui)) "remote"))
             2)
            "controlling attachment input reaches the ordinary line editor")
           (localgroup-write-packet stream (list :event ':submit))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (with-lock-held ((application-input-controller-lock controller))
                 (equal
                  (application-input-controller-work-items controller)
                  (list (list ':message "remote")))))
             2)
            "controlling attachment submission uses the ordinary input queue")
           (localgroup-write-packet
            stream (list :resize :rows 44 :columns 120 :styled-p t))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (and (= (terminal-rows relay) 44)
                    (= (terminal-columns relay) 120)
                    (terminal-styled-p relay)))
             2)
            "controlling attachment resize updates the live terminal")
           (localgroup-write-packet stream '(:detach))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (eq (localgroup-terminal-attachment-kind relay) ':detached))
             2)
            "attachment detach leaves the application running without a terminal"))
      (when stream
        (ignore-errors (close stream)))
      (when (and socket (null stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket)))
      (when session
        (localgroup-stop application))
      (application-input-controller-stop controller)
      (ignore-errors (terminal-stop relay))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)
