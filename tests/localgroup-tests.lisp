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

(-> test-localgroup-terminal-restart () null)
(defun test-localgroup-terminal-restart ()
  "Test that stopping a relay retains its direct terminal for restart."
  (let* ((direct
           (stream-terminal-create
            :input-stream (make-string-input-stream "")
            :output-stream (make-string-output-stream)
            :input-file-descriptor -1))
         (relay (localgroup-terminal-create direct)))
    (terminal-start relay)
    (test-assert (terminal-started-p direct)
                 "a direct relay starts its direct terminal")
    (terminal-stop relay)
    (test-assert
     (eq (localgroup-terminal-direct-terminal relay) direct)
     "stopping a relay retains its direct transport")
    (test-assert (not (terminal-started-p direct))
                 "stopping a relay stops its direct terminal")
    (terminal-start relay)
    (test-assert (terminal-started-p direct)
                 "a stopped direct relay restarts its direct terminal")
    (terminal-stop relay))
  nil)

(-> test-localgroup-protocol () null)
(defun test-localgroup-protocol ()
  "Test bounded safe packets, private discovery, status, and control routing."
  (let* ((text
           (concatenate 'string
                        "first"
                        (string #\Newline)
                        "second"
                        (string #\Return)
                        " quoted \"text\" \\ λ"))
         (packets
           (list (list :output text)
                 (list :event (list :paste text))))
         (wire
           (with-output-to-string (stream)
             (dolist (packet packets)
               (localgroup-write-packet stream packet))))
         (input (make-string-input-stream wire)))
    (test-assert
     (and (equal (localgroup-read-packet input) (first packets))
          (equal (localgroup-read-packet input) (second packets))
          (null (localgroup-read-packet input)))
     "length-prefixed localgroup frames preserve multiline Unicode packets"))
  (let* ((executed-p nil)
         (payload "#.(setf executed-p t)")
         (wire (format nil "~D~%~A" (length payload) payload)))
    (declare (special executed-p))
    (test-assert
     (handler-case
         (progn
           (localgroup-read-packet (make-string-input-stream wire))
           nil)
       (localgroup-error () t))
     "localgroup packet reading disables reader evaluation")
    (test-assert (not executed-p)
                 "rejected localgroup reader syntax never executes"))
  (test-assert
   (handler-case
       (progn
         (localgroup-read-packet
          (make-string-input-stream (format nil "5~%(:x")))
         nil)
     (localgroup-error () t))
   "localgroup framing rejects a payload shorter than its declared length")
  (multiple-value-bind (read-descriptor write-descriptor)
      (sb-posix:pipe)
    (let ((input nil))
      (unwind-protect
           (progn
             (setf input
                   (sb-sys:make-fd-stream
                    read-descriptor
                    :input t
                    :element-type 'character
                    :external-format :utf-8
                    :buffering ':none
                    :auto-close nil))
             (let ((*localgroup-connect-timeout-seconds* 0.05))
               (test-assert
                (handler-case
                    (progn
                      (localgroup-read-response input ':status)
                      nil)
                  (localgroup-error (condition)
                    (search "did not respond in time"
                            (autolith-error-message condition))))
                "localgroup response reads stop at the transport deadline")))
        (when input
          (ignore-errors (close input)))
        (ignore-errors (sb-posix:close read-descriptor))
        (ignore-errors (sb-posix:close write-descriptor)))))
  (let* ((status
           (list :localgroup-status
                 :session-id "ABC123"
                 :state ':idle
                 :conversation-display-id "new"
                 :queued-input-count 0
                 :steering-input-count 0
                 :task-live-count 0
                 :cwd "/tmp/example"))
         (output
           (with-output-to-string (stream)
             (localgroup-print-statuses (list status) :stream stream))))
    (test-assert
     (and (search "SESSION" output)
          (search "ABC123" output)
          (search "/tmp/example" output))
     "localgroup status renders a nonempty human-readable table"))
  (let ((*standard-input* (make-string-input-stream ""))
        (*standard-output* (make-string-output-stream)))
    (test-assert
     (handler-case
         (progn
           (localgroup-attach-record
            (test-configuration)
            (cons #P"noninteractive.sexp"
                  (list :localgroup-endpoint
                        :session-id "NONTTY"
                        :port 1
                        :token "unused"))
            ':read-only)
           nil)
       (localgroup-error (condition)
         (and (eq (localgroup-error-operation condition) ':attach)
              (search "interactive terminal"
                      (autolith-error-message condition)))))
     "localgroup attach rejects noninteractive input before connecting"))
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
  (let* ((terminal (localgroup-terminal-create))
         (socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type ':stream
                                :protocol ':tcp))
         (stream (make-string-output-stream))
         (attachment
           (make-instance 'localgroup-attachment
                          :socket socket
                          :stream stream
                          :mode ':read-only)))
    (unwind-protect
         (let ((*localgroup-terminal-output-chunk-character-limit* 4)
               (*localgroup-terminal-history-character-limit* 5)
               (text (format nil "abcdefghij~%")))
           (test-assert
            (localgroup-terminal-attach
             terminal attachment
             :rows 24
             :columns 80
             :styled-p nil
             :session-id "ORDER")
            "localgroup terminal accepts a read-only attachment")
           (terminal--write terminal text)
           (let* ((frames
                    (with-lock-held ((localgroup-attachment-lock attachment))
                      (copy-list (localgroup-attachment-queue attachment))))
                  (packets
                    (mapcar (lambda (frame)
                              (localgroup-read-packet
                               (make-string-input-stream frame)))
                            frames))
                  (handshake (first packets))
                  (output
                    (apply #'concatenate 'string
                           (mapcar #'second (rest packets)))))
             (test-assert
              (and (= (length frames) 4)
                   (eq (first handshake) ':attached)
                   (string= (getf (rest handshake) :session-id) "ORDER")
                   (every (lambda (packet) (eq (first packet) ':output))
                          (rest packets))
                   (string= output text)
                   (string= (localgroup-terminal-history-text terminal)
                            (subseq text (- (length text) 5))))
              "the handshake precedes bounded lossless output and exact replay")))
      (localgroup-terminal-detach terminal attachment)
      (localgroup-attachment-close attachment)
      (ignore-errors (sb-bsd-sockets:socket-close socket))))
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
           (terminal--write relay (format nil "before attachment~%"))
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
                    (terminal--write relay (format nil "observer output~%"))
                    (let ((packet
                            (test-localgroup--read-packet read-only-stream)))
                      (test-assert
                       (and (eq (first packet) ':output)
                            (string= (second packet) (format nil "observer output~%")))
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
