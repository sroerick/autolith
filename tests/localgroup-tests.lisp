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
