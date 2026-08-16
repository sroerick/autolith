(in-package #:autolith)

;;;; -- Localgroup Process Handoff Tests --

(-> test-localgroup--relay-application
    (configuration &key (:persisted-p boolean))
    (values application application-input-controller localgroup-terminal conversation))
(defun test-localgroup--relay-application (configuration &key persisted-p)
  "Return a leased APPLICATION with a foreground localgroup terminal relay."
  (let* ((direct
           (stream-terminal-create
            :input-stream (make-string-input-stream "")
            :output-stream (make-broadcast-stream)
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
                          :main-thread (current-thread))))
    (when persisted-p
      (conversation-append-user-message conversation "persisted"))
    (setf (application-input-controller application) controller
          (application-conversation-lease application)
          (conversation-lease-acquire
           configuration (conversation-identifier conversation)))
    (values application controller relay conversation)))

(-> test-localgroup-handoff-records () null)
(defun test-localgroup-handoff-records ()
  "Test private handoff records, startup identity, drafts, and registry ownership."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (first-application nil)
         (second-application nil)
         (first-session nil)
         (second-session nil)
         (handoff-pathname nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-bind (application controller relay conversation)
               (test-localgroup--relay-application
                configuration :persisted-p t)
             (declare (ignore controller relay))
             (setf first-application application
                   first-session (localgroup-start application))
             (terminal-ui-set-input (application-ui application) "draft survives")
             (setf handoff-pathname
                   (localgroup-handoff--write
                    application first-session ':detach))
             (let* ((record
                      (localgroup-handoff--read configuration handoff-pathname))
                    (restored (localgroup-handoff-initial-input record)))
               (test-assert
                (and (string=
                      (getf (rest record) :conversation-id)
                      (conversation-identifier conversation))
                     (typep restored 'user-message-input)
                     (string= (user-message-input-text restored) "draft survives"))
                "handoff records preserve durable conversation identity and draft")
               (let ((*localgroup-handoff-setsid-function* (lambda () 0)))
                 (localgroup-handoff-begin-startup record))
               (multiple-value-bind (pid-record complete-p)
                   (snapshot-read
                    (localgroup-handoff--pid-pathname handoff-pathname))
                 (test-assert
                  (and complete-p
                       (probe-file (getf (rest record) :pathname))
                       (= (getf (rest pid-record) :pid)
                          (sb-posix:getpid)))
                  "replacement startup acknowledges its detached process identity"))
               (multiple-value-bind (application controller relay conversation)
                   (test-localgroup--relay-application configuration)
                 (declare (ignore controller relay conversation))
                 (setf second-application application)
                 (let ((*localgroup-startup-record* record))
                   (setf second-session (localgroup-start application))))))
           (test-assert
            (and (string= (localgroup-session-identifier first-session)
                          (localgroup-session-identifier second-session))
                 (string= (localgroup-session-token first-session)
                          (localgroup-session-token second-session))
                 (= (localgroup-session-created-at first-session)
                    (localgroup-session-created-at second-session))
                 (not (probe-file handoff-pathname)))
            "replacement startup preserves localgroup identity and consumes its record")
           (localgroup-stop first-application)
           (test-assert
            (equal
             (localgroup--read-endpoint-record
              (localgroup-session-registry-pathname second-session))
             (localgroup--registry-record second-session))
            "old shutdown cannot delete a replacement endpoint record"))
      (when first-application
        (localgroup-stop first-application)
        (application-release-conversation-lease first-application))
      (when second-application
        (localgroup-stop second-application)
        (application-release-conversation-lease second-application))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup-handoff-scheduling () null)
(defun test-localgroup-handoff-scheduling ()
  "Test foreground handoff admission after queued work and live child jobs."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil)
         (relay nil)
         (session nil)
         (socket nil)
         (stream nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-bind (new-application new-controller new-relay conversation)
               (test-localgroup--relay-application configuration)
             (declare (ignore conversation))
             (setf application new-application
                   controller new-controller
                   relay new-relay
                   session (localgroup-start new-application)))
           (application-input-controller--enqueue
            controller ':message "first")
           (multiple-value-setq (socket stream)
             (multiple-value-bind (new-socket new-stream response)
                 (test-localgroup--attach session ':take-over)
               (test-assert
                (and (eq (first response) ':handoff)
                     (eq (localgroup-terminal-attachment-kind relay) ':foreground)
                     (application-localgroup-handoff-pending-p application))
                "foreground take-over schedules process handoff without dropping the terminal")
               (values new-socket new-stream)))
           (let ((status (localgroup-status-snapshot session)))
             (test-assert
              (and (eq (getf (rest status) :state) ':detaching)
                   (not (getf (rest status) :idle-p)))
              "pending handoff is visible and never reported as strict idle"))
           (test-assert
            (equal (application-input-controller--next-work controller)
                   (list ':message "first"))
            "queued primary work runs before process handoff")
           (application-input-controller--finish-work controller)
           (let ((orchestrator
                   (make-instance 'task-orchestrator
                                  :pool (make-job-pool :name "Autolith handoff test"
                                                       :job-class 'task-job
                                                       :maximum-concurrency 1
                                                       :maximum-batch-size 1
                                                       :maximum-live-jobs 1
                                                       :maximum-runtime-milliseconds 0
                                                       :start-threads-p nil)
                                  :maximum-depth 1)))
             (setf (application-task-presentation-orchestrator application)
                   orchestrator
                   (cl-jobpond::job-pool--live-count
                    (task-orchestrator-pool orchestrator))
                   1)
             (test-assert
              (null (application-localgroup-take-ready-handoff application))
              "live child work prevents handoff admission")
             (setf (cl-jobpond::job-pool--live-count
                    (task-orchestrator-pool orchestrator))
                   0)
             (test-assert
              (equal (application-input-controller--next-work controller)
                     (list ':localgroup-handoff ':take-over))
              "handoff becomes main-thread work after children finish")))
      (when stream
        (ignore-errors (close stream)))
      (when (and socket (null stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket)))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (when controller
        (application-input-controller-stop controller))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup-process-handoff () null)
(defun test-localgroup-process-handoff ()
  "Test successful and failed process handoff lease and snapshot behavior."
  (labels ((run-success (persisted-p)
             "Run one successful fake replacement for PERSISTED-P."
             (let* ((configuration (test-configuration))
                    (root (test-configuration-root configuration))
                    (application nil)
                    (controller nil)
                    (session nil)
                    (captured-record nil))
               (unwind-protect
                    (progn
                      (configuration-ensure-directories configuration)
                      (multiple-value-bind
                            (new-application new-controller relay conversation)
                          (test-localgroup--relay-application
                           configuration :persisted-p persisted-p)
                        (declare (ignore relay))
                        (setf application new-application
                              controller new-controller
                              session (localgroup-start new-application))
                        (terminal-ui-set-input
                         (application-ui application) "handoff draft")
                        (application-localgroup-request-handoff
                         application ':detach)
                        (let ((work
                                (application-input-controller--next-work
                                 controller)))
                          (test-assert
                           (equal work (list ':localgroup-handoff ':detach))
                           "idle detach becomes explicit main-thread work")
                          (let ((*localgroup-handoff-launch-function*
                                  (lambda (ignored-application pathname)
                                    (declare (ignore ignored-application))
                                    (multiple-value-bind (record complete-p)
                                        (snapshot-read pathname)
                                      (test-assert complete-p
                                                   "handoff snapshot is complete before launch")
                                      (setf captured-record record))
                                    ':fake-process))
                                (*localgroup-handoff-wait-function*
                                  (lambda (configuration session-id token old-pid)
                                    (declare (ignore configuration session-id token old-pid))
                                    t)))
                            (application-input-controller--run-work
                             controller work)))
                        (test-assert
                         (and (application-input-controller-stopping-p controller)
                              (null (application-conversation-lease application))
                              (string=
                               (getf (rest captured-record) :draft)
                               "handoff draft")
                              (string=
                               (getf (rest captured-record) :session-id)
                               (localgroup-session-identifier session))
                              (string=
                               (getf (rest captured-record) :token)
                               (localgroup-session-token session))
                              (if persisted-p
                                  (string=
                                   (getf (rest captured-record) :conversation-id)
                                   (conversation-identifier conversation))
                                  (null
                                   (getf (rest captured-record) :conversation-id))))
                         "successful handoff transfers lease, identity, conversation, and draft")))
                 (when application
                   (localgroup-stop application)
                   (application-release-conversation-lease application))
                 (when controller
                   (application-input-controller-stop controller))
                 (uiop:delete-directory-tree root
                                             :validate t
                                             :if-does-not-exist ':ignore)))))
    (run-success nil)
    (run-success t))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil)
         (session nil)
         (stopped-p nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-bind (new-application new-controller relay conversation)
               (test-localgroup--relay-application
                configuration :persisted-p t)
             (declare (ignore relay conversation))
             (setf application new-application
                   controller new-controller
                   session (localgroup-start new-application)))
           (setf (application-input-controller-pause-depth controller) 1
                 (application-input-controller-reader-paused-p controller) t)
           (application-localgroup-request-handoff application ':detach)
           (let ((work (application-input-controller--next-work controller)))
             (test-assert
              (handler-case
                  (let ((*localgroup-handoff-launch-function*
                          (lambda (ignored-application pathname)
                            (declare (ignore ignored-application pathname))
                            ':fake-process))
                        (*localgroup-handoff-wait-function*
                          (lambda (configuration session-id token old-pid)
                            (declare (ignore configuration session-id token old-pid))
                            nil))
                        (*localgroup-handoff-stop-function*
                          (lambda (process pathname)
                            (declare (ignore process pathname))
                            (setf stopped-p t))))
                    (application-localgroup-run-handoff
                     application (second work) controller)
                    nil)
                (localgroup-error () t))
              "failed replacement reports a structured localgroup error"))
           (test-assert
            (and stopped-p
                 (application-conversation-lease application)
                 (not (application-input-controller-stopping-p controller))
                 (not
                  (application-input-controller-localgroup-handoff-p controller))
                 (not (localgroup-session-handoff-running-p session))
                 (null
                  (uiop:directory-files
                   (localgroup-handoff-directory configuration) "*.sexp")))
            "failed replacement is stopped and the old leased session remains usable"))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (when controller
        (setf (application-input-controller-pause-depth controller) 0)
        (application-input-controller-stop controller))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)
