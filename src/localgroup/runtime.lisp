(in-package #:autolith)

;;;; -- Localgroup Runtime State --

(defparameter *localgroup-registry-version* 1
  "The readable localgroup endpoint-record version.")

(defclass localgroup-session ()
  ((application
    :initarg :application
    :reader localgroup-session-application
    :type application
    :documentation "The primary application exposed by this endpoint.")
   (identifier
    :initarg :identifier
    :reader localgroup-session-identifier
    :type non-empty-string
    :documentation "The process-lifetime identifier used by localgroup commands.")
   (token
    :initarg :token
    :reader localgroup-session-token
    :type non-empty-string
    :documentation "The private capability authenticating loopback requests.")
   (listener
    :initarg :listener
    :accessor localgroup-session-listener
    :type t
    :documentation "The loopback listening socket, or NIL after shutdown.")
   (port
    :initarg :port
    :reader localgroup-session-port
    :type (integer 1 65535)
    :documentation "The ephemeral loopback TCP port.")
   (registry-pathname
    :initarg :registry-pathname
    :reader localgroup-session-registry-pathname
    :type pathname
    :documentation "The private endpoint-discovery record.")
   (created-at
    :initarg :created-at
    :reader localgroup-session-created-at
    :type timestamp
    :documentation "The universal time at which the endpoint started.")
   (lock
    :initform (make-lock "Autolith localgroup session")
    :reader localgroup-session-lock
    :type t
    :documentation "The lock protecting lifecycle and pause state.")
   (paused-p
    :initform nil
    :accessor localgroup-session-paused-p
    :type boolean
    :documentation "Whether queued primary work must wait for explicit input.")
   (stopping-p
    :initform nil
    :accessor localgroup-session-stopping-p
    :type boolean
    :documentation "Whether endpoint shutdown has begun.")
   (server-thread
    :initform nil
    :accessor localgroup-session-server-thread
    :type t
    :documentation "The accept-loop thread, when running.")
   (client-threads
    :initform nil
    :accessor localgroup-session-client-threads
    :type list
    :documentation "The bounded request threads not yet reaped.")
   (client-sockets
    :initform nil
    :accessor localgroup-session-client-sockets
    :type list
    :documentation "The accepted sockets closed during endpoint shutdown."))
  (:documentation "One authenticated local control endpoint for a primary application."))

(-> localgroup-registry-directory (configuration) pathname)
(defun localgroup-registry-directory (configuration)
  "Return CONFIGURATION's private localgroup endpoint directory."
  (merge-pathnames "localgroup/" (configuration-state-root configuration)))

(-> localgroup-registry-pathname (configuration string) pathname)
(defun localgroup-registry-pathname (configuration session-id)
  "Return the endpoint record pathname for SESSION-ID."
  (merge-pathnames (make-pathname :name session-id :type "sexp")
                   (localgroup-registry-directory configuration)))

(-> localgroup--registry-record (localgroup-session) list)
(defun localgroup--registry-record (session)
  "Return SESSION's private endpoint discovery record."
  (list :localgroup-endpoint
        :version *localgroup-registry-version*
        :session-id (localgroup-session-identifier session)
        :pid (sb-posix:getpid)
        :address "127.0.0.1"
        :port (localgroup-session-port session)
        :token (localgroup-session-token session)
        :created-at (localgroup-session-created-at session)))

(-> localgroup--publish-registry (localgroup-session) null)
(defun localgroup--publish-registry (session)
  "Atomically publish SESSION's private discovery record."
  (let* ((directory
           (localgroup-registry-directory
            (application-configuration
             (localgroup-session-application session))))
         (pathname (localgroup-session-registry-pathname session)))
    (ensure-directories-exist pathname)
    (sb-posix:chmod (namestring directory) #o700)
    (snapshot-write pathname (localgroup--registry-record session))
    (sb-posix:chmod (namestring pathname) #o600))
  nil)

(-> localgroup--endpoint-record-p (t) boolean)
(defun localgroup--endpoint-record-p (record)
  "Return true when RECORD is one supported localgroup endpoint record."
  (and (localgroup--proper-list-p record)
       (eq (first record) ':localgroup-endpoint)
       (= (or (getf (rest record) :version) 0)
          *localgroup-registry-version*)
       (non-empty-string-p (getf (rest record) :session-id))
       (typep (getf (rest record) :pid) '(integer 1))
       (string= (or (getf (rest record) :address) "") "127.0.0.1")
       (typep (getf (rest record) :port) '(integer 1 65535))
       (non-empty-string-p (getf (rest record) :token))
       (typep (getf (rest record) :created-at) 'timestamp)))

(-> localgroup--read-endpoint-record (pathname) (option list))
(defun localgroup--read-endpoint-record (pathname)
  "Return PATHNAME's valid endpoint record, or NIL."
  (handler-case
      (multiple-value-bind (record complete-p)
          (snapshot-read pathname)
        (and complete-p
             (localgroup--endpoint-record-p record)
             record))
    (error () nil)))

(-> localgroup-endpoint-records (configuration) list)
(defun localgroup-endpoint-records (configuration)
  "Return valid endpoint records ordered by newest publication first."
  (let ((directory (localgroup-registry-directory configuration)))
    (sort
     (loop for pathname in (uiop:directory-files directory "*.sexp")
           for record = (localgroup--read-endpoint-record pathname)
           when record
             collect (cons pathname record))
     #'>
     :key (lambda (entry)
            (or (ignore-errors (file-write-date (first entry))) 0)))))

(-> application-localgroup-paused-p (application) boolean)
(defun application-localgroup-paused-p (application)
  "Return true when APPLICATION is deliberately holding queued work."
  (let ((session (application-localgroup-session application)))
    (and session
         (not
          (null
           (with-lock-held ((localgroup-session-lock session))
             (localgroup-session-paused-p session)))))))

(-> application-localgroup-resume (application) boolean)
(defun application-localgroup-resume (application)
  "Resume APPLICATION's queued work and report whether it had been paused."
  (let ((session (application-localgroup-session application))
        (resumed-p nil))
    (when session
      (with-lock-held ((localgroup-session-lock session))
        (setf resumed-p (localgroup-session-paused-p session)
              (localgroup-session-paused-p session) nil))
      (when resumed-p
        (let ((controller (application-input-controller application)))
          (when controller
            (with-lock-held ((application-input-controller-lock controller))
              (sb-thread:condition-broadcast
               (application-input-controller-condition-variable controller)))))))
    (not (null resumed-p))))

(-> application-localgroup-pause (application) boolean)
(defun application-localgroup-pause (application)
  "Pause queued work and request cancellation of APPLICATION's active turn."
  (let ((session (application-localgroup-session application))
        (controller (application-input-controller application)))
    (unless (and session controller)
      (return-from application-localgroup-pause nil))
    (with-lock-held ((localgroup-session-lock session))
      (setf (localgroup-session-paused-p session) t))
    (application-input-controller--request-active-turn-cancellation controller)
    (with-lock-held ((application-input-controller-lock controller))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    t))

(-> localgroup--task-counts (application) (values (integer 0) (integer 0)))
(defun localgroup--task-counts (application)
  "Return APPLICATION's live and actively running child-job counts."
  (let ((orchestrator (application-task-presentation-orchestrator application)))
    (if orchestrator
        (with-lock-held ((task-orchestrator-lock orchestrator))
          (values (task-orchestrator-live-count orchestrator)
                  (task-orchestrator-active-count orchestrator)))
        (values 0 0))))

(-> localgroup-status-snapshot (localgroup-session) list)
(defun localgroup-status-snapshot (session)
  "Return a portable, internally consistent status snapshot for SESSION."
  (let* ((application (localgroup-session-application session))
         (configuration (application-configuration application))
         (conversation (application-conversation application))
         (controller (application-input-controller application))
         (paused-p (application-localgroup-paused-p application))
         (active-p nil)
         (queued-count 0)
         (steering-count 0)
         (recalled-p nil)
         (stopping-p t)
         (cancelling-p nil)
         (reader-paused-p nil)
         (failed-p nil)
         (task-live-count 0)
         (task-active-count 0))
    (when controller
      (with-lock-held ((application-input-controller-lock controller))
        (setf active-p (application-input-controller-active-p controller)
              queued-count
              (length (application-input-controller-work-items controller))
              steering-count
              (length (application-input-controller-steering-items controller))
              recalled-p
              (not
               (null
                (application-input-controller-follow-up-edit-work controller)))
              stopping-p
              (application-input-controller-stopping-p controller)
              cancelling-p
              (application-input-controller-turn-cancellation-p controller)
              reader-paused-p
              (application-input-controller-reader-paused-p controller)
              failed-p
              (not
               (null (application-input-controller-failure controller))))))
    (multiple-value-setq (task-live-count task-active-count)
      (localgroup--task-counts application))
    (let* ((waiting-for-input-p
             (and controller
                  (not active-p)
                  (zerop queued-count)
                  (zerop steering-count)
                  (not recalled-p)
                  (not stopping-p)
                  (not cancelling-p)
                  (not reader-paused-p)
                  (not failed-p)))
           (idle-p
             (and waiting-for-input-p
                  (not paused-p)
                  (zerop task-live-count)))
           (state
             (cond (stopping-p ':stopping)
                   (failed-p ':failed)
                   (paused-p ':paused)
                   (cancelling-p ':cancelling)
                   (active-p ':active)
                   ((or (plusp queued-count)
                        (plusp steering-count)
                        recalled-p
                        (plusp task-live-count))
                    ':working)
                   (idle-p ':idle)
                   (t ':starting))))
      (list :localgroup-status
            :version *localgroup-protocol-version*
            :session-id (localgroup-session-identifier session)
            :pid (sb-posix:getpid)
            :autolith-version *autolith-version*
            :state state
            :idle-p (not (null idle-p))
            :waiting-for-input-p (not (null waiting-for-input-p))
            :paused-p (not (null paused-p))
            :cwd (namestring (configuration-working-directory configuration))
            :conversation-id (conversation-identifier conversation)
            :conversation-display-id
            (conversation-identifier-display (conversation-identifier conversation))
            :conversation-persisted-p
            (not (null (conversation-persisted-p conversation)))
            :model (configuration-model configuration)
            :reasoning-effort (configuration-reasoning-effort configuration)
            :permission-mode (application-permission-mode application)
            :active-turn-p (not (null active-p))
            :queued-input-count queued-count
            :steering-input-count steering-count
            :recalled-input-p (not (null recalled-p))
            :turn-cancelling-p (not (null cancelling-p))
            :reader-paused-p (not (null reader-paused-p))
            :task-live-count task-live-count
            :task-active-count task-active-count
            :terminal-attached-p
            (not
             (null
              (terminal-interactive-p
               (terminal-ui-terminal (application-ui application)))))
            :created-at (localgroup-session-created-at session)))))


;;;; -- Localgroup Request Handling --

(-> localgroup--request-field (list keyword) t)
(defun localgroup--request-field (request key)
  "Return KEY from REQUEST's property list."
  (getf (rest request) key))

(-> localgroup--valid-request-p (list localgroup-session) boolean)
(defun localgroup--valid-request-p (request session)
  "Return true when REQUEST is structurally valid and authentic for SESSION."
  (and (eq (first request) ':localgroup-request)
       (= (or (localgroup--request-field request :version) 0)
          *localgroup-protocol-version*)
       (stringp (localgroup--request-field request :token))
       (string= (localgroup--request-field request :token)
                (localgroup-session-token session))
       (keywordp (localgroup--request-field request :operation))))

(-> localgroup--tell (localgroup-session list) list)
(defun localgroup--tell (session arguments)
  "Submit ARGUMENTS' message through the ordinary responsive input path."
  (let* ((application (localgroup-session-application session))
         (controller (application-input-controller application))
         (message (getf arguments :message)))
    (unless (and controller (stringp message))
      (error 'localgroup-error
             :message "localgroup tell requires one string message."
             :operation ':tell
             :session-id (localgroup-session-identifier session)))
    (application-localgroup-resume application)
    (application-input-controller--handle-submission
     controller message
     :steer-p (application-input-controller-turn-active-p controller))
    (list :ok :operation :tell
          :session-id (localgroup-session-identifier session))))

(-> localgroup--dispatch-request (localgroup-session list) list)
(defun localgroup--dispatch-request (session request)
  "Return SESSION's response to one authenticated REQUEST."
  (unless (localgroup--valid-request-p request session)
    (return-from localgroup--dispatch-request
      (list :error :message "The localgroup request was rejected.")))
  (let ((operation (localgroup--request-field request :operation))
        (arguments (localgroup--request-field request :arguments)))
    (case operation
      (:status
       (list :ok :status (localgroup-status-snapshot session)))
      (:tell
       (localgroup--tell session arguments))
      (:pause
       (application-localgroup-pause
        (localgroup-session-application session))
       (list :ok :operation :pause
             :session-id (localgroup-session-identifier session)))
      (:kill
       (let ((controller
               (application-input-controller
                (localgroup-session-application session))))
         (when controller
           (application-input-controller--request-exit
            controller ':localgroup-kill))
         (list :ok :operation :kill
               :session-id (localgroup-session-identifier session))))
      (otherwise
       (list :error
             :message (format nil "Unknown localgroup operation ~S." operation))))))

(-> localgroup--handle-client
    (localgroup-session sb-bsd-sockets:socket)
    null)
(defun localgroup--handle-client (session socket)
  "Read, answer, and close one localgroup client SOCKET."
  (let ((stream nil))
    (unwind-protect
         (handler-case
             (progn
               (setf stream (localgroup--socket-stream socket))
               (let ((request (localgroup-read-packet stream)))
                 (localgroup-write-packet
                  stream
                  (if request
                      (localgroup--dispatch-request session request)
                      (list :error :message "The localgroup request was empty.")))))
           (error (condition)
             (when stream
               (ignore-errors
                 (localgroup-write-packet
                  stream
                  (list :error :message (princ-to-string condition)))))))
      (if stream
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))))
  nil)

(-> localgroup--run-client
    (localgroup-session sb-bsd-sockets:socket)
    null)
(defun localgroup--run-client (session socket)
  "Handle SOCKET and remove its runtime resources from SESSION."
  (unwind-protect
       (localgroup--handle-client session socket)
    (with-lock-held ((localgroup-session-lock session))
      (setf (localgroup-session-client-threads session)
            (delete (current-thread)
                    (localgroup-session-client-threads session))
            (localgroup-session-client-sockets session)
            (delete socket (localgroup-session-client-sockets session)))))
  nil)

(-> localgroup--serve (localgroup-session) null)
(defun localgroup--serve (session)
  "Accept bounded localgroup requests until SESSION stops."
  (loop
    (when (with-lock-held ((localgroup-session-lock session))
            (localgroup-session-stopping-p session))
      (return))
    (handler-case
        (let ((socket
                (sb-bsd-sockets:socket-accept
                 (localgroup-session-listener session))))
          (if (with-lock-held ((localgroup-session-lock session))
                (localgroup-session-stopping-p session))
              (ignore-errors (sb-bsd-sockets:socket-close socket))
              (with-lock-held ((localgroup-session-lock session))
                (let ((thread
                        (make-thread
                         (lambda ()
                           (localgroup--run-client session socket))
                         :name "Autolith localgroup request")))
                  (push socket (localgroup-session-client-sockets session))
                  (push thread (localgroup-session-client-threads session))))))
      (error (condition)
        (unless (with-lock-held ((localgroup-session-lock session))
                  (localgroup-session-stopping-p session))
          (format *error-output* "~&Localgroup endpoint failed: ~A~%" condition))
        (return))))
  nil)

(-> localgroup-start
    (application &key (:identifier (option string))
                      (:token (option string))
                      (:created-at (option timestamp)))
    localgroup-session)
(defun localgroup-start (application &key identifier token created-at)
  "Start and publish APPLICATION's authenticated loopback endpoint.

Optional identity values preserve one process session across checkpoint quiescence."
  (let* ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                  :type ':stream
                                  :protocol ':tcp))
         (session nil)
         (completed-p nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind
            listener
            (sb-bsd-sockets:make-inet-address "127.0.0.1")
            0)
           (sb-bsd-sockets:socket-listen listener 16)
           (multiple-value-bind (address port)
               (sb-bsd-sockets:socket-name listener)
             (declare (ignore address))
             (let* ((identifier (or identifier (localgroup-random-session-id)))
                    (token (or token (localgroup-random-token)))
                    (created-at (or created-at (get-universal-time)))
                    (configuration (application-configuration application)))
               (setf session
                     (make-instance
                      'localgroup-session
                      :application application
                      :identifier identifier
                      :token token
                      :listener listener
                      :port port
                      :registry-pathname
                      (localgroup-registry-pathname configuration identifier)
                      :created-at created-at)
                     (application-localgroup-session application) session)
               (localgroup--publish-registry session)
               (setf (localgroup-session-server-thread session)
                     (make-thread
                      (lambda () (localgroup--serve session))
                      :name "Autolith localgroup endpoint")
                     completed-p t)
               session)))
      (unless completed-p
        (when session
          (setf (application-localgroup-session application) nil)
          (ignore-errors
            (delete-file (localgroup-session-registry-pathname session))))
        (ignore-errors (sb-bsd-sockets:socket-close listener))))))

(-> localgroup--wake-server (localgroup-session) null)
(defun localgroup--wake-server (session)
  "Wake SESSION's blocking accept loop without authenticating a request."
  (handler-case
      (multiple-value-bind (socket stream)
          (localgroup-connect (localgroup-session-port session))
        (declare (ignore socket))
        (close stream))
    (error () nil))
  nil)

(-> localgroup-stop (application) null)
(defun localgroup-stop (application)
  "Stop and unpublish APPLICATION's localgroup endpoint idempotently."
  (let ((session (application-localgroup-session application)))
    (when session
      (setf (application-localgroup-session application) nil)
      (let ((server-thread nil)
            (client-threads nil)
            (client-sockets nil)
            (listener nil))
        (with-lock-held ((localgroup-session-lock session))
          (setf (localgroup-session-stopping-p session) t
                server-thread (localgroup-session-server-thread session)
                client-threads
                (copy-list (localgroup-session-client-threads session))
                client-sockets
                (copy-list (localgroup-session-client-sockets session))
                listener (localgroup-session-listener session)))
        (when listener
          (localgroup--wake-server session))
        (dolist (socket client-sockets)
          (ignore-errors (sb-bsd-sockets:socket-close socket)))
        (when (and server-thread
                   (not (eq server-thread (current-thread))))
          (ignore-errors (join-thread server-thread)))
        (when listener
          (ignore-errors (sb-bsd-sockets:socket-close listener))
          (with-lock-held ((localgroup-session-lock session))
            (when (eq listener (localgroup-session-listener session))
              (setf (localgroup-session-listener session) nil))))
        (dolist (thread client-threads)
          (when (and (thread-alive-p thread)
                     (not (eq thread (current-thread))))
            (ignore-errors (join-thread thread))))
        (ignore-errors
          (delete-file (localgroup-session-registry-pathname session))))))
  nil)
