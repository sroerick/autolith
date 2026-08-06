(in-package #:autolith)

;;;; -- Localgroup Discovery Client --

(-> localgroup--record-session-id (list) string)
(defun localgroup--record-session-id (record)
  "Return RECORD's session identifier."
  (getf (rest record) :session-id))

(-> localgroup--record-port (list) integer)
(defun localgroup--record-port (record)
  "Return RECORD's loopback port."
  (getf (rest record) :port))

(-> localgroup--record-token (list) string)
(defun localgroup--record-token (record)
  "Return RECORD's private capability token."
  (getf (rest record) :token))

(-> localgroup--remove-stale-record (pathname list) null)
(defun localgroup--remove-stale-record (pathname expected-record)
  "Delete PATHNAME only when it still contains EXPECTED-RECORD."
  (let ((current (localgroup--read-endpoint-record pathname)))
    (when (equal current expected-record)
      (ignore-errors (delete-file pathname))))
  nil)

(-> localgroup-query-record
    (cons keyword &optional list)
    list)
(defun localgroup-query-record (entry operation &optional arguments)
  "Perform OPERATION against endpoint ENTRY, pruning a confirmed stale record."
  (let* ((pathname (first entry))
         (record (rest entry))
         (session-id (localgroup--record-session-id record))
         (response
           (handler-case
               (localgroup-call
                (localgroup--record-port record)
                (localgroup--record-token record)
                operation
                arguments)
             (error (condition)
               (localgroup--remove-stale-record pathname record)
               (error 'localgroup-error
                      :message (format nil "Localgroup session ~A is unavailable."
                                       session-id)
                      :operation operation
                      :session-id session-id
                      :cause condition)))))
    (unless (and (localgroup--proper-list-p response)
                 (eq (first response) ':ok))
      (error 'localgroup-error
             :message (or (getf (rest response) :message)
                          "The localgroup endpoint rejected the request.")
             :operation operation
             :session-id session-id))
    response))

(-> localgroup--find-record (configuration string) cons)
(defun localgroup--find-record (configuration session-id)
  "Return CONFIGURATION's unique endpoint record matching SESSION-ID."
  (let ((matches
          (remove-if-not
           (lambda (entry)
             (string= session-id
                      (localgroup--record-session-id (rest entry))))
           (localgroup-endpoint-records configuration))))
    (cond ((null matches)
           (error 'localgroup-error
                  :message (format nil "No localgroup session named ~A is running."
                                   session-id)
                  :operation ':discover
                  :session-id session-id))
          ((rest matches)
           (error 'localgroup-error
                  :message (format nil "More than one localgroup session named ~A is registered."
                                   session-id)
                  :operation ':discover
                  :session-id session-id))
          (t
           (first matches)))))

(-> localgroup-statuses (configuration) list)
(defun localgroup-statuses (configuration)
  "Return live localgroup status snapshots, pruning unreachable records."
  (loop for entry in (localgroup-endpoint-records configuration)
        for status =
          (handler-case
              (getf (rest (localgroup-query-record entry ':status)) :status)
            (localgroup-error () nil))
        when status
          collect status))

(-> localgroup--status-state-text (list) string)
(defun localgroup--status-state-text (status)
  "Return STATUS's concise state label."
  (string-downcase (symbol-name (getf (rest status) :state))))

(-> localgroup--status-activity-text (list) string)
(defun localgroup--status-activity-text (status)
  "Return STATUS's compact queue and child activity summary."
  (format nil "q:~D s:~D jobs:~D"
          (getf (rest status) :queued-input-count)
          (getf (rest status) :steering-input-count)
          (getf (rest status) :task-live-count)))

(-> localgroup-print-statuses (list &key (:stream stream)) null)
(defun localgroup-print-statuses (statuses &key (stream *standard-output*))
  "Print a compact human-readable table for STATUSES."
  (if (null statuses)
      (format stream "No local Autolith sessions are running.~%")
      (progn
        (format stream "~&~12A  ~11A  ~9A  ~12A  ~A~%"
                "SESSION" "STATE" "CONVERSATION" "ACTIVITY" "WORKSPACE")
        (format stream "~A~%" (make-string 78 :initial-element #\-))
        (dolist (status statuses)
          (format stream "~12A  ~11A  ~9A  ~12A  ~A~%"
                  (getf (rest status) :session-id)
                  (localgroup--status-state-text status)
                  (getf (rest status) :conversation-display-id)
                  (localgroup--status-activity-text status)
                  (getf (rest status) :cwd)))))
  nil)

(-> localgroup--print-response (list stream) null)
(defun localgroup--print-response (response stream)
  "Print one concise successful localgroup RESPONSE."
  (format stream "~(~A~) requested for localgroup session ~A.~%"
          (getf (rest response) :operation)
          (getf (rest response) :session-id))
  nil)

;;;; -- Localgroup Attach Client --

(-> localgroup--attachment-request-mode (list) keyword)
(defun localgroup--attachment-request-mode (arguments)
  "Return the attach mode selected by command-line ARGUMENTS."
  (let ((read-only-count (count "--read-only" arguments :test #'string=))
        (take-over-count (count "--take-over" arguments :test #'string=)))
    (when (or (> read-only-count 1)
              (> take-over-count 1)
              (and (plusp read-only-count) (plusp take-over-count)))
      (error 'localgroup-error
             :message "Choose at most one of --read-only and --take-over."
             :operation ':arguments))
    (cond ((plusp read-only-count) ':read-only)
          ((plusp take-over-count) ':take-over)
          (t ':control))))

(-> localgroup--attach-receiver
    (stream stream function)
    null)
(defun localgroup--attach-receiver (socket-stream output-stream stop-function)
  "Copy attachment output packets to OUTPUT-STREAM until closure."
  (unwind-protect
       (handler-case
           (loop for packet = (localgroup-read-packet socket-stream)
                 while packet
                 do (case (first packet)
                      (:output
                       (let ((text (second packet)))
                         (when (stringp text)
                           (write-string text output-stream)
                           (finish-output output-stream))))
                      ((:detached :revoked)
                       (return))))
         (error () nil))
    (funcall stop-function))
  nil)

(-> localgroup--attach-terminal-loop
    (stream stream-terminal keyword)
    null)
(defun localgroup--attach-terminal-loop (socket-stream terminal mode)
  "Relay local terminal events to SOCKET-STREAM until the attachment ends."
  (let ((lock (make-lock "Autolith localgroup attach client"))
        (stopped-p nil)
        (receiver nil))
    (labels ((stop ()
               "Mark the local attachment client stopped."
               (with-lock-held (lock)
                 (setf stopped-p t))
               nil)

             (stopped-p ()
               "Return true when the attachment receiver has ended."
               (with-lock-held (lock)
                 stopped-p)))
      (unwind-protect
           (progn
             (setf receiver
                   (make-thread
                    (lambda ()
                      (localgroup--attach-receiver
                       socket-stream *standard-output* #'stop))
                    :name "Autolith localgroup attach input"))
             (loop until (stopped-p)
                   do (when *terminal-resize-pending-p*
                        (setf *terminal-resize-pending-p* nil)
                        (multiple-value-bind (rows columns)
                            (terminal-current-size)
                          (localgroup-write-packet
                           socket-stream
                           (list :resize
                                 :rows rows
                                 :columns columns
                                 :styled-p
                                 (terminal-environment-styling-p)))))
                      (when (terminal-input-ready-p terminal)
                        (let ((event (terminal-read-event terminal)))
                          (cond
                            ((and (eq mode ':read-only)
                                  (member event '(:interrupt :end-of-input)))
                             (return))
                            ((eq event ':end-of-input)
                             (return))
                            ((not (eq mode ':read-only))
                             (localgroup-write-packet
                              socket-stream (list :event event))))))
                      (sleep 0.01)))
        (ignore-errors
          (localgroup-write-packet socket-stream '(:detach)))
        (ignore-errors (close socket-stream))
        (when (and receiver (thread-alive-p receiver))
          (ignore-errors (join-thread receiver))))))
  nil)

(-> localgroup--wait-for-handoff-entry
    (configuration string string integer)
    cons)
(defun localgroup--wait-for-handoff-entry
    (configuration session-id token old-pid)
  "Wait for SESSION-ID's authenticated replacement endpoint after OLD-PID."
  (let ((deadline
          (+ (get-internal-real-time)
             (* *localgroup-handoff-start-timeout-seconds*
                internal-time-units-per-second))))
    (loop
      (let* ((pathname
               (localgroup-registry-pathname configuration session-id))
             (record (localgroup--read-endpoint-record pathname)))
        (when (and record
                   (/= (getf (rest record) :pid) old-pid)
                   (string= (localgroup--record-token record) token)
                   (handler-case
                       (eq
                        (first
                         (localgroup-call
                          (localgroup--record-port record) token ':status))
                        ':ok)
                     (error () nil)))
          (return (cons pathname record))))
      (when (>= (get-internal-real-time) deadline)
        (error 'localgroup-error
               :message "The detached localgroup replacement did not become ready."
               :operation ':attach
               :session-id session-id))
      (sleep 0.05))))

(-> localgroup-attach-record (configuration cons keyword) null)
(defun localgroup-attach-record (configuration entry mode)
  "Attach the current terminal to endpoint ENTRY with MODE."
  (let* ((record (rest entry))
         (socket nil)
         (socket-stream nil)
         (terminal nil)
         (signal-installed-p nil))
    (unwind-protect
         (progn
           (multiple-value-setq (socket socket-stream)
             (localgroup-connect (localgroup--record-port record)))
           (multiple-value-bind (rows columns)
               (terminal-current-size)
             (localgroup-write-packet
              socket-stream
              (list :localgroup-request
                    :version *localgroup-protocol-version*
                    :token (localgroup--record-token record)
                    :operation ':attach
                    :arguments
                    (list :mode mode
                          :rows rows
                          :columns columns
                          :styled-p (terminal-environment-styling-p)))))
           (let ((response (localgroup-read-packet socket-stream)))
             (cond
               ((and response (eq (first response) ':handoff))
                (let ((session-id (getf (rest response) :session-id))
                      (old-pid (getf (rest response) :old-pid))
                      (token (localgroup--record-token record)))
                  (unless (and (non-empty-string-p session-id)
                               (typep old-pid '(integer 1)))
                    (error 'localgroup-error
                           :message "The localgroup handoff response was malformed."
                           :operation ':attach
                           :session-id
                           (localgroup--record-session-id record)))
                  (close socket-stream)
                  (setf socket-stream nil
                        socket nil)
                  (return-from localgroup-attach-record
                    (localgroup-attach-record
                     configuration
                     (localgroup--wait-for-handoff-entry
                      configuration session-id token old-pid)
                     ':control))))
               ((and response (eq (first response) ':attached))
                (let ((history (getf (rest response) :history)))
                  (when (stringp history)
                    (write-string history *standard-output*)
                    (finish-output *standard-output*))))
               (t
                (error 'localgroup-error
                       :message (or (and response
                                         (getf (rest response) :message))
                                    "The localgroup attachment was rejected.")
                       :operation ':attach
                       :session-id
                       (localgroup--record-session-id record)))))
           (setf terminal
                 (stream-terminal-create
                  :rows *terminal-default-rows*
                  :columns *terminal-default-columns*))
           (terminal-start terminal)
           (sb-sys:enable-interrupt
            sb-unix:sigwinch
            (lambda (signal code context)
              (declare (ignore signal code context))
              (setf *terminal-resize-pending-p* t)))
           (setf signal-installed-p t)
           (localgroup--attach-terminal-loop socket-stream terminal mode))
      (when signal-installed-p
        (sb-sys:enable-interrupt sb-unix:sigwinch :default))
      (when terminal
        (ignore-errors (terminal-stop terminal)))
      (when socket-stream
        (ignore-errors (close socket-stream)))
      (when (and socket (null socket-stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket)))))
  nil)


;;;; -- Localgroup Command Entry --

(-> main-localgroup (configuration list) null)
(defun main-localgroup (configuration arguments)
  "Run one noninteractive localgroup command described by ARGUMENTS."
  (configuration-ensure-directories configuration)
  (let ((operation (first arguments)))
    (cond
      ((or (null operation) (string= operation "status"))
       (let ((statuses (localgroup-statuses configuration)))
         (if (member "--sexp" arguments :test #'string=)
             (dolist (status statuses)
               (write status :stream *standard-output* :readably t)
               (terpri))
             (localgroup-print-statuses statuses))))
      ((string= operation "attach")
       (let* ((session-id (second arguments))
              (options (cddr arguments)))
         (unless session-id
           (error 'localgroup-error
                  :message "localgroup attach requires a session identifier."
                  :operation ':arguments))
         (when (some (lambda (option)
                       (not (member option '("--read-only" "--take-over")
                                    :test #'string=)))
                     options)
           (error 'localgroup-error
                  :message "localgroup attach accepts only --read-only or --take-over."
                  :operation ':arguments
                  :session-id session-id))
         (localgroup-attach-record
          configuration
          (localgroup--find-record configuration session-id)
          (localgroup--attachment-request-mode options))))
      ((member operation '("tell" "pause" "detach" "kill") :test #'string=)
       (let* ((session-id (second arguments))
              (entry
                (and session-id
                     (localgroup--find-record configuration session-id))))
         (unless session-id
           (error 'localgroup-error
                  :message (format nil "localgroup ~A requires a session identifier."
                                   operation)
                  :operation ':arguments))
         (let ((response
                 (cond
                   ((string= operation "tell")
                    (let ((message (third arguments)))
                      (unless (and message (= (length arguments) 3))
                        (error 'localgroup-error
                               :message "localgroup tell requires exactly one quoted message argument."
                               :operation ':arguments
                               :session-id session-id))
                      (localgroup-query-record
                       entry ':tell (list :message message))))
                   ((string= operation "pause")
                    (localgroup-query-record entry ':pause))
                   ((string= operation "detach")
                    (localgroup-query-record entry ':detach))
                   (t
                    (localgroup-query-record entry ':kill)))))
           (localgroup--print-response response *standard-output*))))
      (t
       (error 'localgroup-error
              :message (format nil "Unknown localgroup command ~S." operation)
              :operation ':arguments))))
  nil)
