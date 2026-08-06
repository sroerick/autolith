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
        (format stream "~&~-12A  ~-11A  ~-9A  ~-12A  ~A~%"
                "SESSION" "STATE" "CONVERSATION" "ACTIVITY" "WORKSPACE")
        (format stream "~A~%" (make-string 78 :initial-element #\-))
        (dolist (status statuses)
          (format stream "~-12A  ~-11A  ~-9A  ~-12A  ~A~%"
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
      ((member operation '("tell" "pause" "kill") :test #'string=)
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
                   (t
                    (localgroup-query-record entry ':kill)))))
           (localgroup--print-response response *standard-output*))))
      (t
       (error 'localgroup-error
              :message (format nil "Unknown localgroup command ~S." operation)
              :operation ':arguments))))
  nil)
