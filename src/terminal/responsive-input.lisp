(in-package #:autolith)

;;;; -- Responsive Terminal Input --

(defparameter *application-forced-interrupt-status* 130
  "The process status used when repeated Ctrl-C forces exit.")

(defparameter *application-interrupt-exit-window-seconds* 5/2
  "Seconds in which a second Ctrl-C may force the process to exit.")

(defparameter *application-interrupt-hint-delay-seconds* 1/4
  "Seconds a cancellation may run before its force-exit hint becomes worth showing.")

(-> application-input-controller--interrupt-window-text () string)
(defun application-input-controller--interrupt-window-text ()
  "Return the force-exit window as concise user-facing seconds."
  (format nil "~,1F" *application-interrupt-exit-window-seconds*))

(-> application-input-controller--forced-exit-text () string)
(defun application-input-controller--forced-exit-text ()
  "Return the repeated-Ctrl-C forced-exit explanation."
  (format nil
          "Ctrl-C pressed twice within ~A seconds; forcing Autolith to exit."
          (application-input-controller--interrupt-window-text)))

(-> application-input-controller--monotonic-seconds () real)
(defun application-input-controller--monotonic-seconds ()
  "Return monotonically increasing process time in seconds."
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(defparameter *application-interrupt-clock-function*
  #'application-input-controller--monotonic-seconds
  "The monotonic clock captured by newly created input controllers.")

(defvar *application-forced-exit-function*
  (lambda (status)
    (sb-ext:exit :code status :abort t))
  "The process termination boundary captured by each input controller.")

(defclass application-input-controller ()
  ((application
    :initarg :application
    :reader application-input-controller-application
    :type application
    :documentation "The application receiving terminal events and submitted work.")
   (lock
    :initform (make-lock "Autolith input controller")
    :reader application-input-controller-lock
    :type t
    :documentation "The lock protecting work, reader, and exit state.")
   (condition-variable
    :initform (make-condition-variable :name "Autolith input controller")
    :reader application-input-controller-condition-variable
    :type t
    :documentation "The main and reader thread wakeup condition.")
   (work-items
    :initarg :work-items
    :initform nil
    :accessor application-input-controller-work-items
    :type list
    :documentation "FIFO message and command work submitted by the reader.")
   (steering-items
    :initform nil
    :accessor application-input-controller-steering-items
    :type list
    :documentation "FIFO user messages waiting for the active turn's next tool boundary.")
   (follow-up-edit-index
    :initform nil
    :accessor application-input-controller-follow-up-edit-index
    :type (option (integer 0))
    :documentation
    "The recalled draft's position in the virtual queue that still includes it.")
   (follow-up-edit-work
    :initform nil
    :accessor application-input-controller-follow-up-edit-work
    :type (option list)
    :documentation "The queued work item currently recalled into the draft.")
   (later-state
    :initarg :later-state
    :reader application-input-controller-later-state
    :type later-state
    :documentation "The durable deferred inputs owned by this controller.")
   (pending-later-entries
    :initarg :pending-later-entries
    :accessor application-input-controller-pending-later-entries
    :type list
    :documentation "Deferred entries not currently dispatched by this process.")
   (active-p
    :initform nil
    :accessor application-input-controller-active-p
    :type boolean
    :documentation "Whether the main thread is processing one work item.")
   (localgroup-handoff-p
    :initform nil
    :accessor application-input-controller-localgroup-handoff-p
    :type boolean
    :documentation "Whether localgroup has stopped new input for process handoff.")
   (turn-cancellation-p
    :initform nil
    :accessor application-input-controller-turn-cancellation-p
    :type boolean
    :documentation "Whether one active turn is cancelling but not yet finished.")
   (turn-cancellation-delivery-pending-p
    :initform nil
    :accessor application-input-controller-turn-cancellation-delivery-pending-p
    :type boolean
    :documentation "Whether the main thread must still receive turn cancellation.")
   (stopping-p
    :initform nil
    :accessor application-input-controller-stopping-p
    :type boolean
    :documentation "Whether no more terminal input or work may be accepted.")
   (exit-reason
    :initform nil
    :accessor application-input-controller-exit-reason
    :type (option keyword)
    :documentation "The user-facing reason input processing stopped.")
   (reader-thread
    :initform nil
    :accessor application-input-controller-reader-thread
    :type t
    :documentation "The restartable terminal reader thread.")
   (reader-paused-p
    :initform nil
    :accessor application-input-controller-reader-paused-p
    :type boolean
    :documentation "Whether the reader must remain stopped for main-thread input.")
   (pause-depth
    :initform 0
    :accessor application-input-controller-pause-depth
    :type (integer 0)
    :documentation "Nested main-thread requests keeping the reader stopped.")
   (main-thread
    :initarg :main-thread
    :reader application-input-controller-main-thread
    :type t
    :documentation "The model and command thread interrupted for immediate exit.")
   (forced-exit-function
    :initarg :forced-exit-function
    :initform *application-forced-exit-function*
    :reader application-input-controller-forced-exit-function
    :type function
    :documentation "The process termination boundary captured before the reader starts.")
   (interrupt-clock-function
    :initarg :interrupt-clock-function
    :initform *application-interrupt-clock-function*
    :reader application-input-controller-interrupt-clock-function
    :type function
    :documentation "The monotonic clock used to recognize repeated Ctrl-C input.")
   (interrupt-deadline
    :initform nil
    :accessor application-input-controller-interrupt-deadline
    :type (option real)
    :documentation "The inclusive monotonic deadline for a force-exit Ctrl-C.")
   (interrupt-hint-time
    :initform nil
    :accessor application-input-controller-interrupt-hint-time
    :type (option real)
    :documentation "The monotonic time at which an unshown force-exit hint is due.")
   (forced-exit-message
    :initform (format nil "~%~A~%"
                      (application-input-controller--forced-exit-text))
    :accessor application-input-controller-forced-exit-message
    :type string
    :documentation "The complete plain-text notice emitted by forced shutdown.")
   (failure
    :initform nil
    :accessor application-input-controller-failure
    :type (option serious-condition)
    :documentation "A fatal terminal-reader condition awaiting main-thread handling.")
   (failure-backtrace
    :initform nil
    :accessor application-input-controller-failure-backtrace
    :type (option string)
    :documentation "The reader backtrace captured with FAILURE."))
  (:documentation
   "Ephemeral terminal input and FIFO submission state for one application run."))

(-> application--resume-command (application) string)
(defun application--resume-command (application)
  "Return the shell command that resumes APPLICATION's exact conversation."
  (format nil "autolith resume ~A"
          (uiop:escape-shell-token
           (conversation-identifier-display
            (conversation-identifier
             (application-conversation application))))))

(-> application-input-controller--force-interrupt-exit
    (application-input-controller)
    null)
(defun application-input-controller--force-interrupt-exit (controller)
  "Restore the terminal, emit the prepared notice, and force CONTROLLER to exit.

This emergency path deliberately avoids the terminal UI and its presentation
lock because ordinary shutdown may be blocked while either is unavailable."
  (unwind-protect
       (let* ((application
                (application-input-controller-application controller))
              (terminal
                (terminal-ui-terminal (application-ui application))))
         (ignore-errors
           (terminal-stop terminal))
         (ignore-errors
           (terminal--write-safe-text
            terminal
            (application-input-controller-forced-exit-message controller))
           (terminal-flush terminal)))
    (funcall (application-input-controller-forced-exit-function controller)
             *application-forced-interrupt-status*))
  nil)

(-> application-input-controller--forced-exit-message
    (application-input-controller keyword)
    string)
(defun application-input-controller--forced-exit-message (controller reason)
  "Return CONTROLLER's forced-exit notice for shutdown or cancellation REASON.

Forced exit abandons an unfinished run, so every reason carries the durable
resume command that reopens the conversation the run leaves behind."
  (let* ((application
           (application-input-controller-application controller))
         (conversation
           (and (slot-boundp application 'conversation)
                (application-conversation application)))
         (resume-command
           (and conversation
                (conversation-persisted-p conversation)
                (application--resume-command application))))
    (format nil
            "~%~A~%~@[To resume this conversation, run:~%  ~A~%~]"
            (if (eq reason ':turn-cancellation)
                (application-input-controller--forced-exit-text)
                "Ctrl-C pressed during shutdown; forcing Autolith to exit.")
            resume-command)))

(-> application-input-controller--show-interrupt-hint
    (application-input-controller real)
    boolean)
(defun application-input-controller--show-interrupt-hint
    (controller remaining-seconds)
  "Show CONTROLLER's force-exit notice for REMAINING-SECONDS and report display.

The notice expires with the force-exit window itself, and a contended
presentation lock reports no display so a later reader pass can try again."
  (let* ((application
           (application-input-controller-application controller))
         (ui (application-ui application))
         (shown-p nil))
    (ignore-errors
      (setf shown-p
            (nth-value
             1
             (terminal-ui-set-notice
              ui
              (format nil
                      "Press Ctrl-C again within ~A seconds to force exit."
                      (application-input-controller--interrupt-window-text))
              :duration-seconds remaining-seconds))))
    (not (null shown-p))))

(-> application-input-controller--refresh-interrupt-hint
    (application-input-controller)
    null)
(defun application-input-controller--refresh-interrupt-hint (controller)
  "Show CONTROLLER's force-exit hint once cancellation outlives the hint delay.

Waiting keeps a promptly cancelled turn from flashing an option it no longer
offers, so the hint appears only while forcing exit is still worth explaining."
  (let ((hint-time nil)
        (remaining-seconds nil))
    (with-lock-held ((application-input-controller-lock controller))
      (let ((due (application-input-controller-interrupt-hint-time controller))
            (deadline
              (application-input-controller-interrupt-deadline controller)))
        (when (and due deadline)
          (let ((now
                  (funcall
                   (application-input-controller-interrupt-clock-function
                    controller))))
            (when (>= now due)
              (setf hint-time due
                    remaining-seconds (- deadline now)))))))
    (when (and remaining-seconds
               (plusp remaining-seconds)
               (application-input-controller--show-interrupt-hint
                controller remaining-seconds))
      (with-lock-held ((application-input-controller-lock controller))
        ;; A newer press owns any hint time this pass did not observe.
        (when (eql (application-input-controller-interrupt-hint-time controller)
                   hint-time)
          (setf (application-input-controller-interrupt-hint-time controller)
                nil)))))
  nil)

(-> application-input-controller--prepare-shutdown
    (application-input-controller keyword)
    (values boolean boolean))
(defun application-input-controller--prepare-shutdown (controller reason)
  "Prepare CONTROLLER shutdown and report active and prepared state.

The first value reports whether a model or command turn needs cancellation. The
second reports whether shutdown was prepared."
  (let ((active-p nil)
        (prepared-p nil)
        (message
          (application-input-controller--forced-exit-message controller reason)))
    (with-lock-held ((application-input-controller-lock controller))
      (setf active-p (application-input-controller-active-p controller))
      (unless (application-input-controller-stopping-p controller)
        (setf prepared-p t)
        (unless (application-input-controller-exit-reason controller)
          (setf (application-input-controller-exit-reason controller) reason
                (application-input-controller-forced-exit-message controller)
                message))
        (when active-p
          (setf (application-input-controller-turn-cancellation-p controller) t
                (application-input-controller-turn-cancellation-delivery-pending-p
                 controller)
                t))
        ;; Persist unprocessed follow-ups before clearing process-local queues
        ;; so the next session can restore them for this conversation.
        (application-input-controller--persist-pending controller)
        (setf (application-input-controller-stopping-p controller) t
              (application-input-controller-work-items controller) nil
              (application-input-controller-steering-items controller) nil
              (application-input-controller-follow-up-edit-index controller) nil
              (application-input-controller-follow-up-edit-work controller) nil)
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller)))
      (values active-p prepared-p))))

(-> application-input--text ((or string user-message-input)) string)
(defun application-input--text (input)
  "Return the editable text carried by INPUT."
  (etypecase input
    (string input)
    (user-message-input (user-message-input-text input))))

(-> application-input--copy
    ((or string user-message-input))
    (or string user-message-input))
(defun application-input--copy (input)
  "Return a detached copy of INPUT."
  (etypecase input
    (string (copy-seq input))
    (user-message-input (user-message-input-copy input))))

(-> application-input--preview ((or string user-message-input)) string)
(defun application-input--preview (input)
  "Return INPUT's text for pending-work presentation."
  (etypecase input
    (string input)
    (user-message-input (user-message-input-preview input))))

(-> application--message-input
    ((or string user-message-input))
    (option (or string user-message-input)))
(defun application--message-input (input)
  "Return INPUT's model message, or NIL when it is empty or a slash command."
  (let ((text (application-input--text input)))
    (cond
      ((and (not (non-empty-string-p text))
            (not (and (typep input 'user-message-input)
                      (user-message-input-image-pathnames input))))
       nil)
      ((uiop:string-prefix-p "//" text)
       (etypecase input
         (string (subseq text 1))
         (user-message-input
          (user-message-input-create
           :text (subseq text 1)
           :image-pathnames (user-message-input-image-pathnames input)))))
      ((uiop:string-prefix-p "/" text)
       nil)
      (t
       (application-input--copy input)))))

(-> application-input-controller--follow-up-work-p (t) boolean)
(defun application-input-controller--follow-up-work-p (work)
  "Return true when WORK is an editable queued message or command."
  (and (consp work)
       (member (first work) '(:message :command) :test #'eq)
       (consp (rest work))
       (typep (second work) '(or string user-message-input))))

(-> application-input-controller--input-work
    ((or string user-message-input))
    (option list))
(defun application-input-controller--input-work (input)
  "Return queued work for INPUT, or NIL when INPUT has no effective content."
  (let ((message (application--message-input input))
        (text (application-input--text input)))
    (cond
      (message
       (list ':message message))
      ((non-empty-string-p text)
       (list ':command (copy-seq text)))
      (t
       nil))))

(-> application-input-controller--insert-work-at (list integer list) list)
(defun application-input-controller--insert-work-at (work-items index work)
  "Insert WORK at bounded INDEX in WORK-ITEMS without changing item identity."
  (let ((position (min index (length work-items))))
    (append (subseq work-items 0 position)
            (list work)
            (nthcdr position work-items))))

(-> application-input-controller--virtual-work-items
    (application-input-controller)
    list)
(defun application-input-controller--virtual-work-items (controller)
  "Return CONTROLLER's FIFO work including its recalled follow-up.

The caller must hold CONTROLLER's lock."
  (let ((work-items
          (application-input-controller-work-items controller))
        (index
          (application-input-controller-follow-up-edit-index controller))
        (work
          (application-input-controller-follow-up-edit-work controller)))
    (if (and index work)
        (application-input-controller--insert-work-at work-items index work)
        work-items)))

(-> application-input-controller--pending-work-form (list) list)
(defun application-input-controller--pending-work-form (work-items)
  "Return durable forms for WORK-ITEMS that can be restored after restart."
  (loop for work in work-items
        for kind = (first work)
        for input = (second work)
        when (and (member kind '(:message :command) :test #'eq)
                  (typep input '(or string user-message-input)))
          collect (list kind (application-input--text input))))

(-> application-input-controller--restore-work-items (list) list)
(defun application-input-controller--restore-work-items (forms)
  "Return in-memory work items restored from durable FORMS."
  (loop for form in forms
        when (and (listp form)
                  (member (first form) '(:message :command) :test #'eq)
                  (stringp (second form)))
          collect (list (first form) (copy-seq (second form)))))

(-> application-input-controller--persist-pending
    (application-input-controller)
    null)
(defun application-input-controller--persist-pending (controller)
  "Atomically publish CONTROLLER's unprocessed follow-ups and steering."
  (let* ((application (application-input-controller-application controller))
         (configuration
           (and (slot-boundp application 'configuration)
                (application-configuration application)))
         (conversation
           (and (slot-boundp application 'conversation)
                (application-conversation application))))
    (when (and (typep configuration 'configuration)
               (typep conversation 'conversation))
      (handler-case
          (let* ((pathname
                  (configuration-pending-inputs-path
                   configuration (conversation-pathname conversation)))
                 (steering
                   (mapcar #'application-input--text
                           (application-input-controller-steering-items
                            controller)))
                 (work
                   (application-input-controller--pending-work-form
                    (application-input-controller--virtual-work-items
                     controller)))
                 (form
                   (list :pending-inputs
                         :version 1
                         :conversation-id (conversation-identifier conversation)
                         :steering steering
                         :work work)))
            (if (and (null steering) (null work))
                (when (probe-file pathname)
                  (delete-file pathname))
                (progn
                  (ensure-directories-exist pathname)
                  (snapshot-write pathname form))))
        (error ()
          nil))))
  nil)

(-> application-input-controller--load-pending
    (application-input-controller)
    null)
(defun application-input-controller--load-pending (controller)
  "Restore unprocessed follow-ups and steering for CONTROLLER's conversation."
  (let* ((application (application-input-controller-application controller))
         (configuration
           (and (slot-boundp application 'configuration)
                (application-configuration application)))
         (conversation
           (and (slot-boundp application 'conversation)
                (application-conversation application))))
    (when (and (typep configuration 'configuration)
               (typep conversation 'conversation))
      (let* ((pathname
               (configuration-pending-inputs-path
                configuration (conversation-pathname conversation)))
             (legacy-pathname
               (configuration-legacy-pending-inputs-path configuration))
             (source-pathname
               (cond
                 ((probe-file pathname) pathname)
                 ((probe-file legacy-pathname) legacy-pathname)
                 (t nil))))
        (when source-pathname
          (handler-case
              (multiple-value-bind (form complete-p)
                  (snapshot-read source-pathname)
                (when (and complete-p
                           (listp form)
                           (eq (first form) :pending-inputs)
                           (= (or (getf (rest form) :version) 0) 1)
                           (string= (getf (rest form) :conversation-id)
                                    (conversation-identifier conversation)))
                  (let ((steering (getf (rest form) :steering))
                        (work (getf (rest form) :work)))
                    (when (listp steering)
                      (setf (application-input-controller-steering-items
                             controller)
                            (mapcar #'copy-seq
                                    (remove-if-not #'stringp steering))))
                    (when (listp work)
                      (setf (application-input-controller-work-items controller)
                            (append
                             (application-input-controller--restore-work-items
                              work)
                             (application-input-controller-work-items
                              controller)))))
                  (when (equal source-pathname legacy-pathname)
                    (application-input-controller--persist-pending controller)
                    (when (probe-file pathname)
                      (delete-file legacy-pathname)))))
            (error ()
              nil))))))
  nil)

(-> application-input-controller--publish-counts
    (application-input-controller)
    null)
(defun application-input-controller--publish-counts (controller)
  "Publish CONTROLLER's pending input previews through its serialized UI.

A stopping controller has already finalized its durable pending-input snapshot.
Skipping a later publisher prevents an accepted enqueue from deleting that
snapshot after shutdown cleared the process-local queues."
  (with-lock-held ((application-input-controller-lock controller))
    (terminal-ui-set-pending-inputs
     (application-ui (application-input-controller-application controller))
     (mapcar #'application-input--preview
             (application-input-controller-steering-items controller))
     (loop for work in (application-input-controller-work-items controller)
           for input = (second work)
           when (typep input '(or string user-message-input))
             collect (application-input--preview input)))
    (unless (application-input-controller-stopping-p controller)
      (application-input-controller--persist-pending controller)))
  nil)

(-> application-input-controller-turn-active-p
    (application-input-controller)
    boolean)
(defun application-input-controller-turn-active-p (controller)
  "Return true when CONTROLLER's main thread is processing one work item."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (application-input-controller-active-p controller)))))

(-> application-input-controller-busy-p
    (application-input-controller)
    boolean)
(defun application-input-controller-busy-p (controller)
  "Return true when CONTROLLER has active or pending application work."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (or (application-input-controller-active-p controller)
          (application-input-controller-work-items controller)
          (application-input-controller-follow-up-edit-work controller))))))

(-> application-input-controller--follow-up-editing-p
    (application-input-controller)
    boolean)
(defun application-input-controller--follow-up-editing-p (controller)
  "Return true when CONTROLLER has recalled one queued follow-up into the draft."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (application-input-controller-follow-up-edit-index controller)))))

(-> application-input-controller--clear-follow-up-edit
    (application-input-controller)
    null)
(defun application-input-controller--clear-follow-up-edit (controller)
  "Discard CONTROLLER's recalled follow-up without changing its draft."
  (with-lock-held ((application-input-controller-lock controller))
    (setf (application-input-controller-follow-up-edit-index controller) nil
          (application-input-controller-follow-up-edit-work controller) nil)
    (sb-thread:condition-broadcast
     (application-input-controller-condition-variable controller)))
  (application-input-controller--publish-counts controller)
  nil)

(-> application-input-controller--interrupt-main
    (application-input-controller condition)
    null)
(defun application-input-controller--interrupt-main (controller condition)
  "Signal CONDITION on CONTROLLER's main thread unless already running there."
  (let ((thread (application-input-controller-main-thread controller)))
    (unless (eq thread (current-thread))
      (when (thread-alive-p thread)
        (interrupt-thread thread (lambda () (error condition))))))
  nil)

(-> application-input-controller--consume-turn-cancellation-delivery-p
    (application-input-controller)
    boolean)
(defun application-input-controller--consume-turn-cancellation-delivery-p
    (controller)
  "Atomically consume and report CONTROLLER's pending cancellation delivery."
  (eq (sb-ext:compare-and-swap
       (slot-value controller 'turn-cancellation-delivery-pending-p)
       t
       nil)
      t))

(-> application-input-controller--interrupt-main-for-turn-cancellation
    (application-input-controller)
    null)
(defun application-input-controller--interrupt-main-for-turn-cancellation
    (controller)
  "Promptly deliver CONTROLLER's pending turn cancellation to its main thread."
  (let ((thread (application-input-controller-main-thread controller)))
    (unless (eq thread (current-thread))
      (when (thread-alive-p thread)
        (interrupt-thread
         thread
         (lambda ()
           (when
               (application-input-controller--consume-turn-cancellation-delivery-p
                controller)
             (error (make-condition 'application-turn-cancelled))))))))
  nil)

(-> application-input-controller--record-failure
    (application-input-controller serious-condition (option string))
    null)
(defun application-input-controller--record-failure
    (controller condition backtrace)
  "Record reader CONDITION, discard pending work, and wake the main thread."
  (let ((active-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-failure controller)
        (setf (application-input-controller-failure controller) condition
              (application-input-controller-failure-backtrace controller) backtrace
              (application-input-controller-work-items controller) nil
              (application-input-controller-steering-items controller) nil
              (application-input-controller-follow-up-edit-index controller) nil
              (application-input-controller-follow-up-edit-work controller) nil
              (application-input-controller-stopping-p controller) t)
        ;; Reader failure deliberately discards queued work, unlike ordinary
        ;; shutdown, so finalize an empty durable snapshot before publishers
        ;; begin skipping stopping controllers.
        (application-input-controller--persist-pending controller))
      (setf active-p (application-input-controller-active-p controller))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (application-input-controller--publish-counts controller)
    (when active-p
      (handler-case
          (application-input-controller--interrupt-main
           controller
           (make-condition
            'application-input-failed
            :original-condition condition
            :backtrace backtrace))
        (error ()
          nil))))
  nil)

(-> application-input-controller--enqueue
    (application-input-controller keyword (or string user-message-input))
    boolean)
(defun application-input-controller--enqueue (controller kind input)
  "Append one work item of KIND carrying INPUT and report acceptance."
  (let ((queued-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (or (application-input-controller-stopping-p controller)
                  (application-input-controller-localgroup-handoff-p controller))
        (setf (application-input-controller-work-items controller)
              (nconc (application-input-controller-work-items controller)
                     (list (list kind (application-input--copy input))))
              queued-p t)
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller))))
    (application-input-controller--publish-counts controller)
    queued-p))

(-> application-input-controller--queue-input
    (application-input-controller (or string user-message-input))
    boolean)
(defun application-input-controller--queue-input (controller input)
  "Queue INPUT, restoring a recalled follow-up to its virtual FIFO position."
  (let ((work (application-input-controller--input-work input))
        (queued-p nil))
    (when work
      (with-lock-held ((application-input-controller-lock controller))
        (unless (or (application-input-controller-stopping-p controller)
                    (application-input-controller-localgroup-handoff-p controller))
          (let ((index
                  (application-input-controller-follow-up-edit-index controller)))
            (setf (application-input-controller-work-items controller)
                  (if index
                      (application-input-controller--insert-work-at
                       (application-input-controller-work-items controller)
                       index
                       work)
                      (nconc
                       (application-input-controller-work-items controller)
                       (list work)))
                  (application-input-controller-follow-up-edit-index controller) nil
                  (application-input-controller-follow-up-edit-work controller) nil
                  queued-p t))
          (sb-thread:condition-broadcast
           (application-input-controller-condition-variable controller))))
      (application-input-controller--publish-counts controller))
    queued-p))

(-> application-input-controller--enqueue-steering
    (application-input-controller (or string user-message-input))
    null)
(defun application-input-controller--enqueue-steering (controller input)
  "Queue INPUT for the active turn, or promote it before follow-ups if that turn ended."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (or (application-input-controller-stopping-p controller)
                (application-input-controller-localgroup-handoff-p controller))
      (if (application-input-controller-active-p controller)
          (setf (application-input-controller-steering-items controller)
                (nconc (application-input-controller-steering-items controller)
                       (list (application-input--copy input))))
          (progn
            (push (list ':message (application-input--copy input))
                  (application-input-controller-work-items controller))
            (when (application-input-controller-follow-up-edit-index controller)
              (incf
               (application-input-controller-follow-up-edit-index
                controller)))))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller))))
  (application-input-controller--publish-counts controller)
  nil)

(-> application-input-controller--take-steering
    (application-input-controller)
    list)
(defun application-input-controller--take-steering (controller)
  "Return and consume CONTROLLER's messages for the completed tool boundary."
  (let ((messages nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-stopping-p controller)
        (setf messages (application-input-controller-steering-items controller)
              (application-input-controller-steering-items controller) nil)))
    (application-input-controller--publish-counts controller)
    messages))

(-> application-input-controller--request-exit
    (application-input-controller keyword)
    null)
(defun application-input-controller--request-exit (controller reason)
  "Stop CONTROLLER for REASON, discarding work and cancelling an active turn."
  (multiple-value-bind (active-p prepared-p)
      (application-input-controller--prepare-shutdown controller reason)
    (when (and active-p prepared-p)
      (handler-case
          (application-input-controller--interrupt-main-for-turn-cancellation
           controller)
        (error ()
          nil))))
  nil)

(-> application-input-controller--turn-cancellation-active-p
    (application-input-controller)
    boolean)
(defun application-input-controller--turn-cancellation-active-p (controller)
  "Return true while CONTROLLER is still finishing active-turn cancellation."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (application-input-controller-turn-cancellation-p controller)))))

(-> application-input-controller--request-active-turn-cancellation
    (application-input-controller &key (:force-exit-window-p boolean))
    boolean)
(defun application-input-controller--request-active-turn-cancellation
    (controller &key force-exit-window-p)
  "Atomically request cancellation only for CONTROLLER's current active turn.

When FORCE-EXIT-WINDOW-P is true, arm the repeated-Ctrl-C option before
cancellation can finish and schedule the hint that explains it."
  (let ((accepted-p nil)
        (message
          (and force-exit-window-p
               (application-input-controller--forced-exit-message
                controller ':turn-cancellation))))
    (with-lock-held ((application-input-controller-lock controller))
      (when (and (application-input-controller-active-p controller)
                 (not (application-input-controller-stopping-p controller))
                 (not
                  (application-input-controller-turn-cancellation-p controller)))
        (setf (application-input-controller-turn-cancellation-p controller) t
              (application-input-controller-turn-cancellation-delivery-pending-p
               controller)
              t
              accepted-p t)
        (when force-exit-window-p
          (let ((now
                  (funcall
                   (application-input-controller-interrupt-clock-function
                    controller))))
            (setf (application-input-controller-interrupt-deadline controller)
                  (+ now *application-interrupt-exit-window-seconds*)
                  (application-input-controller-interrupt-hint-time controller)
                  (+ now *application-interrupt-hint-delay-seconds*)
                  (application-input-controller-forced-exit-message controller)
                  message)))))
    (when accepted-p
      (handler-case
          (application-input-controller--interrupt-main-for-turn-cancellation
           controller)
        (error ()
          nil)))
    accepted-p))

(-> application-input-controller--active-turn-interrupt-action
    (application-input-controller)
    (option keyword))
(defun application-input-controller--active-turn-interrupt-action (controller)
  "Return and apply CONTROLLER's active-cancellation Ctrl-C action, if any.

The forced-exit notice is prepared outside the lock because a lapsed press
re-arms a window that a wedged turn may never let ordinary shutdown reach."
  (let ((message
          (application-input-controller--forced-exit-message
           controller ':turn-cancellation)))
    (with-lock-held ((application-input-controller-lock controller))
      (when (application-input-controller-turn-cancellation-p controller)
        (let* ((now
                 (funcall
                  (application-input-controller-interrupt-clock-function
                   controller)))
               (deadline
                 (application-input-controller-interrupt-deadline controller)))
          (if (and deadline (<= now deadline))
              (progn
                (setf (application-input-controller-interrupt-deadline controller)
                      nil
                      (application-input-controller-interrupt-hint-time controller)
                      nil)
                ':force)
              (progn
                ;; This press already outlived one window, so the wedged turn
                ;; has earned its hint at the reader's next opportunity.
                (setf (application-input-controller-interrupt-deadline controller)
                      (+ now *application-interrupt-exit-window-seconds*)
                      (application-input-controller-interrupt-hint-time controller)
                      now
                      (application-input-controller-forced-exit-message controller)
                      message)
                ':hint)))))))

(-> application-input-controller--present-scheduled-command
    (application-input-controller string)
    null)
(defun application-input-controller--present-scheduled-command (controller input)
  "Present the busy-command scheduling result for INPUT."
  (let ((invocation (application-command-invocation-parse input)))
    (application-command--call-with-presentation
     invocation
     (lambda ()
       (application-present
        (application-input-controller-application controller)
        (list
         (terminal-span
          ':hint
          "∙ command scheduled until the current response finishes")
         (terminal-span ':plain (string #\Newline))
         (terminal-span
          ':dim
          "  It runs at the first opportunity. Empty Tab edits it; Shift-Tab cycles."))))))
  nil)

(-> application-input-controller--schedule-command
    (application-input-controller string)
    null)
(defun application-input-controller--schedule-command (controller input)
  "Queue busy command INPUT to run at the first idle opportunity."
  (when (application-input-controller--enqueue controller ':command input)
    (application-input-controller--present-scheduled-command controller input))
  nil)

(-> application-input-controller--run-responsive-command
    (application-input-controller application-command
     application-command-invocation)
    keyword)
(defun application-input-controller--run-responsive-command
    (controller command invocation)
  "Execute an immediate COMMAND without converting its errors on the reader."
  (let ((application
          (application-input-controller-application controller)))
    (application-command--call-with-presentation
     invocation
     (lambda ()
       (handler-case
           (application-command-execute command application invocation)
         (autolith-error (condition)
           (application-present
            application
            (application--expected-error-entry application condition))
           ':failed))))))

(-> application-input-controller--handle-recalled-submission
    (application-input-controller (or string user-message-input))
    boolean)
(defun application-input-controller--handle-recalled-submission
    (controller input)
  "Atomically route recalled INPUT and report whether recalled work handled it.

Blank input keeps the recalled work selected. Active messages commit to the
current turn's steering queue before its completion can race. Active commands
commit their busy policy before the recalled work is removed."
  (let* ((message (application--message-input input))
         (text (application-input--text input))
         (work (application-input-controller--input-work input))
         (invocation
           (and (null message)
                (non-empty-string-p text)
                (application-command-invocation-parse text)))
         (command
           (and invocation
                (application-command-invocation-command invocation)))
         (busy-action
           (and invocation
                (if command
                    (application-command-busy-action command invocation)
                    ':hold)))
         (handled-p nil)
         (changed-p nil)
         (post-action nil))
    (with-lock-held ((application-input-controller-lock controller))
      (let ((index
              (application-input-controller-follow-up-edit-index controller))
            (held-work
              (application-input-controller-follow-up-edit-work controller)))
        (when (and index held-work)
          (setf handled-p t)
          (when work
            (if (application-input-controller-active-p controller)
                (cond
                  (message
                   (setf (application-input-controller-steering-items controller)
                         (nconc
                          (application-input-controller-steering-items controller)
                          (list (application-input--copy message)))))
                  ((eq busy-action ':hold)
                   (setf (application-input-controller-work-items controller)
                         (nconc
                          (application-input-controller-work-items controller)
                          (list work))
                         post-action ':hold))
                  (t
                   (setf post-action busy-action)))
                (setf (application-input-controller-work-items controller)
                      (application-input-controller--insert-work-at
                       (application-input-controller-work-items controller)
                       index
                       work)))
            (setf (application-input-controller-follow-up-edit-index controller) nil
                  (application-input-controller-follow-up-edit-work controller) nil
                  changed-p t)
            (sb-thread:condition-broadcast
             (application-input-controller-condition-variable controller))))))
    (when changed-p
      (application-input-controller--publish-counts controller))
    (case post-action
      (:cancel
       (application-input-controller--request-exit controller ':quit))
      (:execute
       (when (eq (application-input-controller--run-responsive-command
                  controller command invocation)
                 ':quit)
         (application-input-controller--request-exit controller ':quit)))
      (:hold
       (application-input-controller--present-scheduled-command controller text)))
    handled-p))

(-> application-input-controller--handle-submission
    (application-input-controller (or string user-message-input)
     &key (:steer-p boolean))
    null)
(defun application-input-controller--handle-submission
    (controller input &key steer-p)
  "Route submitted INPUT to model work, command work, or busy-command policy."
  (application-localgroup-resume
   (application-input-controller-application controller))
  (let ((message (application--message-input input))
        (text (application-input--text input)))
    (cond
      (message
       (if steer-p
           (application-input-controller--enqueue-steering controller message)
           (application-input-controller--enqueue controller ':message message)))
      ((not (non-empty-string-p text))
       nil)
      ((application-input-controller-busy-p controller)
       (let* ((invocation (application-command-invocation-parse text))
              (command
                (application-command-invocation-command invocation))
              (action
                (if command
                    (application-command-busy-action command invocation)
                    ':hold)))
         (ecase action
           (:cancel
            (application-input-controller--request-exit controller ':quit))
           (:execute
            (when (eq (application-input-controller--run-responsive-command
                       controller command invocation)
                      ':quit)
              (application-input-controller--request-exit controller ':quit)))
           (:hold
            (application-input-controller--schedule-command controller text)))))
      (t
       (application-input-controller--enqueue controller ':command text))))
  nil)

(-> application-input-controller--handle-queue-submission
    (application-input-controller (or string user-message-input))
    null)
(defun application-input-controller--handle-queue-submission (controller input)
  "Queue INPUT as post-turn work, preserving a recalled follow-up's position."
  (application-input-controller--queue-input controller input)
  nil)

(-> application-input-controller--recall-follow-up
    (application-input-controller)
    boolean)
(defun application-input-controller--recall-follow-up (controller)
  "Recall CONTROLLER's newest follow-up into the editor for revision."
  (let ((work nil)
        (steering-inputs nil)
        (queued-inputs nil))
    (with-lock-held ((application-input-controller-lock controller))
      (when (and (application-input-controller-active-p controller)
                 (null
                  (application-input-controller-follow-up-edit-index controller)))
        (let* ((work-items
                 (application-input-controller-work-items controller))
               (index
                 (position-if
                  #'application-input-controller--follow-up-work-p
                  work-items
                  :from-end t)))
          (when index
            (setf work (nth index work-items)
                  (application-input-controller-work-items controller)
                  (loop for queued-work in work-items
                        for position from 0
                        unless (= position index)
                          collect queued-work)
                  (application-input-controller-follow-up-edit-index controller)
                  index
                  (application-input-controller-follow-up-edit-work controller)
                  work
                  steering-inputs
                  (copy-list
                   (application-input-controller-steering-items controller))
                  queued-inputs
                  (loop for queued-work
                          in (application-input-controller-work-items controller)
                        for input = (second queued-work)
                        when (typep input '(or string user-message-input))
                          collect (application-input--preview input)))))))
    (when work
      (terminal-ui-recall-follow-up
       (application-ui (application-input-controller-application controller))
       (second work)
       :steering-inputs steering-inputs
       :queued-inputs queued-inputs))
    (not (null work))))

(-> application-input-controller--cycle-follow-up
    (application-input-controller (or string user-message-input))
    boolean)
(defun application-input-controller--cycle-follow-up (controller input)
  "Move CONTROLLER's recalled draft to the previous queued follow-up, wrapping."
  (let ((current-work (application-input-controller--input-work input))
        (selected-work nil)
        (steering-inputs nil)
        (queued-inputs nil))
    (when current-work
      (with-lock-held ((application-input-controller-lock controller))
        (let ((index
                (application-input-controller-follow-up-edit-index controller)))
          (when (and index
                     (application-input-controller-follow-up-edit-work controller))
            (let* ((work-items
                     (application-input-controller-work-items controller))
                   (current-position (min index (length work-items)))
                   (full-work-items
                     (application-input-controller--insert-work-at
                      work-items current-position current-work))
                   (eligible-positions
                     (loop for work in full-work-items
                           for position from 0
                           when (and
                                 (/= position current-position)
                                 (application-input-controller--follow-up-work-p
                                  work))
                             collect position))
                   (selected-position
                     (or
                      (find-if (lambda (position)
                                 (< position current-position))
                               eligible-positions
                               :from-end t)
                      (first (last eligible-positions)))))
              (if selected-position
                  (setf selected-work (nth selected-position full-work-items)
                        (application-input-controller-work-items controller)
                        (loop for work in full-work-items
                              for position from 0
                              unless (= position selected-position)
                                collect work)
                        (application-input-controller-follow-up-edit-index controller)
                        selected-position
                        (application-input-controller-follow-up-edit-work controller)
                        selected-work
                        steering-inputs
                        (copy-list
                         (application-input-controller-steering-items controller))
                        queued-inputs
                        (loop for queued-work
                                in (application-input-controller-work-items controller)
                              for queued-input = (second queued-work)
                              when (typep queued-input
                                          '(or string user-message-input))
                                collect
                                (application-input--preview queued-input)))
                  (setf (application-input-controller-follow-up-edit-work controller)
                        current-work))
              (sb-thread:condition-broadcast
               (application-input-controller-condition-variable controller))))))
    (when selected-work
      (terminal-ui-recall-follow-up
       (application-ui (application-input-controller-application controller))
       (second selected-work)
       :steering-inputs steering-inputs
       :queued-inputs queued-inputs))
    (when current-work
      (application-input-controller--publish-counts controller))
    (not (null selected-work)))))

(-> application-input-controller--process-event
    (application-input-controller t)
    null)
(defun application-input-controller--process-event (controller event)
  "Apply terminal EVENT and publish any resulting work or exit request."
  (let ((ui (application-ui
             (application-input-controller-application controller)))
        (follow-up-editing-p
          (application-input-controller--follow-up-editing-p controller)))
    (if (and (eq event ':interrupt) follow-up-editing-p)
        (progn
          (terminal-ui-process-event
           ui event :queue-editing-p follow-up-editing-p)
          (application-input-controller--clear-follow-up-edit controller))
        (let ((active-interrupt-action
                (and (eq event ':interrupt)
                     (application-input-controller--active-turn-interrupt-action
                      controller))))
          (cond
            ((eq active-interrupt-action ':force)
             (application-input-controller--force-interrupt-exit controller))
            ((eq active-interrupt-action ':hint)
             nil)
            ((and (eq event ':interrupt)
                  (application-input-controller--request-active-turn-cancellation
                   controller :force-exit-window-p t))
             nil)
            ((and (eq event ':escape)
                  (or
                   (application-input-controller--turn-cancellation-active-p
                    controller)
                   (application-input-controller--request-active-turn-cancellation
                    controller)))
             nil)
            (t
             (let ((turn-active-p
                     (application-input-controller-turn-active-p controller)))
               (multiple-value-bind (action payload)
                   (terminal-ui-process-event
                    ui
                    event
                    :queue-completion-p turn-active-p
                    :queue-editing-p follow-up-editing-p)
                 (case action
                   (:cleared
                    (application-input-controller--clear-follow-up-edit controller))
                   (:submit
                    (unless
                        (application-input-controller--handle-recalled-submission
                         controller payload)
                      (application-input-controller--handle-submission
                       controller
                       payload
                       :steer-p
                       (application-input-controller-turn-active-p controller))))
                   (:queue
                    (application-input-controller--handle-queue-submission
                     controller payload))
                   (:edit-queue
                    (application-input-controller--recall-follow-up controller))
                   (:cycle-queue
                    (application-input-controller--cycle-follow-up
                     controller payload))
                   (:end-of-input
                    (application-input-controller--request-exit
                     controller ':end-of-input))
                   (:interrupt
                    (application-input-controller--request-exit
                     controller ':interrupt))))))))))
  nil)

(-> application-input-controller--input-ready-p
    (application-input-controller)
    boolean)
(defun application-input-controller--input-ready-p (controller)
  "Apply pending resizes and report whether CONTROLLER's terminal has input."
  (let* ((ui (application-ui
              (application-input-controller-application controller)))
         (terminal (terminal-ui-terminal ui)))
    (terminal-ui-refresh-size ui #'application-pending-terminal-size)
    (terminal-ui-refresh-status ui)
    (if (terminal-input-ready-p terminal)
        t
        (progn
          (with-lock-held ((application-input-controller-lock controller))
            (unless (or (application-input-controller-stopping-p controller)
                        (application-input-controller-reader-paused-p controller))
              (condition-wait
               (application-input-controller-condition-variable controller)
               (application-input-controller-lock controller)
               :timeout 0.02)))
          nil))))

(-> application-input-controller--reader-loop
    (application-input-controller)
    null)
(defun application-input-controller--reader-loop (controller)
  "Read events until pause, failure, or a completed interrupt escalation."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (loop
            (application-input-controller--refresh-interrupt-hint controller)
            (multiple-value-bind (stopping-p reader-paused-p)
                (with-lock-held
                    ((application-input-controller-lock controller))
                  (values
                   (application-input-controller-stopping-p controller)
                   (application-input-controller-reader-paused-p controller)))
              (cond
                (reader-paused-p
                 (return))
                (stopping-p
                 (let* ((application
                          (application-input-controller-application controller))
                        (ui (application-ui application))
                        (terminal (terminal-ui-terminal ui)))
                   (terminal-ui-refresh-status ui)
                   (if (terminal-input-ready-p terminal)
                       (case (terminal-read-event terminal)
                         (:interrupt
                          (application-input-controller--force-interrupt-exit
                           controller))
                         (:escape
                          nil)
                         (:end-of-input
                          (return)))
                       (with-lock-held
                           ((application-input-controller-lock controller))
                         (unless
                             (application-input-controller-reader-paused-p
                              controller)
                           (condition-wait
                            (application-input-controller-condition-variable
                             controller)
                            (application-input-controller-lock controller)
                            :timeout 0.02))))))
                ((application-input-controller--input-ready-p controller)
                 (application-input-controller--process-event
                  controller
                  (application-read-terminal-event
                   (application-ui
                    (application-input-controller-application controller))))))))
        (serious-condition (condition)
          (application-input-controller--record-failure
           controller condition signal-backtrace)))))
  nil)

(-> application-input-controller--start-reader
    (application-input-controller)
    null)
(defun application-input-controller--start-reader (controller)
  "Start CONTROLLER's reader unless it is paused, stopping, or already live."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (or (application-input-controller-stopping-p controller)
                (application-input-controller-reader-paused-p controller)
                (let ((thread
                        (application-input-controller-reader-thread controller)))
                  (and thread (thread-alive-p thread))))
      (setf (application-input-controller-reader-thread controller)
            (make-thread
             (lambda ()
               (application-input-controller--reader-loop controller))
             :name "Autolith terminal input"))))
  nil)

(-> application-input-controller--pause-reader
    (application-input-controller)
    null)
(defun application-input-controller--pause-reader (controller)
  "Stop and join CONTROLLER's reader without ending the application."
  (let ((thread nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-reader-paused-p controller) t
            thread (application-input-controller-reader-thread controller))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (when thread
      (join-thread thread)
      (with-lock-held ((application-input-controller-lock controller))
        (when (eq thread
                  (application-input-controller-reader-thread controller))
          (setf (application-input-controller-reader-thread controller) nil)))))
  nil)

(-> application-input-controller-call-with-reader-paused
    (application-input-controller function)
    t)
(defun application-input-controller-call-with-reader-paused
    (controller function)
  "Call FUNCTION while CONTROLLER has no live terminal reader."
  (let ((outermost-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf outermost-p
            (zerop (application-input-controller-pause-depth controller)))
      (incf (application-input-controller-pause-depth controller)))
    (when outermost-p
      (application-input-controller--pause-reader controller))
    (unwind-protect
         (funcall function)
      (let ((restart-p nil))
        (with-lock-held ((application-input-controller-lock controller))
          (decf (application-input-controller-pause-depth controller))
          (when (zerop (application-input-controller-pause-depth controller))
            (setf (application-input-controller-reader-paused-p controller) nil
                  restart-p
                  (not (application-input-controller-stopping-p controller)))))
        (when restart-p
          (application-input-controller--start-reader controller))))))

(-> application--command-authorization-items (string pathname) list)
(defun application--command-authorization-items (command directory)
  "Return the modal choices for COMMAND in DIRECTORY."
  (declare (ignore command))
  (list
   (list :name "once"
         :argument nil
         :description "allow once inside the workspace sandbox")
   (list :name "always"
         :argument nil
         :description
         (format nil "always allow this exact command in ~A"
                 (application--abbreviated-directory (namestring directory))))
   (list :name "sandbox"
         :argument nil
         :description "allow sandboxed commands for this session")
   (list :name "full"
         :argument nil
         :description "let it ride with full user privileges for this session")
   (list :name "deny"
         :argument nil
         :description "do not run the command")))

(-> application--ask-command-permission
    (application string pathname)
    keyword)
(defun application--ask-command-permission (application command directory)
  "Ask interactively how COMMAND may run in DIRECTORY, failing closed otherwise."
  (block nil
    (let* ((controller (application-input-controller application))
           (ui         (application-ui application)))
      (unless (and controller
                   ui
                   (terminal-interactive-p (terminal-ui-terminal ui)))
        (return ':deny))
      (let ((choice
              (application-input-controller-call-with-reader-paused
               controller
               (lambda ()
                 (terminal-ui-select
                  ui
                  :title
                  (format nil "run ~A"
                          (text-cell-prefix
                           (sanitize-text command :single-line-p t)
                           56))
                  :items (application--command-authorization-items
                          command directory)
                  :resize-callback #'application-pending-terminal-size)))))
        (cond
          ((string= (or choice "") "once")
           ':sandboxed)
          ((string= (or choice "") "always")
           (permissions-allow
            :configuration (application-configuration application)
            :state         (application-permission-state application)
            :command       command
            :directory     directory)
           ':sandboxed)
          ((string= (or choice "") "sandbox")
           (setf (application-permission-mode application) ':sandboxed)
           ':sandboxed)
          ((string= (or choice "") "full")
           (setf (application-permission-mode application) ':full-access)
           ':full-access)
          (t
           ':deny))))))

(-> application-authorize-command (application string pathname) keyword)
(defun application-authorize-command (application command directory)
  "Return the session, saved, or interactively selected permission for COMMAND."
  (with-lock-held ((application-command-authorization-lock application))
    (case (application-permission-mode application)
      (:full-access
       ':full-access)
      (:sandboxed
       ':sandboxed)
      (:ask
       (if (permissions-allowed-p
            (application-permission-state application)
            command
            directory)
           ':sandboxed
           (application--ask-command-permission
            application command directory))))))

(-> application--tool-authorization-title (tool) string)
(defun application--tool-authorization-title (tool)
  "Return the complete identity of TOOL as one approval-picker title."
  (format nil
          "allow ~{~A~^, ~}"
          (mapcar
           (lambda (field)
             (destructuring-bind (label value) field
               (format nil "~A ~S" label value)))
           (tool-authorization-identity-fields tool))))

(-> application--tool-authorization-request-entry
    (tool json-object)
    string)
(defun application--tool-authorization-request-entry (tool arguments)
  "Render TOOL identity and complete ARGUMENTS for one approval request."
  (with-output-to-string (stream)
    (format stream "External tool approval requested.~%")
    (dolist (field (tool-authorization-identity-fields tool))
      (destructuring-bind (label value) field
        (format stream "  ~A  ~S~%" label value)))
    (format stream "  arguments  ~A" (json-encode arguments))))

(-> application--tool-authorization-items () list)
(defun application--tool-authorization-items ()
  "Return the modal choices for one fully displayed external tool request."
  (list
   (list :name "allow"
         :argument nil
         :description "allow this one call with the arguments shown above")
   (list :name "deny"
         :argument nil
         :description "do not call the external tool")))

(-> application--ask-tool-permission
    (application tool json-object)
    keyword)
(defun application--ask-tool-permission (application tool arguments)
  "Ask interactively whether external TOOL may run, failing closed otherwise."
  (block nil
    (let* ((controller (application-input-controller application))
           (ui         (application-ui application)))
      (unless (and controller
                   ui
                   (terminal-interactive-p (terminal-ui-terminal ui)))
        (return ':deny))
      (let ((choice
              (application-input-controller-call-with-reader-paused
               controller
               (lambda ()
                 (application-present
                  application
                  (application--tool-authorization-request-entry
                   tool arguments))
                 (terminal-ui-select
                  ui
                  :title (application--tool-authorization-title tool)
                  :items (application--tool-authorization-items)
                  :resize-callback #'application-pending-terminal-size)))))
        (if (string= (or choice "") "allow")
            ':allow
            ':deny)))))

(-> application-authorize-tool (application tool json-object) keyword)
(defun application-authorize-tool (application tool arguments)
  "Return the interactively selected permission for one external TOOL call."
  (with-lock-held ((application-command-authorization-lock application))
    (application--ask-tool-permission application tool arguments)))

(-> application-input-controller-schedule-later
    (application-input-controller string &key (:due-at timestamp) (:window string))
    later-entry)
(defun application-input-controller-schedule-later
    (controller input &key due-at window)
  "Persist INPUT for DUE-AT and wake CONTROLLER's deferred scheduler."
  (let* ((application (application-input-controller-application controller))
         (configuration (application-configuration application))
         (entry
           (later-schedule
            :configuration configuration
            :state (application-input-controller-later-state controller)
            :input input
            :directory (configuration-working-directory configuration)
            :due-at due-at
            :window window)))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-pending-later-entries controller)
            (later--sort-entries
             (append
              (application-input-controller-pending-later-entries controller)
              (list entry))))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    entry))

(-> application-input-controller-cancel-later
    (application-input-controller string)
    boolean)
(defun application-input-controller-cancel-later (controller identifier)
  "Cancel deferred IDENTIFIER durably and remove it from CONTROLLER."
  (let* ((application (application-input-controller-application controller))
         (cancelled-p
           (later-cancel
            (application-configuration application)
            (application-input-controller-later-state controller)
            identifier)))
    (when cancelled-p
      (with-lock-held ((application-input-controller-lock controller))
        (setf (application-input-controller-pending-later-entries controller)
              (remove identifier
                      (application-input-controller-pending-later-entries
                       controller)
                      :key #'later-entry-identifier
                      :test #'string=))
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller))))
    cancelled-p))

(-> application-input-controller--promote-due-later
    (application-input-controller timestamp)
    null)
(defun application-input-controller--promote-due-later (controller now)
  "Move CONTROLLER's entries due at NOW onto its ordinary work queue."
  (loop for entry = (first
                     (application-input-controller-pending-later-entries
                      controller))
        while (and entry (<= (later-entry-due-at entry) now))
        do (pop (application-input-controller-pending-later-entries controller))
           (setf (application-input-controller-work-items controller)
                 (nconc (application-input-controller-work-items controller)
                        (list (list ':later entry)))))
  nil)

(-> application-input-controller--later-wait-seconds
    (application-input-controller timestamp)
    (option real))
(defun application-input-controller--later-wait-seconds (controller now)
  "Return seconds until CONTROLLER's next deferred entry, if one exists."
  (let ((entry (first
                (application-input-controller-pending-later-entries controller))))
    (and entry (max 0.01 (- (later-entry-due-at entry) now)))))

(-> application-input-controller--complete-later
    (application-input-controller later-entry)
    null)
(defun application-input-controller--complete-later (controller entry)
  "Remove successfully dispatched ENTRY from durable deferred state."
  (later-cancel
   (application-configuration
    (application-input-controller-application controller))
   (application-input-controller-later-state controller)
   (later-entry-identifier entry))
  nil)

(-> application-input-controller--retry-later
    (application-input-controller later-entry)
    null)
(defun application-input-controller--retry-later (controller entry)
  "Reschedule failed ENTRY from current rate data or a five-minute fallback."
  (let* ((application (application-input-controller-application controller))
         (configuration (application-configuration application))
         (provider (application-provider application))
         (now (get-universal-time)))
    (multiple-value-bind (reset-at window)
        (later-reset-deadline (and provider (provider-rate-limits provider))
                              :now now)
      (let ((replacement
              (later-reschedule
               :configuration configuration
               :state (application-input-controller-later-state controller)
               :entry entry
               :due-at (if (and reset-at (> reset-at now))
                           reset-at
                           (+ now 300))
               :window (if (and window reset-at (> reset-at now))
                           window
                           "5 minute retry"))))
        (with-lock-held ((application-input-controller-lock controller))
          (setf (application-input-controller-pending-later-entries controller)
                (later--sort-entries
                 (append
                  (application-input-controller-pending-later-entries controller)
                  (list replacement))))
          (sb-thread:condition-broadcast
           (application-input-controller-condition-variable controller)))
        (application-present
         application
         (format nil "Deferred input ~A was rescheduled after ~A."
                 (later-entry-identifier replacement)
                 (later-entry-window replacement))))))
  nil)

(-> application-input-controller-create
    (application &key (:initial-work-items list))
    application-input-controller)
(defun application-input-controller-create
    (application &key initial-work-items)
  "Create CONTROLLER for APPLICATION and start its terminal reader."
  (let* ((configuration
           (and (slot-boundp application 'configuration)
                (application-configuration application)))
         (later-state
           (if (typep configuration 'configuration)
               (later-load configuration)
               (make-instance 'later-state)))
         (controller
           (make-instance 'application-input-controller
                          :application application
                          :work-items (copy-tree initial-work-items)
                          :later-state later-state
                          :pending-later-entries
                          (copy-list (later-state-entries later-state))
                          :main-thread (current-thread))))
    (setf (application-input-controller application) controller)
    (application-input-controller--load-pending controller)
    (application-input-controller--publish-counts controller)
    (application-input-controller--start-reader controller)
    controller))

(-> application-input-controller--next-work
    (application-input-controller)
    (option list))
(defun application-input-controller--next-work (controller)
  "Wait for and return CONTROLLER's next work item, or NIL after exit."
  (let ((application (application-input-controller-application controller))
        (work nil))
    (with-lock-held ((application-input-controller-lock controller))
      (loop
        (application-input-controller--promote-due-later
         controller (get-universal-time))
        (when (or (application-input-controller-failure controller)
                  (application-input-controller-stopping-p controller))
          (return))
        (cond
          ((and (not (application-localgroup-paused-p application))
                (null (application-input-controller-work-items controller))
                (null (application-input-controller-steering-items controller))
                (null
                 (application-input-controller-follow-up-edit-index controller)))
           (let ((mode (application-localgroup-take-ready-handoff application)))
             (when mode
               (setf work (list ':localgroup-handoff mode)
                     (application-input-controller-active-p controller) t)
               (return))))
          ((and (not (application-localgroup-paused-p application))
                (application-input-controller-work-items controller)
                (not
                 (eql
                  (application-input-controller-follow-up-edit-index controller)
                  0)))
           (setf work (pop (application-input-controller-work-items controller))
                 (application-input-controller-active-p controller) t)
           (when (application-input-controller-follow-up-edit-index controller)
             (decf
              (application-input-controller-follow-up-edit-index controller)))
           (return)))
        (let* ((later-wait
                 (application-input-controller--later-wait-seconds
                  controller (get-universal-time)))
               (handoff-wait
                 (and (application-localgroup-handoff-pending-p application)
                      1/10))
               (wait-seconds
                 (cond ((and later-wait handoff-wait)
                        (min later-wait handoff-wait))
                       (later-wait later-wait)
                       (handoff-wait handoff-wait))))
          (if wait-seconds
              (condition-wait
               (application-input-controller-condition-variable controller)
               (application-input-controller-lock controller)
               :timeout wait-seconds)
              (condition-wait
               (application-input-controller-condition-variable controller)
               (application-input-controller-lock controller)))))
      (when (application-input-controller-failure controller)
        (error
         'application-input-failed
         :original-condition (application-input-controller-failure controller)
         :backtrace (application-input-controller-failure-backtrace controller))))
    (application-input-controller--publish-counts controller)
    work))

(-> application-input-controller--finish-work
    (application-input-controller)
    null)
(defun application-input-controller--finish-work (controller)
  "Finish current work and promote unconsumed steering before queued follow-ups."
  (let ((clear-notice-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-stopping-p controller)
        (let ((steering-items
                (application-input-controller-steering-items controller)))
          (when steering-items
            (setf (application-input-controller-work-items controller)
                  (append (mapcar (lambda (input)
                                    (list ':message input))
                                  steering-items)
                          (application-input-controller-work-items controller)))
            (when (application-input-controller-follow-up-edit-index controller)
              (incf
               (application-input-controller-follow-up-edit-index controller)
               (length steering-items))))
        (setf (application-input-controller-steering-items controller) nil)))
      (setf clear-notice-p
            (or (application-input-controller-turn-cancellation-p controller)
                (application-input-controller-interrupt-deadline controller))
            (application-input-controller-active-p controller) nil
            (application-input-controller-turn-cancellation-p controller) nil
            (application-input-controller-turn-cancellation-delivery-pending-p
             controller)
            nil
            (application-input-controller-interrupt-deadline controller) nil
            (application-input-controller-interrupt-hint-time controller) nil)
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (when clear-notice-p
      (terminal-ui-set-notice
       (application-ui (application-input-controller-application controller))
       nil))
    (application-input-controller--publish-counts controller))
  nil)

(-> application-input-controller-stop (application-input-controller) null)
(defun application-input-controller-stop (controller)
  "Retire CONTROLLER after shutdown work is complete and join its reader."
  (let ((thread nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-stopping-p controller) t
            (application-input-controller-reader-paused-p controller) t
            (application-input-controller-work-items controller) nil
            (application-input-controller-steering-items controller) nil
            (application-input-controller-pending-later-entries controller) nil
            (application-input-controller-active-p controller) nil
            (application-input-controller-follow-up-edit-index controller) nil
            (application-input-controller-follow-up-edit-work controller) nil
            (application-input-controller-turn-cancellation-p controller) nil
            (application-input-controller-turn-cancellation-delivery-pending-p
             controller)
            nil
            (application-input-controller-interrupt-deadline controller) nil
            (application-input-controller-interrupt-hint-time controller) nil
            thread (application-input-controller-reader-thread controller))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (when thread
      (join-thread thread)
      (with-lock-held ((application-input-controller-lock controller))
        (when (eq thread
                  (application-input-controller-reader-thread controller))
          (setf (application-input-controller-reader-thread controller) nil))))
    (let ((application (application-input-controller-application controller)))
      (when (eq controller (application-input-controller application))
        (setf (application-input-controller application) nil))))
  nil)

(-> application-input-controller-call-with-shutdown-escape
    (application-input-controller function)
    t)
(defun application-input-controller-call-with-shutdown-escape
    (controller function)
  "Call potentially blocking shutdown FUNCTION while Ctrl-C remains effective.

Normal editor input is already disabled while FUNCTION runs. The controller's
reader stays alive in interrupt-only mode until FUNCTION returns or unwinds."
  (application-input-controller--prepare-shutdown controller ':shutdown)
  (unwind-protect
       (funcall function)
    (application-input-controller-stop controller)))

(-> application--run-message-input
    (application (or string user-message-input)
     &key (:steering-function (option function))
          (:tools-p boolean)
          (:tool-allowlist (option list))
          (:tool-restriction-p boolean)
          (:goal-continuations-p boolean)
          (:fatal-agent-loop-errors-p boolean))
    keyword)
(defun application--run-message-input
    (application input &key steering-function (tools-p t)
                            tool-allowlist (tool-restriction-p nil)
                            (goal-continuations-p t)
                            (fatal-agent-loop-errors-p t))
  "Run model INPUT with established expected, cancellation, and fatal handling."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (progn
            (application-run-message
             application
             input
             :steering-function steering-function
             :tools-p tools-p
             :tool-allowlist tool-allowlist
             :tool-restriction-p tool-restriction-p
             :goal-continuations-p goal-continuations-p)
            ':continue)
        (application-turn-cancelled (condition)
          (error condition))
        (application-input-failed (condition)
          (error condition))
        (rollback-requested (condition)
          (error condition))
        (agent-loop-error (condition)
          (if fatal-agent-loop-errors-p
              (application-raise-fatal
               application condition signal-backtrace)
              (progn
                (application-handle-expected-error application condition)
                ':failed)))
        ((or conversation-invariant-error
             active-image-corruption)
         (condition)
          (application-raise-fatal application condition signal-backtrace))
        (autolith-error (condition)
          (application-handle-expected-error application condition)
          (if (provider-rate-limit-error-p condition)
              ':rate-limited
              ':failed))
        (serious-condition (condition)
          (application-raise-fatal application condition signal-backtrace))))))

(-> application--run-command-input (application string) keyword)
(defun application--run-command-input (application input)
  "Run command INPUT with established expected and fatal handling."
  (let ((signal-backtrace nil)
        (invocation (application-command-invocation-parse input)))
    (application-command--call-with-presentation
     invocation
     (lambda ()
       (handler-bind
           ((serious-condition
              (lambda (condition)
                (declare (ignore condition))
                (setf signal-backtrace (application-safe-backtrace)))))
         (handler-case
             (application-handle-input application input)
           (application-turn-cancelled (condition)
             (error condition))
           (application-input-failed (condition)
             (error condition))
           (rollback-requested (condition)
             (error condition))
           ((or agent-loop-error
                conversation-invariant-error
                active-image-corruption)
            (condition)
             (application-raise-fatal application condition signal-backtrace))
           (autolith-error (condition)
             (application-handle-expected-error application condition)
             ':failed)
           (serious-condition (condition)
             (application-raise-fatal
              application condition signal-backtrace))))))))

(-> application-input-controller--run-later
    (application-input-controller later-entry)
    null)
(defun application-input-controller--run-later (controller entry)
  "Dispatch due deferred ENTRY and durably complete or retry it."
  (block nil
    (let* ((application (application-input-controller-application controller))
           (input (later-entry-input entry)))
      (application-present
       application
       (format nil "Running deferred input ~A after its ~A reset.~%  ~A"
               (later-entry-identifier entry)
               (later-entry-window entry)
               (text-cell-prefix
                (sanitize-text input :single-line-p t)
                72)))
      (handler-case
          (application-set-working-directory
           application (later-entry-directory entry))
        (autolith-error (condition)
          (application-handle-expected-error application condition)
          (handler-case
              (application-input-controller--complete-later controller entry)
            (later-error (persistence-condition)
              (application-handle-expected-error application
                                                 persistence-condition)))
          (return nil)))
      (let* ((message (application--message-input input))
             (result
              (if message
                  (application--run-message-input application message)
                  (application--run-command-input application input))))
        (handler-case
            (if (member result '(:failed :rate-limited) :test #'eq)
                (application-input-controller--retry-later controller entry)
                (application-input-controller--complete-later controller entry))
          (later-error (condition)
            (application-handle-expected-error application condition)))
        (when (eq result ':quit)
          (application-input-controller--request-exit controller ':quit)))))
  nil)

(-> application-input-controller--defer-after-rate-limit
    (application-input-controller)
    null)
(defun application-input-controller--defer-after-rate-limit (controller)
  "Move remaining queued work into durable deferred inputs after a 429.

A failed rate-limited turn must not immediately submit the next follow-up.
Queued messages, commands, and unconsumed steering become later entries at the
provider reset deadline, or a five-minute fallback when no reset is known."
  (let* ((application (application-input-controller-application controller))
         (configuration (application-configuration application))
         (provider (application-provider application))
         (directory (configuration-working-directory configuration))
         (now (get-universal-time))
         (deferred-count 0))
    (multiple-value-bind (reset-at window)
        (later-reset-deadline (and provider (provider-rate-limits provider))
                              :now now)
      (let ((due-at (if (and reset-at (> reset-at now))
                        reset-at
                        (+ now 300)))
            (window-label (if (and window reset-at (> reset-at now))
                              window
                              "5 minute retry"))
            (pending nil))
        (with-lock-held ((application-input-controller-lock controller))
          (setf pending
                (append
                 (mapcar (lambda (input)
                           (list ':message input))
                         (application-input-controller-steering-items controller))
                 (application-input-controller-work-items controller))
                (application-input-controller-steering-items controller) nil
                (application-input-controller-work-items controller) nil))
        (dolist (item pending)
          (let ((input
                  (case (first item)
                    (:message
                     (let ((message (second item)))
                       (etypecase message
                         (string message)
                         (user-message-input
                          (user-message-input-text message)))))
                    (:command (second item))
                    (t nil))))
            (when (non-empty-string-p input)
              (handler-case
                  (let ((entry
                          (later-schedule
                           :configuration configuration
                           :state (application-input-controller-later-state
                                   controller)
                           :input input
                           :directory directory
                           :due-at due-at
                           :window window-label)))
                    (with-lock-held
                        ((application-input-controller-lock controller))
                      (setf (application-input-controller-pending-later-entries
                             controller)
                            (later--sort-entries
                             (append
                              (application-input-controller-pending-later-entries
                               controller)
                              (list entry))))
                      (sb-thread:condition-broadcast
                       (application-input-controller-condition-variable
                        controller)))
                    (incf deferred-count))
                (later-error (condition)
                  (application-handle-expected-error application condition))))))
        (application-input-controller--publish-counts controller)
        (when (plusp deferred-count)
          (application-present
           application
           (format nil
                   "Deferred ~D queued follow-up~:P until the ~A reset."
                   deferred-count
                   window-label))))))
  nil)

(-> application-input-controller--run-work
    (application-input-controller list)
    null)
(defun application-input-controller--run-work (controller work)
  "Run one submitted WORK item on the application main thread."
  (let ((application (application-input-controller-application controller)))
    (case (first work)
      (:message
       (let ((result
               (application--run-message-input
                application
                (second work)
                :steering-function
                (lambda ()
                  (application-input-controller--take-steering controller)))))
         (when (eq result ':rate-limited)
           (application-input-controller--defer-after-rate-limit controller))))
      (:recovery-diagnosis
       (application--run-message-input
        application
        (second work)
        :tools-p t
        :tool-allowlist *application-recovery-diagnostic-tool-names*
        :tool-restriction-p t
        :goal-continuations-p nil
        :fatal-agent-loop-errors-p nil))
      (:command
       (let* ((input (second work))
              (invocation (application-command-invocation-parse input))
              (command
                (application-command-invocation-command invocation))
              (result
                (if (and command
                         (application-command-terminal-owner-p
                          command invocation))
                    (application-input-controller-call-with-reader-paused
                     controller
                     (lambda ()
                       (application--run-command-input application input)))
                    (application--run-command-input application input))))
         (when (eq result ':quit)
           (application-input-controller--request-exit controller ':quit))))
      (:project-adaptation-offer
       (application-maybe-offer-project-adaptation application))
      (:localgroup-handoff
       (handler-case
           (application-localgroup-run-handoff
            application (second work) controller)
         (localgroup-error (condition)
           (application-handle-expected-error application condition))))
      (:later
       (application-input-controller--run-later controller (second work)))))
  nil)
