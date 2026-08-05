(in-package #:autolith)

;;;; -- Localgroup Attachment Transport --

(defparameter *localgroup-attachment-queue-character-limit* (* 4 1024 1024)
  "The maximum pending output retained for one attached client.")

(defparameter *localgroup-terminal-history-character-limit* (* 1024 1024)
  "The maximum rendered terminal output replayed to a new attachment.")

(defparameter *localgroup-terminal-output-chunk-character-limit* (* 64 1024)
  "The maximum terminal output characters carried in one attachment packet.")

(defclass localgroup-attachment ()
  ((socket
    :initarg :socket
    :reader localgroup-attachment-socket
    :type sb-bsd-sockets:socket
    :documentation "The accepted loopback socket owned by this attachment.")
   (stream
    :initarg :stream
    :reader localgroup-attachment-stream
    :type stream
    :documentation "The duplex UTF-8 packet stream.")
   (mode
    :initarg :mode
    :reader localgroup-attachment-mode
    :type (member :read-only :control :take-over)
    :documentation "The observation or terminal-control authority of this attachment.")
   (lock
    :initform (make-lock "Autolith localgroup attachment")
    :reader localgroup-attachment-lock
    :type t
    :documentation "The lock protecting output and closure state.")
   (condition-variable
    :initform (make-condition-variable :name "Autolith localgroup attachment")
    :reader localgroup-attachment-condition-variable
    :type t
    :documentation "The condition waking the output writer.")
   (queue
    :initform nil
    :accessor localgroup-attachment-queue
    :type list
    :documentation "FIFO serialized packets awaiting the writer.")
   (queued-characters
    :initform 0
    :accessor localgroup-attachment-queued-characters
    :type (integer 0)
    :documentation "The combined size of packets awaiting the writer.")
   (closed-p
    :initform nil
    :accessor localgroup-attachment-closed-p
    :type boolean
    :documentation "Whether the stream no longer accepts packets.")
   (writer-thread
    :initform nil
    :accessor localgroup-attachment-writer-thread
    :type t
    :documentation "The bounded asynchronous output writer."))
  (:documentation "One persistent read-only or controlling terminal attachment."))

(-> localgroup-attachment--close-stream (localgroup-attachment) null)
(defun localgroup-attachment--close-stream (attachment)
  "Close ATTACHMENT's duplex stream without signaling."
  (ignore-errors (close (localgroup-attachment-stream attachment)))
  nil)

(-> localgroup-attachment--writer-loop (localgroup-attachment) null)
(defun localgroup-attachment--writer-loop (attachment)
  "Write ATTACHMENT's queued packets until closure or transport failure."
  (handler-case
      (loop
        for packet =
          (with-lock-held ((localgroup-attachment-lock attachment))
            (loop while (and (null (localgroup-attachment-queue attachment))
                             (not (localgroup-attachment-closed-p attachment)))
                  do (condition-wait
                      (localgroup-attachment-condition-variable attachment)
                      (localgroup-attachment-lock attachment)))
            (when (and (localgroup-attachment-closed-p attachment)
                       (null (localgroup-attachment-queue attachment)))
              (return-from localgroup-attachment--writer-loop nil))
            (let ((packet (pop (localgroup-attachment-queue attachment))))
              (decf (localgroup-attachment-queued-characters attachment)
                    (length packet))
              packet))
        do (write-string packet (localgroup-attachment-stream attachment))
           (finish-output (localgroup-attachment-stream attachment)))
    (error ()
      nil))
  (with-lock-held ((localgroup-attachment-lock attachment))
    (setf (localgroup-attachment-closed-p attachment) t
          (localgroup-attachment-queue attachment) nil
          (localgroup-attachment-queued-characters attachment) 0)
    (condition-notify (localgroup-attachment-condition-variable attachment)))
  (localgroup-attachment--close-stream attachment)
  nil)

(-> localgroup-attachment-create
    (sb-bsd-sockets:socket stream keyword)
    localgroup-attachment)
(defun localgroup-attachment-create (socket stream mode)
  "Create a persistent attachment over SOCKET and STREAM with MODE."
  (let ((attachment
          (make-instance 'localgroup-attachment
                         :socket socket
                         :stream stream
                         :mode mode)))
    (setf (localgroup-attachment-writer-thread attachment)
          (make-thread
           (lambda () (localgroup-attachment--writer-loop attachment))
           :name "Autolith localgroup attachment output"))
    attachment))

(-> localgroup-attachment-send (localgroup-attachment list) boolean)
(defun localgroup-attachment-send (attachment packet)
  "Queue PACKET for ATTACHMENT, closing a client that falls too far behind."
  (let ((text (localgroup--packet-string packet))
        (accepted-p nil)
        (close-p nil))
    (with-lock-held ((localgroup-attachment-lock attachment))
      (unless (localgroup-attachment-closed-p attachment)
        (if (> (+ (localgroup-attachment-queued-characters attachment)
                  (length text))
               *localgroup-attachment-queue-character-limit*)
            (setf (localgroup-attachment-closed-p attachment) t
                  (localgroup-attachment-queue attachment) nil
                  (localgroup-attachment-queued-characters attachment) 0
                  close-p t)
            (setf (localgroup-attachment-queue attachment)
                  (nconc (localgroup-attachment-queue attachment)
                         (list text))
                  (localgroup-attachment-queued-characters attachment)
                  (+ (localgroup-attachment-queued-characters attachment)
                     (length text))
                  accepted-p t))
        (condition-notify
         (localgroup-attachment-condition-variable attachment))))
    (when close-p
      (localgroup-attachment--close-stream attachment))
    accepted-p))

(-> localgroup-attachment-close (localgroup-attachment) null)
(defun localgroup-attachment-close (attachment)
  "Close ATTACHMENT and join its writer when called from another thread."
  (let ((writer nil))
    (with-lock-held ((localgroup-attachment-lock attachment))
      (setf (localgroup-attachment-closed-p attachment) t
            (localgroup-attachment-queue attachment) nil
            (localgroup-attachment-queued-characters attachment) 0
            writer (localgroup-attachment-writer-thread attachment))
      (condition-notify
       (localgroup-attachment-condition-variable attachment)))
    (localgroup-attachment--close-stream attachment)
    (when (and writer
               (not (eq writer (current-thread)))
               (thread-alive-p writer))
      (ignore-errors (join-thread writer))))
  nil)


;;;; -- Relay Terminal --

(defclass localgroup-terminal (terminal)
  ((lock
    :initform (make-lock "Autolith localgroup terminal")
    :reader localgroup-terminal-lock
    :type t
    :documentation "The lock protecting terminal ownership, input, and history.")
   (direct-terminal
    :initarg :direct-terminal
    :accessor localgroup-terminal-direct-terminal
    :type (option stream-terminal)
    :documentation "The terminal inherited from the launching foreground process.")
   (controller
    :initform nil
    :accessor localgroup-terminal-controller
    :type (option localgroup-attachment)
    :documentation "The attachment currently allowed to submit terminal input.")
   (observers
    :initform nil
    :accessor localgroup-terminal-observers
    :type list
    :documentation "Read-only attachments receiving rendered terminal output.")
   (input-events
    :initform nil
    :accessor localgroup-terminal-input-events
    :type list
    :documentation "FIFO semantic events received from the controlling attachment.")
   (history
    :initform nil
    :accessor localgroup-terminal-history
    :type list
    :documentation "Bounded terminal output chunks ordered from oldest to newest.")
   (history-characters
    :initform 0
    :accessor localgroup-terminal-history-characters
    :type (integer 0)
    :documentation "The combined character count of retained history chunks.")
   (wake-function
    :initform nil
    :accessor localgroup-terminal-wake-function
    :type (option function)
    :documentation "The callback waking the responsive input controller."))
  (:documentation "A terminal relay supporting foreground, detached, and observed use."))

(-> localgroup-terminal-create
    (&optional (option stream-terminal))
    localgroup-terminal)
(defun localgroup-terminal-create (&optional direct-terminal)
  "Create a relay terminal initially owned by DIRECT-TERMINAL when supplied."
  (make-instance 'localgroup-terminal
                 :direct-terminal direct-terminal
                 :rows (if direct-terminal
                           (terminal-rows direct-terminal)
                           *terminal-default-rows*)
                 :columns (if direct-terminal
                              (terminal-columns direct-terminal)
                              *terminal-default-columns*)
                 :interactive-p
                 (and direct-terminal
                      (terminal-interactive-p direct-terminal))
                 :styled-p
                 (and direct-terminal
                      (terminal-styled-p direct-terminal))))

(-> localgroup-terminal-set-wake-function
    (localgroup-terminal (option function))
    null)
(defun localgroup-terminal-set-wake-function (terminal function)
  "Set TERMINAL's responsive-reader wake FUNCTION."
  (with-lock-held ((localgroup-terminal-lock terminal))
    (setf (localgroup-terminal-wake-function terminal) function))
  nil)

(-> localgroup-terminal--output-chunks (string) list)
(defun localgroup-terminal--output-chunks (text)
  "Return TEXT as ordered bounded attachment-output chunks."
  (loop with length = (length text)
        for start = 0 then end
        while (< start length)
        for end = (min length
                       (+ start
                          *localgroup-terminal-output-chunk-character-limit*))
        collect (subseq text start end)))

(-> localgroup-terminal--retain-output (localgroup-terminal string) null)
(defun localgroup-terminal--retain-output (terminal text)
  "Append TEXT to TERMINAL's exactly bounded attachment replay history while locked."
  (setf (localgroup-terminal-history terminal)
        (nconc (localgroup-terminal-history terminal) (list text)))
  (incf (localgroup-terminal-history-characters terminal) (length text))
  (loop while (> (localgroup-terminal-history-characters terminal)
                 *localgroup-terminal-history-character-limit*)
        for excess = (- (localgroup-terminal-history-characters terminal)
                        *localgroup-terminal-history-character-limit*)
        for first = (first (localgroup-terminal-history terminal))
        do (if (<= (length first) excess)
               (progn
                 (pop (localgroup-terminal-history terminal))
                 (decf (localgroup-terminal-history-characters terminal)
                       (length first)))
               (progn
                 (setf (first (localgroup-terminal-history terminal))
                       (subseq first excess))
                 (decf (localgroup-terminal-history-characters terminal) excess))))
  nil)

(-> localgroup-terminal-history-text (localgroup-terminal) string)
(defun localgroup-terminal-history-text (terminal)
  "Return a consistent copy of TERMINAL's retained rendered output."
  (with-lock-held ((localgroup-terminal-lock terminal))
    (apply #'concatenate 'string
           (cons "" (copy-list (localgroup-terminal-history terminal))))))

(defmethod terminal-start ((terminal localgroup-terminal))
  "Start TERMINAL's inherited direct transport when one exists."
  (with-lock-held ((localgroup-terminal-lock terminal))
    (when (terminal-started-p terminal)
      (return-from terminal-start terminal))
    (let ((direct (localgroup-terminal-direct-terminal terminal)))
      (when direct
        (terminal-start direct)
        (setf (terminal-rows terminal) (terminal-rows direct)
              (terminal-columns terminal) (terminal-columns direct)
              (terminal-interactive-p terminal) (terminal-interactive-p direct)
              (terminal-styled-p terminal) (terminal-styled-p direct)))
      (setf (terminal-started-p terminal) t)))
  terminal)

(defmethod terminal-stop ((terminal localgroup-terminal))
  "Stop TERMINAL's transports and close attachments, retaining direct transport."
  (let ((direct nil)
        (attachments nil))
    (with-lock-held ((localgroup-terminal-lock terminal))
      (unless (terminal-started-p terminal)
        (return-from terminal-stop terminal))
      (setf direct (localgroup-terminal-direct-terminal terminal)
            attachments
            (remove-duplicates
             (append (when (localgroup-terminal-controller terminal)
                       (list (localgroup-terminal-controller terminal)))
                     (localgroup-terminal-observers terminal))
             :test #'eq)
            (localgroup-terminal-controller terminal) nil
            (localgroup-terminal-observers terminal) nil
            (localgroup-terminal-input-events terminal) nil
            (terminal-interactive-p terminal) nil
            (terminal-styled-p terminal) nil
            (terminal-started-p terminal) nil))
    (when direct
      (ignore-errors (terminal-stop direct)))
    (dolist (attachment attachments)
      (localgroup-attachment-close attachment)))
  terminal)

(defmethod terminal--write ((terminal localgroup-terminal) (text string))
  "Write trusted TEXT to the current terminal and every observer in exact order."
  (with-lock-held ((localgroup-terminal-lock terminal))
    (let ((direct (localgroup-terminal-direct-terminal terminal))
          (attachments
            (remove-duplicates
             (append (when (localgroup-terminal-controller terminal)
                       (list (localgroup-terminal-controller terminal)))
                     (localgroup-terminal-observers terminal))
             :test #'eq)))
      (when direct
        (terminal--write direct text))
      (dolist (chunk (localgroup-terminal--output-chunks text))
        (localgroup-terminal--retain-output terminal chunk)
        (dolist (attachment attachments)
          (localgroup-attachment-send attachment (list :output chunk))))))
  nil)

(defmethod terminal-flush ((terminal localgroup-terminal))
  "Flush TERMINAL's direct transport when attached."
  (with-lock-held ((localgroup-terminal-lock terminal))
    (let ((direct (localgroup-terminal-direct-terminal terminal)))
      (when direct
        (terminal-flush direct))))
  nil)

(defmethod terminal-input-ready-p ((terminal localgroup-terminal))
  "Return true when TERMINAL has a queued or direct input event."
  (let ((direct nil)
        (queued-p nil))
    (with-lock-held ((localgroup-terminal-lock terminal))
      (setf queued-p (not (null (localgroup-terminal-input-events terminal)))
            direct (localgroup-terminal-direct-terminal terminal)))
    (or queued-p
        (and direct (terminal-input-ready-p direct)))))

(defmethod terminal-read-event ((terminal localgroup-terminal))
  "Read one queued attachment event or delegate to the direct terminal."
  (let ((event nil)
        (direct nil))
    (with-lock-held ((localgroup-terminal-lock terminal))
      (if (localgroup-terminal-input-events terminal)
          (setf event (pop (localgroup-terminal-input-events terminal)))
          (setf direct (localgroup-terminal-direct-terminal terminal))))
    (cond (event event)
          (direct (terminal-read-event direct))
          (t ':end-of-input))))

(-> localgroup-terminal-enqueue-event
    (localgroup-terminal localgroup-attachment t)
    boolean)
(defun localgroup-terminal-enqueue-event (terminal attachment event)
  "Queue ATTACHMENT's controlling EVENT for TERMINAL."
  (let ((wake-function nil))
    (with-lock-held ((localgroup-terminal-lock terminal))
      (unless (eq attachment (localgroup-terminal-controller terminal))
        (return-from localgroup-terminal-enqueue-event nil))
      (setf (localgroup-terminal-input-events terminal)
            (nconc (localgroup-terminal-input-events terminal) (list event))
            wake-function (localgroup-terminal-wake-function terminal)))
    (when wake-function
      (funcall wake-function))
    t))

(-> localgroup-terminal-resize
    (localgroup-terminal integer integer boolean)
    null)
(defun localgroup-terminal-resize (terminal rows columns styled-p)
  "Apply controlling client dimensions and styling to TERMINAL."
  (with-lock-held ((localgroup-terminal-lock terminal))
    (when (plusp rows)
      (setf (terminal-rows terminal) rows))
    (when (plusp columns)
      (setf (terminal-columns terminal) columns))
    (setf (terminal-interactive-p terminal) t
          (terminal-styled-p terminal) (not (null styled-p))))
  nil)

(-> localgroup-terminal-release-direct (localgroup-terminal) boolean)
(defun localgroup-terminal-release-direct (terminal)
  "Restore and release TERMINAL's launching foreground transport."
  (let ((direct nil))
    (with-lock-held ((localgroup-terminal-lock terminal))
      (setf direct (localgroup-terminal-direct-terminal terminal)
            (localgroup-terminal-direct-terminal terminal) nil)
      (unless (localgroup-terminal-controller terminal)
        (setf (terminal-interactive-p terminal) nil)))
    (when direct
      (ignore-errors (terminal-stop direct)))
    (not (null direct))))

(-> localgroup-terminal-attach
    (localgroup-terminal localgroup-attachment
     &key (:rows integer) (:columns integer) (:styled-p boolean)
     (:session-id string))
    (values boolean boolean))
(defun localgroup-terminal-attach
    (terminal attachment &key rows columns styled-p session-id)
  "Attach ATTACHMENT after queueing its handshake and report direct release."
  (let ((mode (localgroup-attachment-mode attachment))
        (old-controller nil)
        (direct nil)
        (released-direct-p nil))
    (with-lock-held ((localgroup-terminal-lock terminal))
      (when (and (eq mode ':control)
                 (or (localgroup-terminal-direct-terminal terminal)
                     (localgroup-terminal-controller terminal)))
        (error 'localgroup-error
               :message "The localgroup session already has a controlling terminal."
               :operation ':attach))
      (let* ((history
               (apply #'concatenate 'string
                      (cons "" (copy-list
                                (localgroup-terminal-history terminal)))))
             (next-rows
               (if (and (not (eq mode ':read-only)) (plusp rows))
                   rows
                   (terminal-rows terminal)))
             (next-columns
               (if (and (not (eq mode ':read-only)) (plusp columns))
                   columns
                   (terminal-columns terminal))))
        (unless
            (localgroup-attachment-send
             attachment
             (list :attached
                   :mode mode
                   :session-id session-id
                   :history history
                   :rows next-rows
                   :columns next-columns))
          (return-from localgroup-terminal-attach (values nil nil)))
        (ecase mode
          (:read-only
           (pushnew attachment
                    (localgroup-terminal-observers terminal)
                    :test #'eq))
          (:control
           (setf (localgroup-terminal-controller terminal) attachment))
          (:take-over
           (setf direct (localgroup-terminal-direct-terminal terminal)
                 old-controller (localgroup-terminal-controller terminal)
                 (localgroup-terminal-direct-terminal terminal) nil
                 (localgroup-terminal-controller terminal) attachment
                 released-direct-p (not (null direct)))))
        (when (not (eq mode ':read-only))
          (setf (terminal-rows terminal) next-rows
                (terminal-columns terminal) next-columns
                (terminal-interactive-p terminal) t
                (terminal-styled-p terminal) (not (null styled-p))))))
    (when direct
      (ignore-errors (terminal-stop direct)))
    (when (and old-controller (not (eq old-controller attachment)))
      (localgroup-attachment-send old-controller '(:revoked))
      (localgroup-attachment-close old-controller))
    (values t released-direct-p)))

(-> localgroup-terminal-detach
    (localgroup-terminal localgroup-attachment)
    boolean)
(defun localgroup-terminal-detach (terminal attachment)
  "Remove ATTACHMENT and report whether it controlled TERMINAL."
  (let ((controlled-p nil))
    (with-lock-held ((localgroup-terminal-lock terminal))
      (setf (localgroup-terminal-observers terminal)
            (delete attachment (localgroup-terminal-observers terminal)))
      (when (eq attachment (localgroup-terminal-controller terminal))
        (setf (localgroup-terminal-controller terminal) nil
              (localgroup-terminal-input-events terminal) nil
              controlled-p t)
        (unless (localgroup-terminal-direct-terminal terminal)
          (setf (terminal-interactive-p terminal) nil))))
    controlled-p))

(-> localgroup-terminal-release-control (localgroup-terminal) boolean)
(defun localgroup-terminal-release-control (terminal)
  "Release TERMINAL's direct or remote controller and report direct ownership."
  (let ((direct nil)
        (controller nil))
    (with-lock-held ((localgroup-terminal-lock terminal))
      (setf direct (localgroup-terminal-direct-terminal terminal)
            controller (localgroup-terminal-controller terminal)
            (localgroup-terminal-direct-terminal terminal) nil
            (localgroup-terminal-controller terminal) nil
            (localgroup-terminal-input-events terminal) nil
            (terminal-interactive-p terminal) nil))
    (when direct
      (ignore-errors (terminal-stop direct)))
    (when controller
      (localgroup-attachment-send controller '(:detached))
      (localgroup-attachment-close controller))
    (not (null direct))))

(-> localgroup-terminal-observer-count (localgroup-terminal) (integer 0))
(defun localgroup-terminal-observer-count (terminal)
  "Return TERMINAL's current read-only attachment count."
  (with-lock-held ((localgroup-terminal-lock terminal))
    (length (localgroup-terminal-observers terminal))))

(-> localgroup-terminal-attached-p (localgroup-terminal) boolean)
(defun localgroup-terminal-attached-p (terminal)
  "Return true when TERMINAL has direct or controlling ownership."
  (with-lock-held ((localgroup-terminal-lock terminal))
    (not
     (null
      (or (localgroup-terminal-direct-terminal terminal)
          (localgroup-terminal-controller terminal))))))

(-> localgroup-terminal-attachment-kind (localgroup-terminal) keyword)
(defun localgroup-terminal-attachment-kind (terminal)
  "Return TERMINAL's current ownership kind."
  (with-lock-held ((localgroup-terminal-lock terminal))
    (cond ((localgroup-terminal-direct-terminal terminal) ':foreground)
          ((localgroup-terminal-controller terminal) ':remote)
          (t ':detached))))
