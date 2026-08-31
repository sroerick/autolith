(in-package #:autolith)

;;;; -- Herdr Runtime Integration --

;;; Autolith reports its agent lifecycle to a surrounding Herdr pane so the
;;; sidebar can show working, blocked, and idle. Reports only happen when the
;;; process runs inside Herdr (HERDR_ENV=1 with HERDR_PANE_ID and
;;; HERDR_BIN_PATH set) and never change behavior otherwise. The Herdr CLI is
;;; invoked on one serialized background thread so state transitions never
;;; block the model loop or the terminal reader.

(defparameter *herdr-environment-variable* "HERDR_ENV"
  "The environment variable Herdr sets inside every pane it owns.")

(defparameter *herdr-pane-variable* "HERDR_PANE_ID"
  "The environment variable naming the Herdr pane that owns this process.")

(defparameter *herdr-binary-variable* "HERDR_BIN_PATH"
  "The environment variable naming the Herdr executable for pane reports.")

(defparameter *herdr-source* "custom:autolith"
  "The stable Herdr lifecycle source identifying this integration.")

(defparameter *herdr-agent-label* "autolith"
  "The Herdr agent label reported for every Autolith pane.")

(defparameter *herdr-quiescence-timeout* 5
  "The maximum seconds one shutdown waits for queued Herdr reports.")

(defvar *herdr-lock* (make-lock "herdr-reporter")
  "The lock serializing the Herdr report queue and lifecycle bookkeeping.")

(defvar *herdr-condition* (make-condition-variable :name "herdr-reporter")
  "The condition variable signaling queued and drained Herdr reports.")

(defvar *herdr-queue* (make-deque)
  "The ordered queue of pending Herdr CLI argument lists and stop markers.")

(defvar *herdr-sequence* 0
  "The strictly increasing report sequence across one Autolith process.")

(defvar *herdr-thread* nil
  "The live Herdr reporter thread, or NIL after a stop report completed.")

(defvar *herdr-last-state* nil
  "The most recent semantic lifecycle state reported to Herdr.")

(defvar *herdr-state-before-block* nil
  "The lifecycle state Herdr reports resume to after one blocked prompt.")

(defvar *herdr-pending* 0
  "The number of queued or in-flight Herdr reports not yet run.")

(defvar *herdr-released-p* nil
  "True after the Herdr lifecycle authority was released at shutdown.")


(-> herdr-available-p () boolean)
(defun herdr-available-p ()
  "Return true when this process runs inside one Herdr pane."
  (and (string= (or (uiop:getenv *herdr-environment-variable*) "") "1")
       (non-empty-string-p (uiop:getenv *herdr-pane-variable*))
       (non-empty-string-p (uiop:getenv *herdr-binary-variable*))))


(-> herdr--report-arguments
    (string string keyword &key (:seq integer) (:summary (option string)))
    list)
(defun herdr--report-arguments (binary pane-id state &key seq summary)
  "Return the Herdr CLI arguments reporting STATE for PANE-ID via BINARY."
  (append
   (list binary
         "pane"
         "report-agent"
         pane-id
         "--source"  *herdr-source*
         "--agent"   *herdr-agent-label*
         "--state"   (string-downcase (symbol-name state))
         "--seq"     (write-to-string seq))
   (when summary
     (list "--message" summary))))

(-> herdr--release-arguments (string string &key (:seq integer)) list)
(defun herdr--release-arguments (binary pane-id &key seq)
  "Return the Herdr CLI arguments releasing the lifecycle authority."
  (list binary
        "pane"
        "release-agent"
        pane-id
        "--source" *herdr-source*
        "--agent"  *herdr-agent-label*
        "--seq"    (write-to-string seq)))

(-> herdr--run-cli (list) null)
(defun herdr--run-cli (arguments)
  "Run the Herdr CLI with ARGUMENTS, tolerating every failure."
  (handler-case
      (uiop:run-program arguments
                        :output              nil
                        :error-output        nil
                        :ignore-error-status t)
    (error ()
      nil))
  nil)


(-> herdr--reporter-loop () null)
(defun herdr--reporter-loop ()
  "Run queued Herdr CLI reports until the stop marker arrives."
  (loop
    (let ((report
            (with-lock-held (*herdr-lock*)
              (loop
                (unless (deque-empty-p *herdr-queue*)
                  (return (deque-pop-front *herdr-queue*)))
                (condition-wait *herdr-condition* *herdr-lock*)))))
      (when (eq (first report) ':stop)
        (with-lock-held (*herdr-lock*)
          (condition-notify *herdr-condition*))
        (return))
      (herdr--run-cli (second report))
      (with-lock-held (*herdr-lock*)
        (incf *herdr-pending* -1)
        (when (and (deque-empty-p *herdr-queue*)
                   (zerop *herdr-pending*))
          (condition-notify *herdr-condition*)))))
  nil)

(-> herdr--ensure-thread () null)
(defun herdr--ensure-thread ()
  "Start the Herdr reporter thread when none is alive. Lock must be held."
  (unless (and *herdr-thread* (thread-alive-p *herdr-thread*))
    (setf *herdr-thread*
          (make-thread #'herdr--reporter-loop
                       :name "autolith-herdr-reporter")))
  nil)

(-> herdr--enqueue (list) null)
(defun herdr--enqueue (report)
  "Append REPORT to the queue. Lock must be held."
  (when (eq (first report) ':report)
    (incf *herdr-pending*))
  (deque-push-back *herdr-queue* report)
  (herdr--ensure-thread)
  (condition-notify *herdr-condition*)
  nil)

(-> herdr--submit-state (keyword (option string)) null)
(defun herdr--submit-state (state summary)
  "Track and queue one lifecycle STATE report. Lock must be held."
  (let ((binary  (uiop:getenv *herdr-binary-variable*))
        (pane-id (uiop:getenv *herdr-pane-variable*)))
    (when (eq state ':blocked)
      (unless *herdr-state-before-block*
        (setf *herdr-state-before-block* (or *herdr-last-state* ':working))))
    (setf *herdr-last-state* state)
    (herdr--enqueue
     (list ':report
           (herdr--report-arguments binary pane-id state
                                    :seq     (1+ *herdr-sequence*)
                                    :summary summary)))
    (incf *herdr-sequence*))
  nil)

(-> herdr-report-idle () null)
(defun herdr-report-idle ()
  "Report that the agent is ready for interactive input."
  (when (herdr-available-p)
    (with-lock-held (*herdr-lock*)
      (unless *herdr-released-p*
        (herdr--submit-state ':idle nil))))
  nil)

(-> herdr-report-working () null)
(defun herdr-report-working ()
  "Report that the agent is running a model turn or background work."
  (when (herdr-available-p)
    (with-lock-held (*herdr-lock*)
      (unless *herdr-released-p*
        (herdr--submit-state ':working nil))))
  nil)


(-> herdr-report-blocked (string) null)
(defun herdr-report-blocked (summary)
  "Report that the agent waits on a user decision described by SUMMARY."
  (when (herdr-available-p)
    (with-lock-held (*herdr-lock*)
      (unless *herdr-released-p*
        (herdr--submit-state ':blocked summary))))
  nil)

(-> herdr-report-resumed () null)
(defun herdr-report-resumed ()
  "Restore the tracked pre-block state after one user decision."
  (when (herdr-available-p)
    (with-lock-held (*herdr-lock*)
      (unless *herdr-released-p*
        (herdr--submit-state (or *herdr-state-before-block* ':working) nil)
        (setf *herdr-state-before-block* nil))))
  nil)


(-> herdr--await-quiescence (&key (:seconds integer)) boolean)
(defun herdr--await-quiescence (&key (seconds *herdr-quiescence-timeout*))
  "Wait up to SECONDS for every queued Herdr report to run."
  (block nil
    (with-lock-held (*herdr-lock*)
      (loop
        (when (and (deque-empty-p *herdr-queue*)
                   (zerop *herdr-pending*))
          (return t))
        (unless (condition-wait *herdr-condition* *herdr-lock*
                                :timeout seconds)
          (return nil))))))

(-> herdr-release () null)
(defun herdr-release ()
  "Release the Herdr lifecycle authority exactly once at shutdown."
  (when (herdr-available-p)
    (with-lock-held (*herdr-lock*)
      (unless *herdr-released-p*
        (setf *herdr-released-p* t)
        (incf *herdr-sequence*)
        (herdr--enqueue
         (list ':report
               (herdr--release-arguments
                (uiop:getenv *herdr-binary-variable*)
                (uiop:getenv *herdr-pane-variable*)
                :seq *herdr-sequence*)))
        (herdr--enqueue (list ':stop)))))
  (herdr--await-quiescence)
  nil)
