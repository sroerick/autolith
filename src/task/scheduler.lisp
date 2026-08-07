(in-package #:autolith)

;;;; -- Task Scheduling and Publication --

(defun task-job--set-progress-state (job state)
  "Set JOB's normalized progress STATE."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (setf (task-progress-status progress) state
            (task-progress-updated-at progress) (get-internal-real-time))
      (when (eq state :running)
        (setf (task-progress-started-at progress) (get-internal-real-time)))))
  nil)

(-> task--retained-prefix (string integer) string)
(defun task--retained-prefix (text limit)
  "Return at most LIMIT leading characters from TEXT."
  (subseq text 0 (min limit (length text))))

(-> task-job--compact-result
    (list &key (:artifact-available-p boolean))
    list)
(defun task-job--compact-result (result &key artifact-available-p)
  "Return a bounded terminal summary of RESULT and its artifact availability."
  (let ((retained
          (loop for field in
                  '(:id :name :agent :agent-source :assignment :status
                    :output :error :yielded-p
                    :structured-output-present-p :structured-output :label
                    :request-count :usage :duration-ms :model
                    :conversation-file :detached :output-path
                    :agent-definition)
                append (list field (getf result field))))
        (storage (if artifact-available-p :artifact :omitted)))
    (flet ((compact-string
              (field limit &key storage-field characters-field)
             (let ((value (getf retained field)))
               (when (and (stringp value) (> (length value) limit))
                 (setf (getf retained field)
                       (task--retained-prefix value limit)
                       (getf retained storage-field) storage
                       (getf retained characters-field) (length value))))))
      (compact-string :assignment *task-retained-assignment-limit*
                      :storage-field :assignment-storage
                      :characters-field :assignment-characters)
      (compact-string :output *task-retained-output-limit*
                      :storage-field :output-storage
                      :characters-field :output-characters)
      (compact-string :error *task-retained-output-limit*
                      :storage-field :error-storage
                      :characters-field :error-characters)
      (compact-string :label *task-result-label-maximum-characters*
                      :storage-field :label-storage
                      :characters-field :label-characters))
    (when (getf retained :structured-output-present-p)
      (let* ((value (getf retained :structured-output))
             (serialized (task--write-readable-sexp value)))
        (when (> (length serialized)
                 *task-retained-structured-output-limit*)
          (setf (getf retained :structured-output) nil
                (getf retained :structured-output-storage) storage
                (getf retained :structured-output-characters)
                (length serialized)))))
    (let* ((usage (getf retained :usage))
           (serialized (and usage (task--write-readable-sexp usage))))
      (when (and serialized
                 (> (length serialized) *task-retained-usage-limit*))
        (setf (getf retained :usage) nil
              (getf retained :usage-storage) storage
              (getf retained :usage-characters) (length serialized))))
    retained))

(-> task-job--compact-progress (task-job keyword) null)
(defun task-job--compact-progress (job state)
  "Make JOB's progress terminal and release its large transient fields."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (let* ((output (task-progress-output-tail progress))
             (start
               (max 0
                    (- (length output)
                       *task-retained-progress-output-limit*))))
        (setf (task-progress-status progress) state
              (task-progress-current-tool progress) nil
              (task-progress-output-tail progress) (subseq output start)
              (task-progress-usage progress)
              (task--compact-native-value
               (task-progress-usage progress)
               *task-retained-usage-limit*)
              (task-progress-updated-at progress) (get-internal-real-time)))))
  nil)

(-> task-job--compact-item (task-job) list)
(defun task-job--compact-item (job)
  "Return the bounded assignment metadata retained for terminal JOB."
  (let ((item (task-job-item job)))
    (list :name (getf item :name)
          :agent (getf item :agent)
          :task (task--retained-prefix
                 (or (getf item :task) "")
                 *task-retained-assignment-limit*)
          :async (getf item :async))))

(-> task--compact-native-value (t integer) t)
(defun task--compact-native-value (value limit)
  "Return native VALUE or a descriptor when its readable form exceeds LIMIT."
  (let ((characters (length (task--write-readable-sexp value))))
    (if (<= characters limit)
        value
        (list :omitted :characters characters))))

(-> task--agent-definition-summary (task-agent-definition) list)
(defun task--agent-definition-summary (definition)
  "Return compact non-instruction metadata for DEFINITION."
  (let ((pathname (task-agent-definition-pathname definition))
        (output (task-agent-definition-output definition)))
    (list :name (task-agent-definition-name definition)
          :source (task-agent-definition-source definition)
          :pathname (and pathname (namestring pathname))
          :tools
          (task--compact-native-value
           (task-agent-definition-tools definition) 1000)
          :spawns
          (task--compact-native-value
           (task-agent-definition-spawns definition) 1000)
          :models
          (task--compact-native-value
           (task-agent-definition-models definition) 1000)
          :reasoning-effort
          (task-agent-definition-reasoning-effort definition)
          :output-contract-p (and output t)
          :blocking-p
          (and (task-agent-definition-blocking-p definition) t))))

(-> task-job--lifecycle-event (task-job keyword list) list)
(defun task-job--lifecycle-event (job state result)
  "Return JOB's portable terminal lifecycle event."
  (list :id (job-identifier job)
        :agent (task-job-agent-name job)
        :agent-source (task-job-agent-source job)
        :status state
        :session-file (getf result :conversation-file)
        :parent-tool-call-id (task-job-parent-call-id job)
        :index (job-index job)
        :detached (task-job-detached-p job)))

(-> task-job--terminal-record
    (task-job keyword t (option string))
    (values list (option string) keyword))
(defun task-job--terminal-record (job state result report)
  "Return JOB's terminal record for STATE, releasing what it must not retain.

Runs inside the pool's publication claim, so the result artifact is written exactly
once however many writers raced for the job. The order matters: a missing result is
rebuilt while the role is still available, the artifact is written before the result
is compacted, and the parent session and borrowed capabilities are dropped last.

The child's reported status decides the terminal state, since the pool only sees
that a body returned. An artifact that cannot be written downgrades the state to
:FAILED rather than claiming a success whose output nobody can find."
  (let* ((reported (and (listp result) (getf result :status)))
         (final-state (case reported
                        (:success :completed)
                        (:aborted :aborted)
                        (:failed :failed)
                        (otherwise state)))
         (final-result (and (listp result) (copy-list result)))
         (final-report report)
         (definition-summary
           (or (task-job-definition-summary job)
               (and (task-job-definition job)
                    (task--agent-definition-summary (task-job-definition job))))))
    (unless final-result
      (setf final-result
            (task--failed-result job
                                 (if (eq final-state :completed)
                                     :failed
                                     final-state)
                                 (or final-report "The child produced no result."))
            final-state
            (if (eq final-state :completed) :failed final-state)))
    (setf (getf final-result :status)
          (case final-state
            (:completed :success)
            (:aborted :aborted)
            (otherwise :failed))
          (getf final-result :agent-definition) definition-summary)
    (handler-case
        (setf final-result
              (append final-result
                      (list :output-path
                            (namestring
                             (task--write-result-artifact job final-result)))))
      (error (condition)
        (setf final-state :failed
              (getf final-result :status) :failed
              (getf final-result :error)
              (format nil "Could not persist task artifact: ~A" condition)
              final-report
              (or final-report
                  (bounded-string (princ-to-string condition)
                                  :limit *task-retained-output-limit*)))))
    (setf final-result
          (task-job--compact-result final-result
                                    :artifact-available-p
                                    (and (getf final-result :output-path) t))
          final-report
          (and final-report
               (bounded-string final-report
                               :limit *task-retained-output-limit*)))
    (task-job--compact-progress job final-state)
    (setf (task-job-item job) (task-job--compact-item job)
          (task-job-definition-summary job) definition-summary
          (task-job-definition job) nil
          (task-job-parent-agent job) nil
          (task-job-inherited-reference-p job) nil
          (task-job-inherited-reference-items job) nil
          (task-job-command-authorization-function job) nil
          (task-job-tool-authorization-function job) nil)
    (values final-result final-report final-state)))

(-> task-job--cancelled-ancestor-reason (task-job) (option keyword))
(defun task-job--cancelled-ancestor-reason (job)
  "Return the reason an ancestor of JOB was cancelled, or NIL when none was.

A child admitted just as its parent was cancelled misses the cascade, which only
walks the subtree that existed when it ran. Checking ancestry closes that window
before the child does any work. An ancestor already evicted from retention counts
as live: a parent that finished normally is no reason to stop a detached child."
  (let ((pool (job-pool job)))
    (dolist (identifier (job-owner-identifiers job))
      (let ((ancestor (handler-case
                          (job-pool-find-job pool identifier)
                        (job-not-found ()
                          nil))))
        (when ancestor
          (let ((reason (job-cancellation-reason ancestor)))
            (when reason
              (return reason))))))))

(-> task-job--run (task-job) list)
(defun task-job--run (job)
  "Run JOB's child agent on a pool worker and return its portable result.

The pool has already moved JOB to :RUNNING and armed its cancellation guard, so
this only mirrors that state into the progress record job tools read, refuses to
start under a cancelled ancestor, and hands over to the child."
  (task-job--set-progress-state job :running)
  (let ((reason (task-job--cancelled-ancestor-reason job)))
    (when reason
      (error 'job-aborted
             :message (format nil "Task ~A was ~(~A~) with its parent."
                              (job-identifier job) reason)
             :identifier (job-identifier job)
             :reason reason)))
  (task-run-child job))

(-> task-parent-root-conversation-identifier (agent) non-empty-string)
(defun task-parent-root-conversation-identifier (parent)
  "Return the primary conversation identifier owning PARENT's task tree."
  (if (typep parent 'task-child-agent)
      (task-job-root-conversation-identifier (task-child-agent-job parent))
      (conversation-identifier (agent-conversation parent))))

(-> task-parent-owner-identifiers (agent) list)
(defun task-parent-owner-identifiers (parent)
  "Return the task identifiers authorized to inspect PARENT's descendants."
  (if (typep parent 'task-child-agent)
      (let ((job (task-child-agent-job parent)))
        (append (job-owner-identifiers job)
                (list (job-identifier job))))
      nil))

(defun task-orchestrator--refuse-admission (orchestrator condition)
  "Re-signal pool refusal CONDITION as the typed task error task.run reports."
  (error 'task-error
         :message
         (typecase condition
           (job-pool-capacity-exceeded
            (if (eq (job-pool-capacity-exceeded-limit-kind condition) :batch-size)
                (format nil "A task batch may contain at most ~D children."
                        (task-orchestrator-maximum-batch-size orchestrator))
                (format nil "The task runtime admits at most ~D live jobs."
                        (task-orchestrator-maximum-live-jobs orchestrator))))
           (t "The task runtime is shutting down."))
         :tool-name "task.run"))

(defun task-orchestrator--check-hurry-up (orchestrator count)
  "Refuse COUNT further children once hurry-up mode has spent its allowance.

The pool caps how many children are live at once; this is the cumulative budget
for the whole interval, a session policy the pool knows nothing about."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (when (and (task-orchestrator-hurry-up-p orchestrator)
               (> (+ (task-orchestrator-hurry-up-admission-count orchestrator)
                     count)
                  *task-hurry-up-maximum-agents*))
      (error 'task-error
             :message
             (format nil
                     "Hurry-up mode permits at most ~D child agents before it is turned off."
                     *task-hurry-up-maximum-agents*)
             :tool-name "task.run")))
  nil)

(defun task-orchestrator--note-hurry-up-admission (orchestrator count)
  "Charge COUNT admitted children against the current hurry-up allowance."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (when (task-orchestrator-hurry-up-p orchestrator)
      (incf (task-orchestrator-hurry-up-admission-count orchestrator) count)))
  nil)

(defun task-orchestrator--refuse-cancelled-parent (parent-job)
  "Signal that PARENT-JOB was cancelled before it could admit a child."
  (error 'job-aborted
         :message (format nil "Task ~A was cancelled before child admission."
                          (job-identifier parent-job))
         :identifier (job-identifier parent-job)
         :reason (or (job-cancellation-reason parent-job) :shutdown)))

(defun task-orchestrator-start-jobs
    (orchestrator parent-agent entries
     &key parent-call-id
       command-authorization-function
       tool-authorization-function)
  "Atomically admit ENTRIES and return jobs plus nested synchronous inline jobs.

A cancelled parent is refused here so the asking agent gets a prompt error instead
of a child that aborts a moment later. This refusal races the parent's own
cancellation and is only a courtesy; TASK-JOB--RUN's ancestry check is the
guarantee."
  (when (typep parent-agent 'task-child-agent)
    (let ((parent-job (task-child-agent-job parent-agent)))
      (when (or (job-cancellation-requested-p parent-job)
                (job-terminal-p parent-job))
        (task-orchestrator--refuse-cancelled-parent parent-job))))
  (let ((count (length entries))
        (root-conversation-identifier
          (task-parent-root-conversation-identifier parent-agent))
        (owner-identifiers (task-parent-owner-identifiers parent-agent))
        (reference-enabled-entries (make-hash-table :test #'eq))
        (reference-byte-limit nil)
        (inherited-reference-items nil))
    (task-orchestrator--check-hurry-up orchestrator count)
    (dolist (entry entries)
      (let* ((definition (getf entry :definition))
             (child-configuration
               (task-configuration-for-definition
                (agent-configuration parent-agent) definition)))
        (when (task-child-reference-history-p parent-agent child-configuration)
          (let ((limit
                  (task-child-reference-history-byte-limit
                   child-configuration)))
            (when limit
              (setf (gethash entry reference-enabled-entries) t
                    reference-byte-limit
                    (if reference-byte-limit
                        (min reference-byte-limit limit)
                        limit)))))))
    (when reference-byte-limit
      (setf inherited-reference-items
            (conversation-inherited-reference-snapshot
             (agent-conversation parent-agent) reference-byte-limit)))
    (let ((jobs
            (handler-case
                (job-pool-submit-batch
                 (task-orchestrator-pool orchestrator)
                 (mapcar
                  (lambda (entry)
                    (let* ((item (getf entry :item))
                           (inherited-p
                             (not (null (gethash entry
                                                 reference-enabled-entries))))
                           (nested-synchronous-p
                             (and (typep parent-agent 'task-child-agent)
                                  (not (getf entry :detached)))))
                      (list :function #'task-job--run
                            :terminal-result-function #'task-job--terminal-record
                            :name (task-orchestrator--child-name
                                   orchestrator (getf item :name))
                            ;; A child a child agent waits for runs on the
                            ;; waiting worker rather than occupying a second one,
                            ;; so a deep synchronous chain cannot starve the pool.
                            :inline-only-p nested-synchronous-p
                            :owner-identifiers owner-identifiers
                            :root-identifier root-conversation-identifier
                            :initargs
                            (list :orchestrator orchestrator
                                  :execution-identifier (make-identifier)
                                  :definition (getf entry :definition)
                                  :item item
                                  :parent-agent parent-agent
                                  :inherited-reference-p inherited-p
                                  :inherited-reference-items
                                  (and inherited-p inherited-reference-items)
                                  :parent-call-id parent-call-id
                                  :detached-p (getf entry :detached)
                                  :command-authorization-function
                                  command-authorization-function
                                  :tool-authorization-function
                                  tool-authorization-function))))
                  entries))
              (job-pool-error (condition)
                (task-orchestrator--refuse-admission orchestrator condition)))))
      (task-orchestrator--note-hurry-up-admission orchestrator count)
      (values jobs
              (if (typep parent-agent 'task-child-agent)
                  (remove-if #'task-job-detached-p jobs)
                  nil)))))

(defun task-orchestrator-start-job
    (orchestrator
     &key parent-agent definition item detached-p parent-call-id
       command-authorization-function tool-authorization-function)
  "Admit one JOB through the atomic scheduler admission path."
  (multiple-value-bind (jobs inline)
      (task-orchestrator-start-jobs
       orchestrator
       parent-agent
       (list (list :definition definition
                   :item item
                   :detached detached-p))
       :parent-call-id parent-call-id
       :command-authorization-function command-authorization-function
       :tool-authorization-function tool-authorization-function)
    (dolist (job inline)
      (job-run-inline job))
    (first jobs)))
