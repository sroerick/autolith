(in-package #:autolith)

;;;; -- Task Runtime --

(-> task--environment-integer
    (string integer &key (:minimum (option integer)) (:maximum (option integer)))
    integer)
(defun task--environment-integer (name fallback &key minimum maximum)
  "Return bounded integer environment NAME or FALLBACK."
  (let ((value (uiop/os:getenv name)))
    (if (non-empty-string-p value)
        (handler-case
            (let ((parsed (parse-integer value :junk-allowed nil)))
              (if (and (integerp parsed)
                       (or (null minimum) (>= parsed minimum)))
                  (if maximum (min parsed maximum) parsed)
                  fallback))
          (error nil fallback))
        fallback)))

(-> task-orchestrator--apply-limits-locked
    (task-orchestrator &key (:refresh-runtime-p boolean))
    null)
(defun task-orchestrator--apply-limits-locked
    (orchestrator &key refresh-runtime-p)
  "Apply environment or hurry-up admission bounds while ORCHESTRATOR is locked.

Hurry-up mode replaces every admission bound with one small number, because an
urgent session should not be spending its remaining budget on child agents. The
orchestrator-to-pool lock order serializes a complete policy update with task
submission and cl-jobpond workers."
  (let ((pool       (task-orchestrator-pool orchestrator))
        (hurry-up-p (task-orchestrator-hurry-up-p orchestrator)))
    (with-lock-held ((cl-jobpond::job-pool--lock pool))
      (setf (job-pool-maximum-concurrency pool)
            (if hurry-up-p
                *task-hurry-up-maximum-agents*
                (task--environment-integer "AUTOLITH_TASK_MAX_CONCURRENCY"
                                           *task-default-maximum-concurrency*
                                           :minimum 1
                                           :maximum *task-maximum-concurrency*))
            (job-pool-maximum-batch-size pool)
            (if hurry-up-p
                *task-hurry-up-maximum-agents*
                *task-maximum-batch-size*)
            (job-pool-maximum-live-jobs pool)
            (if hurry-up-p
                *task-hurry-up-maximum-agents*
                *task-maximum-live-jobs*))
      (when refresh-runtime-p
        (setf (job-pool-maximum-runtime-milliseconds pool)
              (task--environment-integer
               "AUTOLITH_TASK_MAX_RUNTIME_MS"
               *task-default-maximum-runtime-milliseconds*
               :minimum 0)))))
  nil)

(-> task-orchestrator-set-hurry-up (task-orchestrator boolean) task-orchestrator)
(defun task-orchestrator-set-hurry-up (orchestrator enabled-p)
  "Apply ENABLED-P and its hard admission limits to ORCHESTRATOR.

This only moves the bounds. Admission starts whatever workers the current bounds
call for, so changing a limit never costs a session threads it may never use."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (unless (eq (task-orchestrator-hurry-up-p orchestrator) enabled-p)
      (setf (task-orchestrator-hurry-up-p orchestrator) enabled-p
            (task-orchestrator-hurry-up-admission-count orchestrator)
            (if enabled-p
                (job-pool-live-count (task-orchestrator-pool orchestrator))
                0)))
    (task-orchestrator--apply-limits-locked orchestrator))
  orchestrator)


;;;; -- Pool Bounds Seen As Orchestrator State --

(-> task-orchestrator-maximum-concurrency (task-orchestrator) (integer 1))
(defun task-orchestrator-maximum-concurrency (orchestrator)
  "Return the child jobs ORCHESTRATOR may run at the same time."
  (job-pool-maximum-concurrency (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-maximum-batch-size (task-orchestrator) (integer 1))
(defun task-orchestrator-maximum-batch-size (orchestrator)
  "Return the children ORCHESTRATOR accepts in one atomic batch."
  (job-pool-maximum-batch-size (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-maximum-live-jobs (task-orchestrator) (integer 1))
(defun task-orchestrator-maximum-live-jobs (orchestrator)
  "Return the combined queued and running children ORCHESTRATOR permits."
  (job-pool-maximum-live-jobs (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-maximum-runtime-milliseconds
    (task-orchestrator)
    (integer 0))
(defun task-orchestrator-maximum-runtime-milliseconds (orchestrator)
  "Return ORCHESTRATOR's wall-clock cap for one child, or zero when disabled."
  (job-pool-maximum-runtime-milliseconds (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-live-count (task-orchestrator) (integer 0))
(defun task-orchestrator-live-count (orchestrator)
  "Return ORCHESTRATOR's admitted queued, running, and finalizing children."
  (job-pool-live-count (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-active-count (task-orchestrator) (integer 0))
(defun task-orchestrator-active-count (orchestrator)
  "Return the children ORCHESTRATOR is currently running on workers."
  (job-pool-active-count (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-lifecycle-state (task-orchestrator) keyword)
(defun task-orchestrator-lifecycle-state (orchestrator)
  "Return ORCHESTRATOR's :OPEN, :CLOSING, or :CLOSED lifecycle state."
  (job-pool-lifecycle-state (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-shutdown-p (task-orchestrator) boolean)
(defun task-orchestrator-shutdown-p (orchestrator)
  "Return true when ORCHESTRATOR has stopped accepting new children."
  (not (eq (task-orchestrator-lifecycle-state orchestrator) :open)))


;;;; -- Construction and Refresh --

(-> task-orchestrator-create () task-orchestrator)
(defun task-orchestrator-create ()
  "Create an orchestrator and its worker pool from the task environment."
  (let* ((pool
           (make-job-pool
            :name "Autolith task"
            :job-class 'task-job
            :maximum-concurrency
            (task--environment-integer "AUTOLITH_TASK_MAX_CONCURRENCY"
                                       *task-default-maximum-concurrency*
                                       :minimum 1
                                       :maximum *task-maximum-concurrency*)
            :maximum-batch-size *task-maximum-batch-size*
            :maximum-live-jobs *task-maximum-live-jobs*
            :maximum-runtime-milliseconds
            (task--environment-integer
             "AUTOLITH_TASK_MAX_RUNTIME_MS"
             *task-default-maximum-runtime-milliseconds*
             :minimum 0)
            :terminal-retention-limit *task-terminal-retention-limit*
            ;; Most sessions never spawn a child agent, and a session that has
            ;; not spawned one must stay single threaded so it can still save its
            ;; own image. Admission starts the workers when they are first needed.
            :start-threads-p nil))
         (orchestrator
           (make-instance 'task-orchestrator
                          :pool pool
                          :maximum-depth
                          (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                                     *task-default-maximum-depth*
                                                     :minimum 1))))
    (job-pool-add-listener pool #'task--pool-event-listener)
    orchestrator))

(-> task-orchestrator-refresh (task-orchestrator) task-orchestrator)
(defun task-orchestrator-refresh (orchestrator)
  "Apply current limits to ORCHESTRATOR and ensure its reusable workers."
  (let ((pool (task-orchestrator-pool orchestrator)))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (task-orchestrator--apply-limits-locked
       orchestrator
       :refresh-runtime-p t)
      (setf (task-orchestrator-maximum-depth orchestrator)
            (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                       *task-default-maximum-depth* :minimum 1)))
    ;; A detached pool dropped its listeners along with its job table, so this
    ;; registration has to be repeated rather than assumed to have survived.
    (job-pool-add-listener pool #'task--pool-event-listener)
    ;; The pool reaps threads that died before deciding whether it is still
    ;; closing, so a close that timed out and then finished reopens here.
    (handler-case
        (job-pool-refresh pool)
      (job-pool-closed ()
        (error 'task-error
               :message "The task runtime is still shutting down."
               :tool-name "task.run"))))
  orchestrator)


;;;; -- Listeners --

(defun task-orchestrator-add-listener (orchestrator listener)
  "Register LISTENER for portable task events and return it."
  (check-type listener function)
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (pushnew listener (task-orchestrator-listeners orchestrator) :test #'eq))
  listener)

(defun task-orchestrator-remove-listener (orchestrator listener)
  "Remove LISTENER from ORCHESTRATOR."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-listeners orchestrator)
          (remove listener (task-orchestrator-listeners orchestrator) :test
                  #'eq)))
  nil)

(defun task-orchestrator-emit (orchestrator channel payload)
  "Deliver portable CHANNEL and PAYLOAD to a snapshot of listeners."
  (let ((listeners
         (with-lock-held ((task-orchestrator-lock orchestrator))
           (copy-list (task-orchestrator-listeners orchestrator)))))
    (dolist (listener listeners)
      (handler-case
          (funcall listener channel payload)
        (serious-condition ()
          nil))))
  nil)

(defun task--identifier-fragment (value)
  "Return VALUE normalized for child identifiers and artifact names."
  (let* ((unbounded (string-downcase (task--trim (or value ""))))
         (text (subseq unbounded
                       0
                       (min (length unbounded)
                            *task-identifier-maximum-characters*)))
         (mapped
          (map 'string
               (lambda (character)
                 (if (or (alphanumericp character)
                         (member character '(#\HYPHEN-MINUS #\LOW_LINE) :test
                                 #'char=))
                     character
                     #\HYPHEN-MINUS))
               text))
         (trimmed (string-trim '(#\HYPHEN-MINUS) mapped)))
    (and (non-empty-string-p trimmed) trimmed)))

(-> task-orchestrator--child-name (task-orchestrator (option string)) string)
(defun task-orchestrator--child-name (orchestrator requested-name)
  "Return the readable name a new child of ORCHESTRATOR is admitted under.

A supplied name passes through: the pool bounds it and appends the admission index
to make it unique. A child given no name still gets a readable one, since a task
identifier is what an agent uses to refer to its own children."
  (if (non-empty-string-p requested-name)
      requested-name
      (let ((index (with-lock-held ((task-orchestrator-lock orchestrator))
                     (incf (task-orchestrator-next-name-index orchestrator))))
            (adjectives
              #("amber" "brisk" "calm" "clear" "keen" "quiet" "rapid" "steady"
                "vivid" "wise"))
            (nouns
              #("badger" "falcon" "heron" "lynx" "otter" "raven" "sparrow" "tern"
                "wolf" "wren")))
        (format nil "~A-~A"
                (aref adjectives (mod (1- index) (length adjectives)))
                (aref nouns
                      (mod (floor (1- index) (length adjectives))
                           (length nouns)))))))


;;;; -- Pool Events Seen As Task Events --

(defun task--pool-event-listener (channel payload)
  "Re-emit one pool CHANNEL event in Autolith's own task vocabulary.

The pool reports only that a job started or became terminal; Autolith's observers
want the child role, the parent tool call, and the conversation file. Only the job
can answer that, so the event carries it. A job of another class is ignored."
  (let ((job (getf payload :job)))
    (when (and (typep job 'task-job) (eq channel :job-lifecycle))
      (let ((status (getf payload :status)))
        (task-orchestrator-emit
         (task-job-orchestrator job)
         :task-subagent-lifecycle
         (task-job--lifecycle-event job
                                    status
                                    (if (eq status :started)
                                        nil
                                        (job-result job)))))))
  nil)


;;;; -- Shutdown --

(-> task-orchestrator-close (task-orchestrator) boolean)
(defun task-orchestrator-close (orchestrator)
  "Cancel all children, stop the pool's threads, and report complete shutdown."
  (let ((cl-jobpond:*shutdown-timeout-seconds* *task-shutdown-timeout-seconds*))
    (job-pool-close (task-orchestrator-pool orchestrator))))

(-> task-orchestrator-detach (task-orchestrator) null)
(defun task-orchestrator-detach (orchestrator)
  "Remove closed runtime state before an image save or registry replacement."
  (handler-case
      (job-pool-detach (task-orchestrator-pool orchestrator))
    (job-pool-detach-refused (condition)
      (error 'task-error
             :message
             (if (eq (job-pool-detach-refused-reason condition) :not-closed)
                 "Task runtime must close before it can detach."
                 "Task runtime cannot detach while its threads are alive.")
             :tool-name "task.run")))
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-listeners orchestrator) nil))
  nil)

(defmethod tool-runtime-identity ((tool task-orchestrator-tool))
  "Return the scheduler shared by task and job tools."
  (task-orchestrator-tool-orchestrator tool))

(defmethod tool-runtime-close ((tool task-orchestrator-tool))
  "Stop TOOL's shared jobs and reusable scheduler threads."
  (unless (task-orchestrator-close
           (task-orchestrator-tool-orchestrator tool))
    (error 'task-error
           :message "Task workers did not stop before the shutdown deadline."
           :tool-name (tool-canonical-name tool)))
  nil)

(defmethod tool-runtime-close-priority ((tool task-orchestrator-tool))
  "Stop child task workers before runtimes their shared registries may use."
  100)

(defmethod tool-runtime-resume
    ((tool task-orchestrator-tool) (registry tool-registry))
  "Restart TOOL's scheduler after a non-stopping checkpoint fork."
  (declare (ignore registry))
  (task-orchestrator-refresh (task-orchestrator-tool-orchestrator tool))
  nil)

(defmethod tool-runtime-detach ((tool task-orchestrator-tool))
  "Remove TOOL's closed shared scheduler graph before image saving."
  (task-orchestrator-detach (task-orchestrator-tool-orchestrator tool)))

(defun task--milliseconds-between (start end)
  "Return elapsed milliseconds between internal real times START and END."
  (round (* 1000 (- end start)) internal-time-units-per-second))

(defun task-progress-append-output (progress text)
  "Append streamed TEXT while retaining only a bounded tail."
  (with-lock-held ((task-progress-lock progress))
    (let* ((combined
            (concatenate 'string (task-progress-output-tail progress) text))
           (start (max 0 (- (length combined) *task-progress-output-limit*))))
      (setf (task-progress-output-tail progress) (subseq combined start)
            (task-progress-updated-at progress) (get-internal-real-time))))
  nil)

(defun task-progress-note-status (job status details)
  "Update JOB's normalized progress from one child observer STATUS event."
  (let ((progress (task-job-progress job))
        (event nil))
    (with-lock-held ((task-progress-lock progress))
      (case status
        (:provider-request-started
         (setf (task-progress-request-count progress)
               (or (getf details :request-number)
                   (1+ (task-progress-request-count progress)))))
        (:provider-request-completed
         (setf (task-progress-usage progress) (getf details :usage)))
        (:tool-call-started
         (setf (task-progress-current-tool progress) (getf details :tool)))
        (:tool-call-completed
         (let ((tool (getf details :tool)))
           (when tool
             (push tool (task-progress-recent-tools progress))
             (setf (task-progress-recent-tools progress)
                   (subseq (task-progress-recent-tools progress) 0
                           (min 8
                                (length
                                 (task-progress-recent-tools progress)))))))
         (setf (task-progress-current-tool progress) nil)))
      (setf (task-progress-updated-at progress) (get-internal-real-time)
            event
            (list :id (job-identifier job)
                  :status (task-progress-status progress)
                  :current-tool (task-progress-current-tool progress)
                  :request-count (task-progress-request-count progress))))
    (task-orchestrator-emit (task-job-orchestrator job) :task-subagent-progress
                            event)
    ;; Every observed child event is also a cancellation point, so a child whose
    ;; controller gave up stops at its next provider or tool boundary even when
    ;; an interrupt could not be delivered to it.
    (job-check-cancellation job))
  nil)

(-> task--terminal-state-p (keyword) boolean)
(defun task--terminal-state-p (state)
  "Return true when STATE is a published terminal task state.

Snapshots carry a state rather than a job, so this is still needed alongside the
pool's own JOB-TERMINAL-P."
  (not (null (member state '(:completed :failed :aborted) :test #'eq))))

(-> task-job-display-name (task-job) non-empty-string)
(defun task-job-display-name (job)
  "Return the name JOB is presented under in results and transcripts.

A child admitted with a name keeps it; one named for it falls back to its
identifier, which is generated to be readable for this reason. A caller-supplied
name is bounded here, because only the identifier derived from it was."
  (let ((name (job-name job)))
    (if name
        (subseq name 0 (min (length name) *task-identifier-maximum-characters*))
        (job-identifier job))))

(-> task-job-identity (task-job) list)
(defun task-job-identity (job)
  "Return JOB's stable identity plist.

The pool owns the identifier and index, so this assembles what a child session is
handed rather than storing a second copy."
  (list :id (job-identifier job)
        :display-name (task-job-display-name job)
        :index (job-index job)))

(-> task-job-root-conversation-identifier (task-job) non-empty-string)
(defun task-job-root-conversation-identifier (job)
  "Return the primary conversation identifier owning JOB's task tree.

A task tree is a pool job tree, named by the conversation that started it."
  (job-root-identifier job))

(-> task-job-agent-name (task-job) non-empty-string)
(defun task-job-agent-name (job)
  "Return JOB's live or retained child role name."
  (let ((definition (task-job-definition job)))
    (if definition
        (task-agent-definition-name definition)
        (getf (task-job-definition-summary job) :name))))

(-> task-job-agent-source (task-job) keyword)
(defun task-job-agent-source (job)
  "Return JOB's live or retained child role source."
  (let ((definition (task-job-definition job)))
    (if definition
        (task-agent-definition-source definition)
        (getf (task-job-definition-summary job) :source))))

(-> task-progress--snapshot
    (task-job &key (:parent t) (:result t) (:ended-at t))
    list)
(defun task-progress--snapshot (job &key parent result ended-at)
  "Return JOB progress using lifecycle values captured under the job lock."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (list :id (job-identifier job)
            :agent (task-job-agent-name job)
            :status (task-progress-status progress)
            :current-tool (task-progress-current-tool progress)
            :recent-tools
            (reverse (copy-list (task-progress-recent-tools progress)))
            :recent-output (task-progress-output-tail progress)
            :request-count (task-progress-request-count progress)
            :usage (copy-tree (task-progress-usage progress))
            :duration-ms
            (and (task-progress-started-at progress)
                 (task--milliseconds-between
                  (task-progress-started-at progress)
                  (or ended-at (get-internal-real-time))))
            :model
            (or (getf result :model)
                (and parent
                     (configuration-model
                      (task-configuration-for-definition
                       (agent-configuration parent)
                       (task-job-definition job)))))))))

(-> task-progress-snapshot (task-job) list)
(defun task-progress-snapshot (job)
  "Return a coherent portable snapshot of JOB's current progress."
  (let ((snapshot (job-snapshot job)))
    (task-progress--snapshot job
                             :parent (task-job-parent-agent job)
                             :result (getf snapshot :result)
                             :ended-at (getf snapshot :ended-at))))

(-> task-job-snapshot (task-job) list)
(defun task-job-snapshot (job)
  "Return JOB's coherent portable lifecycle, progress, and result snapshot.

The lifecycle fields are read once through the pool snapshot so they cannot mix
values from either side of a terminal transition."
  (let* ((snapshot (job-snapshot job))
         (result (copy-tree (getf snapshot :result))))
    (list :job-id (job-identifier job)
          :execution-id (task-job-execution-identifier job)
          :type :task
          :state (getf snapshot :state)
          :detached (task-job-detached-p job)
          :agent (task-job-agent-name job)
          :assignment
          (bounded-string (getf (task-job-item job) :task)
                          :limit *task-retained-assignment-limit*)
          :progress
          (task-progress--snapshot job
                                   :parent (task-job-parent-agent job)
                                   :result result
                                   :ended-at (getf snapshot :ended-at))
          :result result
          :cancellation-reason (getf snapshot :cancellation-reason)
          :condition-report (getf snapshot :condition-report))))

(defun task-orchestrator-find-job (orchestrator identifier)
  "Return IDENTIFIER's job or signal a typed task error."
  (handler-case
      (job-pool-find-job (task-orchestrator-pool orchestrator) identifier)
    (job-not-found ()
      (error 'task-error :message
             (format nil "No task job named ~A exists." identifier)
             :tool-name "job.get" :task-id identifier))))

(defun task-orchestrator-list-jobs (orchestrator)
  "Return all jobs sorted by child index."
  (job-pool-list-jobs (task-orchestrator-pool orchestrator)))

(-> task-job-live-activity (task-job) (option list))
(defun task-job-live-activity (job)
  "Return JOB's lightweight queued or running presentation snapshot."
  (let ((state (job-state job)))
    (when (member state '(:queued :running) :test #'eq)
      (let ((progress (task-job-progress job)))
        (with-lock-held ((task-progress-lock progress))
          (list :id (job-identifier job)
                :index (job-index job)
                :agent (task-job-agent-name job)
                :state state
                :current-tool (task-progress-current-tool progress)
                :assignment
                (bounded-string
                 (getf (task-job-item job) :task)
                 :limit *task-retained-assignment-limit*)
                :detached (task-job-detached-p job)))))))

(-> task-orchestrator-live-activities (task-orchestrator) list)
(defun task-orchestrator-live-activities (orchestrator)
  "Return stable lightweight snapshots for every queued or running child."
  (loop for job in (task-orchestrator-list-jobs orchestrator)
        for activity = (task-job-live-activity job)
        when activity
          collect activity))

(-> task-job-visible-to-agent-p (task-job agent) boolean)
(defun task-job-visible-to-agent-p (job viewer)
  "Return true when VIEWER owns JOB through conversation or task ancestry."
  (not
   (null
    (if (typep viewer 'task-child-agent)
        (member (job-identifier (task-child-agent-job viewer))
                (job-owner-identifiers job)
                :test #'string=)
        (string=
         (task-job-root-conversation-identifier job)
         (conversation-identifier (agent-conversation viewer)))))))

(-> task-orchestrator-list-visible-jobs
    (task-orchestrator agent)
    list)
(defun task-orchestrator-list-visible-jobs (orchestrator viewer)
  "Return jobs VIEWER may inspect, ordered by child index."
  (remove-if-not
   (lambda (job) (task-job-visible-to-agent-p job viewer))
   (task-orchestrator-list-jobs orchestrator)))

(-> task-orchestrator-find-visible-job
    (task-orchestrator string agent string)
    task-job)
(defun task-orchestrator-find-visible-job
    (orchestrator identifier viewer tool-name)
  "Return VIEWER's visible IDENTIFIER or signal a non-disclosing task error."
  (let ((job (handler-case
                 (job-pool-find-job (task-orchestrator-pool orchestrator)
                                    identifier)
               (job-not-found ()
                 nil))))
    (if (and job (task-job-visible-to-agent-p job viewer))
        job
        (error 'task-error
               :message (format nil "No visible task job named ~A exists."
                                identifier)
               :tool-name tool-name
               :task-id identifier))))

(-> task-job-cancel (task-job keyword) (values boolean list))
(defun task-job-cancel (job reason)
  "Cancel JOB and every retained live descendant, returning accepted identities.

One pass over the subtree is enough. JOB is cancelled before its descendants are
walked, and admission refuses a child whose parent already carries a cancellation
reason, so a job cancelled here cannot add to the subtree behind the walk."
  (multiple-value-bind (accepted-p cascaded)
      (job-cancel job :reason reason :cascade-p t)
    (values accepted-p
            (sort (mapcar #'job-identifier cascaded) #'string<))))

(-> task-job-await
    (task-job (option (real 0)))
    (values list boolean))
(defun task-job-await (job timeout-seconds)
  "Wait up to TIMEOUT-SECONDS and return a snapshot plus terminal flag."
  (multiple-value-bind (pool-snapshot terminal-p)
      (job-await job :timeout-seconds timeout-seconds)
    (declare (ignore pool-snapshot))
    (values (task-job-snapshot job) terminal-p)))
