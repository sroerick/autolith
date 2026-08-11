(in-package #:autolith)

;;;; -- Agenda Resource Conditions --

(define-condition agenda-resource-identifier-unsupported (autolith-error)
  ((identifier
    :initarg :identifier
    :reader agenda-resource-identifier-unsupported-identifier
    :type non-empty-string
    :documentation "The agenda resource identifier that is not yet supported."))
  (:default-initargs :message "The agenda resource identifier is not supported.")
  (:documentation "An agenda: URI names something other than the current workspace agenda.")
  (:report (lambda (condition stream)
             (format stream
                     "Agenda resource identifier ~S is unsupported; use agenda:current."
                     (agenda-resource-identifier-unsupported-identifier
                      condition)))))


;;;; -- Agenda Resources --

(defparameter *agenda-resource-maximum-observations* 16
  "The transient agenda observations retained by one conversation.")

(defvar *agenda-resource-digest-key* (random-data 16)
  "The process-local key identifying exact current-workspace agenda snapshots.")

(defclass agenda-resource (resource)
  ((directory
    :initarg :directory
    :reader agenda-resource-directory
    :type non-empty-string
    :documentation "The canonical current workspace directory resolved under authority."))
  (:documentation "The canonical current workspace agenda exposed as agenda:current."))

(defclass agenda-resolver (resource-resolver)
  ()
  (:documentation "Resolve only agenda:current for the caller's current workspace."))

(defclass agenda-observation (resource-observation)
  ((directory
    :initarg :directory
    :reader agenda-observation-directory
    :type non-empty-string
    :documentation "The canonical workspace directory represented by this observation.")
   (snapshot
    :initarg :snapshot
    :reader agenda-observation-snapshot
    :type list
    :documentation "The exact detached readable agenda snapshot, including empty state."))
  (:documentation "An exact rendered and structural snapshot of the current workspace agenda."))

(defclass agenda-observation-state (resource-observation-state)
  ()
  (:documentation "One conversation-local model observation of agenda:current."))

(defmethod resource-observation-state-maximum ((state agenda-observation-state))
  "Return the configured agenda observation limit."
  (declare (ignore state))
  *agenda-resource-maximum-observations*)


;;;; -- URI Resolution --

(-> agenda-resource--current-directory (tool-context) non-empty-string)
(defun agenda-resource--current-directory (context)
  "Return CONTEXT's canonical current workspace agenda directory."
  (let ((configuration (tool-context-configuration context)))
    (agenda-directory-name configuration
                           (configuration-working-directory configuration)
                           :require-existing-p t)))

(defmethod resource-resolver-resolve
    ((resolver agenda-resolver) identifier (context tool-context))
  "Resolve only CURRENT without granting access to another workspace agenda."
  (declare (ignore resolver))
  (unless (string= identifier "current")
    (error 'agenda-resource-identifier-unsupported :identifier identifier))
  (make-instance 'agenda-resource
                 :uri       "agenda:current"
                 :directory (agenda-resource--current-directory context)))

(defmethod resource-capabilities
    ((resource agenda-resource) (context tool-context))
  "Return current agenda operations only while RESOURCE matches CONTEXT's workspace."
  (if (string= (agenda-resource-directory resource)
               (agenda-resource--current-directory context))
      '(:read :edit)
      nil))


;;;; -- Exact Agenda Observation --

(-> agenda-resource--snapshot (non-empty-string (option workspace-agenda)) list)
(defun agenda-resource--snapshot (directory record)
  "Return an exact detached snapshot for DIRECTORY and optional agenda RECORD."
  (list :directory directory
        :record (and record (agenda--record->form record))))

(-> agenda-resource--observation-from-state
    (agenda-resource agenda-state)
    agenda-observation)
(defun agenda-resource--observation-from-state (resource state)
  "Return RESOURCE's exact current-workspace observation from validated STATE."
  (let* ((configuration-directory (agenda-resource-directory resource))
         (record (agenda-find state configuration-directory))
         (snapshot (agenda-resource--snapshot configuration-directory record)))
    (make-instance 'agenda-observation
                   :uri       (resource-uri resource)
                   :revision  (resource-readable-snapshot-digest
                               *agenda-resource-digest-key*
                               snapshot)
                   :content   (agenda-tool--render-record record)
                   :directory configuration-directory
                   :snapshot  snapshot)))

(-> agenda-resource--read-current
    (agenda-resource tool-context)
    (values agenda-observation agenda-state))
(defun agenda-resource--read-current (resource context)
  "Strictly read RESOURCE's current state and return its observation and agenda state."
  (let ((current-directory (agenda-resource--current-directory context)))
    (unless (string= current-directory (agenda-resource-directory resource))
      (error 'resource-revision-stale
             :uri               (resource-uri resource)
             :expected-revision "workspace-context"
             :actual-revision   nil))
    (let ((state (agenda--read (tool-context-configuration context))))
      (values (agenda-resource--observation-from-state resource state)
              state))))

(defmethod resource-observe ((resource agenda-resource) (context tool-context))
  "Observe the complete current workspace agenda under the shared agenda lock."
  (with-recursive-lock-held (*agenda-lock*)
    (agenda-resource--read-current resource context)))


;;;; -- Conversation Observation State --


(-> agenda-resource--observation-state-for-snapshot
    (conversation agenda-observation)
    agenda-observation-state)
(defun agenda-resource--observation-state-for-snapshot (conversation observation)
  "Return or create CONVERSATION's state for exact agenda OBSERVATION."
  (with-recursive-lock-held
      ((conversation-resource-observation-lock conversation))
    (let* ((states (conversation-resource-observations conversation))
           (matching
             (loop for state being the hash-values of states
                   when (typep state 'agenda-observation-state)
                     do
                        (let ((existing
                                (resource-observation-state-observation state)))
                          (when (and
                                 (string= (resource-observation-uri existing)
                                          (resource-observation-uri observation))
                                 (string= (agenda-observation-directory existing)
                                          (agenda-observation-directory observation))
                                 (string= (resource-observation-revision existing)
                                          (resource-observation-revision observation))
                                 (equal (agenda-observation-snapshot existing)
                                        (agenda-observation-snapshot observation)))
                            (return state))))))
      (when matching
        (return-from agenda-resource--observation-state-for-snapshot matching))
      (let* ((alias (resource-observation-state-new-alias states))
             (state (make-instance 'agenda-observation-state
                                   :alias       alias
                                   :observation observation)))
        (setf (gethash alias states) state
              (conversation-resource-observation-order conversation)
              (append (conversation-resource-observation-order conversation)
                      (list alias)))
        (resource-observation-state-trim conversation state)
        state))))

(-> agenda-resource--find-observation-state
    (conversation agenda-resource non-empty-string)
    agenda-observation-state)
(defun agenda-resource--find-observation-state (conversation resource alias)
  "Return CONVERSATION's exact RESOURCE observation ALIAS or signal staleness."
  (let ((state
          (resource-observation-state-find
           (conversation-resource-observations conversation)
           alias
           'agenda-observation-state)))
    (unless (and state
                 (let ((observation
                         (resource-observation-state-observation state)))
                   (and (string= (resource-uri resource)
                                 (resource-observation-uri observation))
                        (string= (agenda-resource-directory resource)
                                 (agenda-observation-directory observation)))))
      (error 'resource-revision-stale
             :uri               (resource-uri resource)
             :expected-revision alias
             :actual-revision   nil))
    state))


;;;; -- Agenda Operations --

(-> agenda-resource--validate-operation-fields
    (json-object list list)
    null)
(defun agenda-resource--validate-operation-fields (operation allowed required)
  "Require OPERATION to contain exactly ALLOWED fields and every REQUIRED field."
  (loop for name being the hash-keys of operation
        unless (member name allowed :test #'string=)
          do
             (error 'tool-error
                    :message (format nil
                                     "Agenda resource operation does not accept field ~A."
                                     name)
                    :tool-name "resource.edit"))
  (dolist (name required)
    (multiple-value-bind (value present-p)
        (gethash name operation)
      (declare (ignore value))
      (unless present-p
        (error 'tool-error
               :message (format nil
                                "Agenda resource operation requires ~A."
                                name)
               :tool-name "resource.edit"))))
  nil)

(-> agenda-resource--normalize-operation (t) list)
(defun agenda-resource--normalize-operation (operation)
  "Return one validated normalized agenda resource OPERATION."
  (unless (hash-table-p operation)
    (error 'tool-error
           :message "Agenda resource operations must be JSON objects."
           :tool-name "resource.edit"))
  (let ((name (tool-argument operation "op" :required t)))
    (unless (stringp name)
      (error 'tool-error
             :message "Agenda resource operation op must be a string."
             :tool-name "resource.edit"))
    (cond
      ((string= name "agenda-add")
       (agenda-resource--validate-operation-fields
        operation
        '("op" "text" "status" "memory-ids")
        '("op" "text"))
       (multiple-value-bind (memory-identifiers memory-identifiers-supplied-p)
           (agenda-tool--memory-identifiers operation "resource.edit")
         (declare (ignore memory-identifiers-supplied-p))
         (list :kind ':add
               :text (agenda-tool--string-argument
                      operation "text" "resource.edit" :required t)
               :status (or (agenda-tool--status operation) ':todo)
               :memory-identifiers memory-identifiers)))
      ((string= name "agenda-update")
       (agenda-resource--validate-operation-fields
        operation
        '("op" "id" "text" "status" "memory-ids")
        '("op" "id"))
       (let ((identifier
               (agenda-tool--string-argument
                operation "id" "resource.edit" :required t))
             (text
               (agenda-tool--string-argument
                operation "text" "resource.edit"))
             (status (agenda-tool--status operation)))
         (multiple-value-bind (memory-identifiers memory-identifiers-supplied-p)
             (agenda-tool--memory-identifiers operation "resource.edit")
           (unless (or text status memory-identifiers-supplied-p)
             (error 'tool-error
                    :message "agenda-update requires text, status, or memory-ids."
                    :tool-name "resource.edit"))
           (list :kind ':update
                 :identifier identifier
                 :text text
                 :status status
                 :memory-identifiers memory-identifiers
                 :memory-identifiers-supplied-p
                 memory-identifiers-supplied-p))))
      ((string= name "agenda-remove")
       (agenda-resource--validate-operation-fields
        operation '("op" "id") '("op" "id"))
       (list :kind ':remove
             :identifier
             (agenda-tool--string-argument
              operation "id" "resource.edit" :required t)))
      (t
       (error 'tool-error
              :message
              "Agenda resource operation op must be agenda-add, agenda-update, or agenda-remove."
              :tool-name "resource.edit")))))

(-> agenda-resource--apply-operation
    (configuration agenda-state list)
    non-empty-string)
(defun agenda-resource--apply-operation (configuration state operation)
  "Apply one normalized agenda OPERATION and return its concise summary."
  (case (getf operation :kind)
    (:add
     (let ((item
             (agenda-add :configuration configuration
                         :state state
                         :text (getf operation :text)
                         :status (getf operation :status)
                         :memory-identifiers
                         (getf operation :memory-identifiers))))
       (format nil "Added agenda item ~A." (agenda-item-identifier item))))
    (:update
     (let ((item
             (apply #'agenda-update
                    configuration state (getf operation :identifier)
                    (append
                     (and (getf operation :text)
                          (list :text (getf operation :text)))
                     (and (getf operation :status)
                          (list :status (getf operation :status)))
                     (and (getf operation :memory-identifiers-supplied-p)
                          (list :memory-identifiers
                                (getf operation :memory-identifiers)))))))
       (format nil "Updated agenda item ~A." (agenda-item-identifier item))))
    (:remove
     (let ((identifier (getf operation :identifier)))
       (unless (agenda-remove configuration state identifier)
         (error 'tool-error
                :message (format nil "Agenda item ~A does not exist here."
                                 identifier)
                :tool-name "resource.edit"))
       (format nil "Removed agenda item ~A." identifier)))
    (otherwise
     (error 'tool-error
            :message "The normalized agenda resource operation is unsupported."
            :tool-name "resource.edit"))))

(defmethod resource-apply-operations
    ((resource agenda-resource) (context tool-context)
     &key base-revision operations)
  "Apply exactly one revision-gated operation to the current workspace agenda."
  (unless (and (listp operations) (= (length operations) 1))
    (error 'tool-error
           :message "agenda:current requires exactly one operation per resource.edit call."
           :tool-name "resource.edit"))
  (let ((conversation (tool-context-conversation context)))
    (with-recursive-lock-held (*agenda-lock*)
      (with-recursive-lock-held
          ((conversation-resource-observation-lock conversation))
        (let* ((state
                 (agenda-resource--find-observation-state
                  conversation resource base-revision))
               (base-observation
                 (resource-observation-state-observation state)))
          (multiple-value-bind (current current-state)
              (agenda-resource--read-current resource context)
            (unless (and
                     (string= (resource-observation-revision current)
                              (resource-observation-revision base-observation))
                     (equal (agenda-observation-snapshot current)
                            (agenda-observation-snapshot base-observation)))
              (error 'resource-revision-stale
                     :uri               (resource-uri resource)
                     :expected-revision base-revision
                     :actual-revision
                     (resource-observation-revision current)))
            (let* ((operation
                     (agenda-resource--normalize-operation (first operations)))
                   (summary
                     (agenda-resource--apply-operation
                      (tool-context-configuration context)
                      current-state
                      operation)))
              (values
               (agenda-resource--observation-from-state resource current-state)
               summary))))))))


;;;; -- Resource Tool Methods --

(-> agenda-resource--read-result
    (agenda-observation-state)
    non-empty-string)
(defun agenda-resource--read-result (state)
  "Return one explicit model-facing complete agenda observation result."
  (let ((observation (resource-observation-state-observation state)))
    (format nil "URI: ~A~%Revision: ~A~%Content:~%~A"
            (resource-observation-uri observation)
            (resource-observation-state-alias state)
            (resource-observation-content observation))))

(defmethod resource-tool-read
    ((resource agenda-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read agenda:current completely and establish its revision-gated observation."
  (declare (ignore tool))
  (when (or (nth-value 1 (gethash "start-line" arguments))
            (nth-value 1 (gethash "line-count" arguments)))
    (error 'tool-error
           :message "agenda:current is always read in full and does not accept line windows."
           :tool-name "resource.read"))
  (with-recursive-lock-held (*agenda-lock*)
    (let* ((observation (resource-observe resource context))
           (state
             (agenda-resource--observation-state-for-snapshot
              (tool-context-conversation context)
              observation)))
      (tool-success (agenda-resource--read-result state)))))

(defmethod resource-tool-edit
    ((resource agenda-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Apply one revision-gated agenda operation and return a fresh complete observation."
  (declare (ignore tool))
  (let* ((uri (tool-argument arguments "uri" :required t))
         (base-revision (tool-argument arguments "base-revision" :required t))
         (operation-array (tool-argument arguments "operations" :required t)))
    (unless (non-empty-string-p base-revision)
      (error 'tool-error
             :message "Resource edit base-revision must be a non-empty string."
             :tool-name "resource.edit"))
    (unless (and (vectorp operation-array) (= (length operation-array) 1))
      (error 'tool-error
             :message "agenda:current requires exactly one operation per resource.edit call."
             :tool-name "resource.edit"))
    (handler-case
        (multiple-value-bind (observation summary)
            (resource-apply-operations resource context
                                       :base-revision base-revision
                                       :operations (coerce operation-array 'list))
          (let ((state
                  (agenda-resource--observation-state-for-snapshot
                   (tool-context-conversation context)
                   observation)))
            (tool-success
             (format nil "~A~%~A"
                     summary
                     (agenda-resource--read-result state)))))
      (resource-revision-stale ()
        (tool-failure
         (format nil "Resource revision ~A is stale, expired, from another workspace, or was not observed in this conversation. Reread ~A with resource.read and retry against the returned revision."
                 base-revision uri))))))
