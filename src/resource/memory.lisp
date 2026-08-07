(in-package #:autolith)

;;;; -- Memory Resource Conditions --

(define-condition memory-resource-not-found (autolith-error)
  ((identifier
    :initarg :identifier
    :reader memory-resource-not-found-identifier
    :type non-empty-string
    :documentation "The stable memory identifier that has no active value."))
  (:default-initargs :message "The persistent memory does not exist.")
  (:documentation "A memory: URI names a missing or forgotten persistent memory.")
  (:report (lambda (condition stream)
             (format stream "Memory ~A does not exist."
                     (memory-resource-not-found-identifier condition)))))


;;;; -- Memory Resources --

(defparameter *memory-resource-maximum-observations* 16
  "The transient memory observations retained by one conversation.")

(defparameter *memory-resource-excerpt-limit* 240
  "The maximum memory-body characters shown for one collection entry.")

(defvar *memory-resource-digest-key* (random-data 16)
  "The process-local key identifying exact persistent-memory snapshots.")

(defclass memory-resource (resource)
  ((identifier
    :initarg :identifier
    :reader memory-resource-identifier
    :type non-empty-string
    :documentation "The collection name or stable memory identifier in this resource URI."))
  (:documentation "A read-only persistent-memory resource."))

(defclass memory-collection-resource (memory-resource)
  ((visibility
    :initarg :visibility
    :reader memory-collection-resource-visibility
    :type memory-visibility
    :documentation "The existing persistent-memory visibility selected by this collection."))
  (:documentation "A scoped collection of active persistent memories."))

(defclass memory-item-resource (memory-resource)
  ()
  (:documentation "One exact active persistent memory selected by stable identifier."))

(defclass memory-resolver (resource-resolver)
  ()
  (:documentation "Resolve scoped memory collections and exact active memories."))

(defclass memory-observation (resource-observation)
  ((identifier
    :initarg :identifier
    :reader memory-observation-identifier
    :type non-empty-string
    :documentation "The collection name or stable memory identifier observed.")
   (kind
    :initarg :kind
    :reader memory-observation-kind
    :type keyword
    :documentation "Whether the observation represents a collection or exact item.")
   (snapshot
    :initarg :snapshot
    :reader memory-observation-snapshot
    :type list
    :documentation "The exact detached readable memory snapshot, including empty state."))
  (:documentation "An exact rendered and structural persistent-memory snapshot."))

(defclass memory-observation-state (resource-observation-state)
  ()
  (:documentation "One conversation-local model observation of a memory: resource."))


;;;; -- URI Resolution --

(-> memory-resource--collection-visibility
    (non-empty-string)
    (option memory-visibility))
(defun memory-resource--collection-visibility (identifier)
  "Return the reserved collection visibility named by IDENTIFIER, or NIL."
  (cond
    ((string= identifier "relevant") ':relevant)
    ((string= identifier "workspace") ':workspace)
    ((string= identifier "global") ':global)
    (t
     nil)))

(defmethod resource-resolver-resolve
    ((resolver memory-resolver) identifier (context tool-context))
  "Resolve reserved collections or one exact active memory under CONTEXT."
  (declare (ignore resolver))
  (let ((visibility (memory-resource--collection-visibility identifier)))
    (cond
      (visibility
       (make-instance 'memory-collection-resource
                      :uri        (format nil "memory:~A" identifier)
                      :identifier identifier
                      :visibility visibility))
      ((memory-find (tool-context-configuration context) identifier)
       (make-instance 'memory-item-resource
                      :uri        (format nil "memory:~A" identifier)
                      :identifier identifier))
      (t
       (error 'memory-resource-not-found :identifier identifier)))))

(defmethod resource-capabilities
    ((resource memory-resource) (context tool-context))
  "Expose only read capability after authority-confined memory resolution."
  (declare (ignore resource context))
  '(:read))


;;;; -- Exact Memory Observations --

(-> memory-resource--digest (list) non-empty-string)
(defun memory-resource--digest (snapshot)
  "Return a full process-local digest for exact memory SNAPSHOT."
  (let ((mac (make-mac ':siphash
                       *memory-resource-digest-key*
                       :digest-length 16))
        (content
          (with-standard-io-syntax
            (let ((*print-readably* t))
              (prin1-to-string snapshot)))))
    (update-mac mac
                (sb-ext:string-to-octets content :external-format :utf-8))
    (with-output-to-string (stream)
      (loop for octet across (produce-mac mac)
            do (format stream "~2,'0X" octet)))))

(-> memory-resource--collection-entry (memory) string)
(defun memory-resource--collection-entry (memory)
  "Return one complete metadata and bounded excerpt entry for MEMORY."
  (format nil
          "id: ~A~%scope: ~A~%created: ~A~%updated: ~A~%source conversation: ~A~%title: ~A~%tags: ~:[none~;~:*~{~A~^, ~}~]~%excerpt: ~A"
          (memory-identifier memory)
          (memory-tool--scope-label memory)
          (memory--timestamp-string (memory-created-at memory))
          (memory--timestamp-string (memory-updated-at memory))
          (if (memory-source-conversation memory)
              (conversation-identifier-display
               (memory-source-conversation memory))
              "unknown")
          (memory-title memory)
          (memory-tags memory)
          (memory--excerpt (memory-content memory)
                           *memory-resource-excerpt-limit*)))

(-> memory-resource--render-collection (non-empty-string list) string)
(defun memory-resource--render-collection (identifier memories)
  "Return a clear complete rendering of collection IDENTIFIER and MEMORIES."
  (if memories
      (format nil "collection: ~A~%count: ~D~2%~{~A~^~2%~}"
              identifier
              (length memories)
              (mapcar #'memory-resource--collection-entry memories))
      (format nil "collection: ~A~%count: 0~2%No matching memories."
              identifier)))

(-> memory-resource--collection-observation
    (memory-collection-resource tool-context)
    memory-observation)
(defun memory-resource--collection-observation (resource context)
  "Return RESOURCE's exact scoped collection observation under CONTEXT."
  (let* ((memories
           (memory-list
            (tool-context-configuration context)
            :visibility (memory-collection-resource-visibility resource)))
         (snapshot
           (list :kind ':collection
                 :identifier (memory-resource-identifier resource)
                 :records (mapcar #'memory--record memories))))
    (make-instance 'memory-observation
                   :uri        (resource-uri resource)
                   :revision   (memory-resource--digest snapshot)
                   :content    (memory-resource--render-collection
                                (memory-resource-identifier resource)
                                memories)
                   :metadata   (list :visibility
                                     (memory-collection-resource-visibility resource)
                                     :count (length memories))
                   :identifier (memory-resource-identifier resource)
                   :kind       ':collection
                   :snapshot   snapshot)))

(-> memory-resource--item-observation
    (memory-item-resource tool-context)
    memory-observation)
(defun memory-resource--item-observation (resource context)
  "Return RESOURCE's exact active memory observation under CONTEXT."
  (let* ((identifier (memory-resource-identifier resource))
         (memory
           (memory-find (tool-context-configuration context) identifier)))
    (unless memory
      (error 'memory-resource-not-found :identifier identifier))
    (let ((snapshot
            (list :kind ':item
                  :identifier identifier
                  :record (memory--record memory))))
      (make-instance 'memory-observation
                     :uri        (resource-uri resource)
                     :revision   (memory-resource--digest snapshot)
                     :content    (memory-tool--render-memory memory)
                     :metadata   (list :scope (memory-scope memory))
                     :identifier identifier
                     :kind       ':item
                     :snapshot   snapshot))))

(defmethod resource-observe
    ((resource memory-collection-resource) (context tool-context))
  "Observe one complete scoped persistent-memory collection."
  (memory-resource--collection-observation resource context))

(defmethod resource-observe
    ((resource memory-item-resource) (context tool-context))
  "Observe one complete exact active persistent memory."
  (memory-resource--item-observation resource context))


;;;; -- Conversation Observation State --

(-> memory-resource--observation-state-count (hash-table) (integer 0))
(defun memory-resource--observation-state-count (states)
  "Return the number of memory observation STATES."
  (loop for state being the hash-values of states
        count (typep state 'memory-observation-state)))

(-> memory-resource--expire-oldest-observation-state
    (conversation hash-table)
    boolean)
(defun memory-resource--expire-oldest-observation-state (conversation states)
  "Expire the oldest memory state without disturbing other resource observations."
  (loop for alias in (conversation-resource-observation-order conversation)
        for state = (resource-observation-state-find
                     states alias 'memory-observation-state)
        when state
          do
             (setf (conversation-resource-observation-order conversation)
                   (remove alias
                           (conversation-resource-observation-order conversation)
                           :test #'string=
                           :count 1))
             (remhash alias states)
             (return t)
        finally (return nil)))

(-> memory-resource--observation-state-for-snapshot
    (conversation memory-observation)
    memory-observation-state)
(defun memory-resource--observation-state-for-snapshot (conversation observation)
  "Return or create CONVERSATION's state for exact memory OBSERVATION."
  (with-recursive-lock-held
      ((conversation-resource-observation-lock conversation))
    (let* ((states (conversation-resource-observations conversation))
           (matching
             (loop for state being the hash-values of states
                   when (typep state 'memory-observation-state)
                     do
                        (let ((existing
                                (resource-observation-state-observation state)))
                          (when (and
                                 (string= (resource-observation-uri existing)
                                          (resource-observation-uri observation))
                                 (string= (resource-observation-revision existing)
                                          (resource-observation-revision observation))
                                 (equal (memory-observation-snapshot existing)
                                        (memory-observation-snapshot observation)))
                            (return state))))))
      (when matching
        (return-from memory-resource--observation-state-for-snapshot matching))
      (let* ((states (conversation-resource-observations conversation))
             (alias (resource-observation-state-new-alias states))
             (state (make-instance 'memory-observation-state
                                   :alias       alias
                                   :observation observation)))
        (setf (gethash alias states) state
              (conversation-resource-observation-order conversation)
              (append (conversation-resource-observation-order conversation)
                      (list alias)))
        (loop while (> (memory-resource--observation-state-count states)
                       *memory-resource-maximum-observations*)
              while (memory-resource--expire-oldest-observation-state
                     conversation states))
        state))))


;;;; -- Resource Tool Method --

(-> memory-resource--read-result
    (memory-observation-state)
    non-empty-string)
(defun memory-resource--read-result (state)
  "Return one explicit model-facing complete memory observation result."
  (let ((observation (resource-observation-state-observation state)))
    (format nil "URI: ~A~%Revision: ~A~%Content:~%~A"
            (resource-observation-uri observation)
            (resource-observation-state-alias state)
            (resource-observation-content observation))))

(defmethod resource-tool-read
    ((resource memory-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read one memory resource completely and retain its transient exact snapshot."
  (declare (ignore tool))
  (when (or (nth-value 1 (gethash "start-line" arguments))
            (nth-value 1 (gethash "line-count" arguments)))
    (error 'tool-error
           :message "memory: resources are always read in full and do not accept line windows."
           :tool-name "resource.read"))
  (let* ((observation (resource-observe resource context))
         (state
           (memory-resource--observation-state-for-snapshot
            (tool-context-conversation context)
            observation)))
    (tool-success (memory-resource--read-result state))))
