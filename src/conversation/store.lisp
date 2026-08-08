(in-package #:autolith)

;;;; -- Conversation Object --

(defclass conversation ()
  ((identifier
    :initarg :identifier
    :reader conversation-identifier
    :type non-empty-string
    :documentation "The stable conversation identifier.")
   (prompt-cache-key
    :initarg :prompt-cache-key
    :initform nil
    :accessor conversation-prompt-cache-key
    :type (option non-empty-string)
    :documentation "The root-lineage key shared by provider prompt caches.")
   (pathname
    :initarg :pathname
    :reader conversation-pathname
    :type pathname
    :documentation "The append-only S-expression file once persistence begins.")
   (persisted-p
    :initarg :persisted-p
    :accessor conversation-persisted-p
    :type boolean
    :documentation "True after the header and first durable record are published.")
   (incomplete-tail-p
    :initarg :incomplete-tail-p
    :initform nil
    :accessor conversation-incomplete-tail-p
    :type boolean
    :documentation "Whether the next append must repair an interrupted final form.")
   (log-generation
    :initform 0
    :accessor conversation-log-generation
    :type (integer 0)
    :documentation "The count of whole-log replacements since this object loaded.")
   (append-lock
    :initform (make-recursive-lock "Autolith conversation append")
    :reader conversation-append-lock
    :type t
    :documentation "The lock serializing durable record sequence assignment.")
   (created-at
    :initarg :created-at
    :reader conversation-created-at
    :type timestamp
    :documentation "The creation time as Common Lisp universal time.")
   (origin-directory
    :initarg :origin-directory
    :initform nil
    :reader conversation-origin-directory
    :type (option string)
    :documentation "The workspace directory in which this conversation began.")
   (model
    :initarg :model
    :initform nil
    :accessor conversation-model
    :type (option non-empty-string)
    :documentation "The provider model most recently selected for this conversation.")
   (reasoning-effort
    :initarg :reasoning-effort
    :initform nil
    :accessor conversation-reasoning-effort
    :type (option non-empty-string)
    :documentation "The reasoning effort most recently selected for this conversation.")
   (next-sequence
    :initarg :next-sequence
    :accessor conversation-next-sequence
    :type integer
    :documentation "The sequence number assigned to the next appended event.")
   (input-items
    :initarg :input-items
    :accessor conversation-input-items
    :type list
    :documentation "Provider wire items in chronological order.")
   (input-items-tail
    :initform nil
    :accessor conversation-input-items-tail
    :type list
    :documentation "The final cons of the provider projection for constant-time append.")
   (ephemeral-input-entries
    :initform nil
    :accessor conversation-ephemeral-input-entries
    :type list
    :documentation
    "Request-local provider items and owned attachments awaiting one response.")
   (resource-observations
    :initform (make-hash-table :test #'equal)
    :reader conversation-resource-observations
    :type hash-table
    :documentation
    "Transient model-visible resource revisions keyed by opaque alias.")
   (resource-observation-order
    :initform nil
    :accessor conversation-resource-observation-order
    :type list
    :documentation
    "Oldest-first aliases bounding transient resource observation retention.")
   (resource-observation-lock
    :initform (make-recursive-lock "Autolith resource observations")
    :reader conversation-resource-observation-lock
    :type t
    :documentation
    "The lock serializing transient resource observations and gated edits.")
   (input-item-families
    :initform (make-hash-table :test #'eq)
    :reader conversation-input-item-families
    :type hash-table
    :documentation
    "The model family that produced each projected item, keyed by item.")
   (turn-state
    :initform nil
    :accessor conversation-turn-state
    :type (option string)
    :documentation "The transient provider routing token for one user turn.")
   (last-total-tokens
    :initform 0
    :accessor conversation-last-total-tokens
    :type (integer 0)
    :documentation "The total token usage reported by the newest provider step.")
   (last-activity-at
    :initform nil
    :accessor conversation-last-activity-at
    :type (option timestamp)
    :documentation "The newest timestamp observed in a durable record.")
   (user-turn-count
    :initform 0
    :accessor conversation-user-turn-count
    :type (integer 0)
    :documentation "The number of durable user message records.")
   (working-seconds
    :initform 0
    :accessor conversation-working-seconds
    :type (integer 0)
    :documentation
    "Accumulated seconds of agent work, excluding gaps before user messages.")
   (picker-search-messages
    :initform nil
    :accessor conversation-picker-search-messages
    :type list
    :documentation "Chronological durable user and assistant message text.")
   (picker-search-messages-tail
    :initform nil
    :accessor conversation-picker-search-messages-tail
    :type list
    :documentation "The final message-text cons for constant-time search indexing.")
   (picker-search-message-count
    :initform 0
    :accessor conversation-picker-search-message-count
    :type (integer 0)
    :documentation "The count of durable user and assistant search messages.")
   (picker-preview
    :initform nil
    :accessor conversation-picker-preview
    :type (option string)
    :documentation "The newest user or assistant text retained for conversation pickers.")
   (latest-goal-record
    :initform nil
    :accessor conversation-latest-goal-record
    :type (option list)
    :documentation "The newest durable goal record observed in this conversation."))
  (:documentation "An append-only conversation and its provider projection."))

(defmethod initialize-instance
    :after ((conversation conversation) &key &allow-other-keys)
  "Initialize CONVERSATION's constant-time projection tails."
  (unless (conversation-prompt-cache-key conversation)
    (setf (conversation-prompt-cache-key conversation)
          (conversation-identifier conversation)))
  (setf (conversation-input-items-tail conversation)
        (last (conversation-input-items conversation))
        (conversation-picker-search-messages-tail conversation)
        (last (conversation-picker-search-messages conversation))
        (conversation-picker-search-message-count conversation)
        (length (conversation-picker-search-messages conversation))))

(defmethod (setf conversation-input-items)
    :around ((items list) (conversation conversation))
  "Serialize whole provider-projection replacements with incremental appends."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (call-next-method)))

(defmethod (setf conversation-input-items)
    :after ((items list) (conversation conversation))
  "Keep CONVERSATION's projection tail and item families synchronized.

Compaction replaces the whole projection, so families recorded for
discarded items are pruned here rather than retained for the session."
  (setf (conversation-input-items-tail conversation) (last items))
  (let ((families (conversation-input-item-families conversation)))
    (when (plusp (hash-table-count families))
      (let ((retained (make-hash-table :test #'eq)))
        (dolist (item items)
          (multiple-value-bind (family present-p) (gethash item families)
            (when present-p
              (setf (gethash item retained) family))))
        (clrhash families)
        (maphash (lambda (item family)
                   (setf (gethash item families) family))
                 retained)))))


(defclass conversation-picker-metadata ()
  ((source-size
    :initarg :source-size
    :reader conversation-picker-metadata-source-size
    :type (integer 0)
    :documentation "The byte size of the indexed conversation file.")
   (source-write-date
    :initarg :source-write-date
    :reader conversation-picker-metadata-source-write-date
    :type (integer 0)
    :documentation "The indexed conversation file's write date.")
   (source-revision
    :initarg :source-revision
    :reader conversation-picker-metadata-source-revision
    :type (integer 0)
    :documentation "The durable picker-cache revision captured with the log.")
   (working-seconds
    :initarg :working-seconds
    :reader conversation-picker-metadata-working-seconds
    :type (integer 0)
    :documentation "The accumulated durable agent-working seconds.")
   (user-turn-count
    :initarg :user-turn-count
    :reader conversation-picker-metadata-user-turn-count
    :type (integer 0)
    :documentation "The count of durable user-message records.")
   (search-message-count
    :initarg :search-message-count
    :reader conversation-picker-metadata-search-message-count
    :type (integer 0)
    :documentation "The count of indexed durable user and assistant messages.")
   (preview
    :initarg :preview
    :reader conversation-picker-metadata-preview
    :type (option string)
    :documentation "The newest user or assistant message text.")
   (incomplete-tail-p
    :initarg :incomplete-tail-p
    :reader conversation-picker-metadata-incomplete-tail-p
    :type boolean
    :documentation "Whether indexing stopped before an interrupted final form."))
  (:documentation "A validated compact cache of one conversation's picker fields."))


(defclass conversation-picker-search-index ()
  ((source-revision
    :initarg :source-revision
    :reader conversation-picker-search-index-source-revision
    :type (integer 0)
    :documentation "The durable message-search revision captured with the log.")
   (message-count
    :initarg :message-count
    :reader conversation-picker-search-index-message-count
    :type (integer 0)
    :documentation "The count of indexed durable user and assistant messages.")
   (messages
    :initarg :messages
    :reader conversation-picker-search-index-messages
    :type list
    :documentation "Chronological durable user and assistant message text."))
  (:documentation "A validated durable search index for one conversation picker."))


;;;; -- Primary Application Ownership --

(defclass conversation-lease ()
  ((identifier
    :initarg :identifier
    :reader conversation-lease-identifier
    :type non-empty-string
    :documentation "The normalized conversation identifier held by this lease.")
   (pathname
    :initarg :pathname
    :reader conversation-lease-pathname
    :type pathname
    :documentation "The persistent file carrying the process-shared advisory lock.")
   (descriptor
    :initarg :descriptor
    :accessor conversation-lease-descriptor
    :type (option integer)
    :documentation "The open descriptor holding the advisory lock, or NIL after release."))
  (:documentation
   "A process-lifetime exclusive lease on one primary conversation."))

(defvar *conversation-lease-lock*
  (make-lock "Autolith conversation leases")
  "Serialize process-local lease registration and descriptor release.")

(defvar *conversation-leases*
  (make-hash-table :test #'equal)
  "Map lease pathnames to held primary-conversation leases in this process.")

(-> conversation--lease-pathname (configuration string) pathname)
(defun conversation--lease-pathname (configuration identifier)
  "Return the process-shared lease pathname for normalized IDENTIFIER."
  (merge-pathnames
   (make-pathname :name identifier :type "lock")
   (merge-pathnames
    "conversation-leases/"
    (configuration-state-root configuration))))

(-> conversation--lease-in-use (string pathname pathname) null)
(defun conversation--lease-in-use
    (identifier conversation-pathname lease-pathname)
  "Signal that normalized IDENTIFIER already has a live primary owner."
  (error
   'conversation-in-use
   :message
   (format
    nil
    "Conversation ~A is already active in another Autolith process."
    (conversation-identifier-display identifier))
   :pathname conversation-pathname
   :sequence nil
   :identifier identifier
   :lease-pathname lease-pathname))

(-> conversation-lease-held-p (conversation-lease) boolean)
(defun conversation-lease-held-p (lease)
  "Return true when LEASE still owns an open lock descriptor."
  (not (null (conversation-lease-descriptor lease))))

(-> conversation-lease-matches-p (conversation-lease string) boolean)
(defun conversation-lease-matches-p (lease identifier)
  "Return true when held LEASE owns normalized IDENTIFIER."
  (and (conversation-lease-held-p lease)
       (string= (conversation-lease-identifier lease) identifier)))

(-> conversation-lease-acquire (configuration string) conversation-lease)
(defun conversation-lease-acquire (configuration identifier)
  "Acquire the primary process lease for IDENTIFIER without waiting.

The kernel lock is authoritative. Its empty file may remain after normal exit
or a crash, and a later process can immediately reuse it after the former
owner has exited."
  (let* ((normalized
           (conversation-identifier-migration-resolve
            configuration identifier))
         (conversation-pathname
           (conversation-pathname-for-id configuration normalized))
         (lease-pathname
           (conversation--lease-pathname configuration normalized))
         (lease-key (namestring lease-pathname))
         (descriptor nil)
         (acquired-p nil))
    (with-lock-held (*conversation-lease-lock*)
      (let ((existing (gethash lease-key *conversation-leases*)))
        (when (and existing (conversation-lease-held-p existing))
          (conversation--lease-in-use
           normalized conversation-pathname lease-pathname))
        (when existing
          (remhash lease-key *conversation-leases*)))
      (unwind-protect
           (handler-case
               (progn
                 (ensure-directories-exist lease-pathname)
                 (setf descriptor
                       (sb-posix:open
                        (namestring lease-pathname)
                        (logior sb-posix:o-creat sb-posix:o-rdwr)
                        #o600))
                 (sb-posix:lockf descriptor sb-posix:f-tlock 0)
                 (let ((lease
                         (make-instance
                          'conversation-lease
                          :identifier normalized
                          :pathname lease-pathname
                          :descriptor descriptor)))
                   (setf acquired-p t
                         (gethash lease-key *conversation-leases*) lease)
                   lease))
             (sb-posix:syscall-error (condition)
               (if (and
                    descriptor
                    (member
                     (sb-posix:syscall-errno condition)
                     (list sb-posix:eacces sb-posix:eagain)))
                   (conversation--lease-in-use
                    normalized conversation-pathname lease-pathname)
                   (error
                    'conversation-invariant-error
                    :message
                    (format nil
                            "Could not claim conversation ~A: ~A"
                            (conversation-identifier-display normalized)
                            condition)
                    :pathname conversation-pathname
                    :sequence nil)))
             (conversation-error (condition)
               (error condition))
             (error (condition)
               (error
                'conversation-invariant-error
                :message
                (format nil
                        "Could not claim conversation ~A: ~A"
                        (conversation-identifier-display normalized)
                        condition)
                :pathname conversation-pathname
                :sequence nil)))
        (unless acquired-p
          (when descriptor
            (ignore-errors
              (sb-posix:close descriptor))))))))

(-> conversation-lease-release (conversation-lease) null)
(defun conversation-lease-release (lease)
  "Release LEASE idempotently.

Closing the descriptor is the final authority even when an explicit unlock
reports an operating-system failure."
  (with-lock-held (*conversation-lease-lock*)
    (let ((descriptor (conversation-lease-descriptor lease))
          (lease-key (namestring (conversation-lease-pathname lease))))
      (when descriptor
        (when (eq (gethash lease-key *conversation-leases*) lease)
          (remhash lease-key *conversation-leases*))
        (setf (conversation-lease-descriptor lease) nil)
        (ignore-errors
          (sb-posix:lockf descriptor sb-posix:f-ulock 0))
        (ignore-errors
          (sb-posix:close descriptor)))))
  nil)


;;;; -- Durable Projection --

(-> conversation--activity-after-record
    (list &key (:working-seconds (integer 0))
               (:user-turn-count (integer 0))
               (:last-activity-at (option timestamp)))
    (values (integer 0) (integer 0) (option timestamp)))
(defun conversation--activity-after-record
    (record &key (working-seconds 0) (user-turn-count 0) last-activity-at)
  "Return activity values after applying durable RECORD to a picker summary."
  (let ((time (and (consp record) (getf (rest record) :time)))
        (user-message-p
          (and (consp record)
               (eq (first record) ':message)
               (eq (getf (rest record) :role) ':user))))
    (when (typep time 'timestamp)
      (when (and last-activity-at
                 (not user-message-p)
                 (> time last-activity-at))
        (incf working-seconds (- time last-activity-at)))
      (setf last-activity-at (max (or last-activity-at 0) time)))
    (when user-message-p
      (incf user-turn-count))
    (values working-seconds user-turn-count last-activity-at)))

(-> conversation--assistant-preview (json-object) (option string))
(defun conversation--assistant-preview (item)
  "Return the visible assistant text in provider response ITEM, when present."
  (when (and (string= (or (json-get item "type") "") "message")
             (string= (or (json-get item "role") "") "assistant"))
    (let ((content (json-get item "content")))
      (when (vectorp content)
        (let ((parts
                (loop for part across content
                      when (and (json-object-p part)
                                (member (json-get part "type")
                                        '("output_text" "text")
                                        :test #'string=)
                                (stringp (json-get part "text")))
                        collect (json-get part "text"))))
          (when parts
            (format nil "~{~A~^~%~}" parts)))))))

(-> conversation--record-preview (list) (option string))
(defun conversation--record-preview (record)
  "Return the user or assistant text represented by durable RECORD."
  (case (first record)
    (:message
     (let ((content (getf (rest record) :content)))
       (when (and (eq (getf (rest record) :role) ':user)
                  (stringp content))
         content)))
    (:provider-item
     (let ((wire-json (getf (rest record) :wire-json)))
       (when (stringp wire-json)
         (handler-case
             (let ((item (json-decode wire-json)))
               (when (json-object-p item)
                 (conversation--assistant-preview item)))
           (error ()
             nil)))))))


(-> conversation--note-picker-search-message (conversation string) string)
(defun conversation--note-picker-search-message (conversation message)
  "Retain MESSAGE as the newest preview and append it to the search corpus."
  (setf (conversation-picker-preview conversation) message)
  (let ((cell (list message))
        (tail (conversation-picker-search-messages-tail conversation)))
    (if tail
        (setf (rest tail) cell)
        (setf (conversation-picker-search-messages conversation) cell))
    (setf (conversation-picker-search-messages-tail conversation) cell)
    (incf (conversation-picker-search-message-count conversation))
    message))

(-> conversation--note-activity (conversation list) null)
(defun conversation--note-activity (conversation record)
  "Project RECORD's activity metadata into CONVERSATION."
  (multiple-value-bind (working-seconds user-turn-count last-activity-at)
      (conversation--activity-after-record
       record
       :working-seconds (conversation-working-seconds conversation)
       :user-turn-count (conversation-user-turn-count conversation)
       :last-activity-at (conversation-last-activity-at conversation))
    (setf (conversation-working-seconds conversation) working-seconds
          (conversation-user-turn-count conversation) user-turn-count
          (conversation-last-activity-at conversation) last-activity-at))
  nil)

(-> conversation--header-record (conversation) list)
(defun conversation--header-record (conversation)
  "Return CONVERSATION's portable file header."
  (list :conversation
        :version 1
        :id (conversation-identifier conversation)
        :created-at (conversation-created-at conversation)
        :directory (conversation-origin-directory conversation)
        :model (conversation-model conversation)
        :reasoning-effort (conversation-reasoning-effort conversation)))

(-> conversation--write-initial-record (conversation list) null)
(defun conversation--write-initial-record (conversation record)
  "Atomically publish CONVERSATION's header and first durable RECORD."
  (let ((pathname (conversation-pathname conversation)))
    (when (probe-file pathname)
      (error 'conversation-invariant-error
             :message "A new conversation pathname became occupied."
             :pathname pathname
             :sequence (conversation-next-sequence conversation)))
    (log-append pathname
                record
                :initial-forms
                (list (conversation--header-record conversation)))
    (setf (conversation-persisted-p conversation) t
          (conversation-incomplete-tail-p conversation) nil))
  nil)

;;;; -- Conversation Picker Metadata --


(-> conversation-picker-revision-read (pathname) (integer 0))
(defun conversation-picker-revision-read (conversation-pathname)
  "Return CONVERSATION-PATHNAME's durable picker-cache revision, or zero."
  (let ((revision-pathname
          (conversation-picker-revision-pathname conversation-pathname)))
    (if (probe-file revision-pathname)
        (handler-case
            (multiple-value-bind (record complete-p)
                (snapshot-read revision-pathname)
              (let ((revision (and complete-p
                                   (listp record)
                                   (eq (first record) :conversation-picker-revision)
                                   (= (or (getf (rest record) :version) 0) 1)
                                   (getf (rest record) :value))))
                (if (typep revision '(integer 0))
                    revision
                    0)))
          (error ()
            0))
        0)))

(-> conversation-picker-revision-write (pathname (integer 0)) (integer 0))
(defun conversation-picker-revision-write (conversation-pathname revision)
  "Atomically record REVISION before changing CONVERSATION-PATHNAME's log."
  (let ((revision-pathname
          (conversation-picker-revision-pathname conversation-pathname)))
    (ensure-directories-exist revision-pathname)
    (snapshot-write revision-pathname
                    (list :conversation-picker-revision
                          :version 1
                          :value revision))
    revision))

(-> conversation-picker-metadata-invalidate (conversation) (integer 0))
(defun conversation-picker-metadata-invalidate (conversation)
  "Advance CONVERSATION's picker revision before its durable log changes."
  (let ((pathname (conversation-pathname conversation)))
    (handler-case
        (conversation-picker-revision-write
         pathname
         (1+ (conversation-picker-revision-read pathname)))
      (error (condition)
        (error 'conversation-invariant-error
               :message (format nil
                                "Could not invalidate conversation picker metadata: ~A"
                                condition)
               :pathname pathname
               :sequence (conversation-next-sequence conversation))))))

(-> conversation--file-identity (pathname)
    (values (integer 0) (integer 0)))
(defun conversation--file-identity (pathname)
  "Return PATHNAME's byte size and write date for picker-cache validation."
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (values (file-length stream)
            (or (file-write-date pathname) 0))))

(-> conversation-picker-metadata-record (conversation-picker-metadata) list)
(defun conversation-picker-metadata-record (metadata)
  "Return METADATA as one portable atomically published picker-cache form."
  (list :conversation-picker-metadata
        :version 1
        :source-size (conversation-picker-metadata-source-size metadata)
        :source-write-date
        (conversation-picker-metadata-source-write-date metadata)
        :source-revision (conversation-picker-metadata-source-revision metadata)
        :working-seconds (conversation-picker-metadata-working-seconds metadata)
        :user-turn-count (conversation-picker-metadata-user-turn-count metadata)
        :search-message-count
        (conversation-picker-metadata-search-message-count metadata)
        :preview (conversation-picker-metadata-preview metadata)
        :incomplete-tail-p
        (conversation-picker-metadata-incomplete-tail-p metadata)))

(-> conversation-picker-metadata-from-record (t)
    (option conversation-picker-metadata))
(defun conversation-picker-metadata-from-record (record)
  "Return validated picker metadata represented by RECORD, or NIL."
  (when (and (listp record)
             (eq (first record) :conversation-picker-metadata)
             (= (or (getf (rest record) :version) 0) 1))
    (let ((source-size (getf (rest record) :source-size))
          (source-write-date (getf (rest record) :source-write-date))
          (source-revision (getf (rest record) :source-revision))
          (working-seconds (getf (rest record) :working-seconds))
          (user-turn-count (getf (rest record) :user-turn-count))
          (search-message-count (getf (rest record) :search-message-count))
          (preview (getf (rest record) :preview))
          (incomplete-tail-p (getf (rest record) :incomplete-tail-p)))
      (when (and (typep source-size '(integer 0))
                 (typep source-write-date '(integer 0))
                 (typep source-revision '(integer 0))
                 (typep working-seconds '(integer 0))
                 (typep user-turn-count '(integer 0))
                 (typep search-message-count '(integer 0))
                 (or (null preview) (stringp preview))
                 (typep incomplete-tail-p 'boolean))
        (make-instance 'conversation-picker-metadata
                       :source-size source-size
                       :source-write-date source-write-date
                       :source-revision source-revision
                       :working-seconds working-seconds
                       :user-turn-count user-turn-count
                       :search-message-count search-message-count
                       :preview preview
                       :incomplete-tail-p incomplete-tail-p)))))

(-> conversation-picker-metadata-read (pathname)
    (option conversation-picker-metadata))
(defun conversation-picker-metadata-read (pathname)
  "Return PATHNAME's valid picker cache, or NIL when it is absent or stale."
  (let ((metadata-pathname (conversation-picker-metadata-pathname pathname)))
    (when (probe-file metadata-pathname)
      (handler-case
          (multiple-value-bind (record complete-p)
              (snapshot-read metadata-pathname)
            (let ((metadata
                    (and complete-p
                         (conversation-picker-metadata-from-record record))))
              (when metadata
                (multiple-value-bind (size write-date)
                    (conversation--file-identity pathname)
                  (when (and (= size (conversation-picker-metadata-source-size
                                      metadata))
                             (= write-date
                                (conversation-picker-metadata-source-write-date
                                 metadata))
                             (= (conversation-picker-revision-read pathname)
                                (conversation-picker-metadata-source-revision
                                 metadata)))
                    metadata)))))
        (error ()
          nil)))))

(-> conversation-picker-metadata-write
    (pathname conversation-picker-metadata)
    conversation-picker-metadata)
(defun conversation-picker-metadata-write (pathname metadata)
  "Atomically publish METADATA as the validated picker cache for PATHNAME."
  (let ((metadata-pathname (conversation-picker-metadata-pathname pathname)))
    (ensure-directories-exist metadata-pathname)
    (snapshot-write metadata-pathname
                    (conversation-picker-metadata-record metadata))
    metadata))

(-> conversation-picker-metadata-publish (conversation) null)
(defun conversation-picker-metadata-publish (conversation)
  "Best-effort publish CONVERSATION's compact picker cache after a durable append."
  (ignore-errors
    (multiple-value-bind (size write-date)
        (conversation--file-identity (conversation-pathname conversation))
      (conversation-picker-metadata-write
       (conversation-pathname conversation)
       (make-instance 'conversation-picker-metadata
                      :source-size size
                      :source-write-date write-date
                      :source-revision
                      (conversation-picker-revision-read
                       (conversation-pathname conversation))
                      :working-seconds (conversation-working-seconds conversation)
                      :user-turn-count (conversation-user-turn-count conversation)
                      :search-message-count
                      (conversation-picker-search-message-count conversation)
                      :preview (conversation-picker-preview conversation)
                      :incomplete-tail-p
                      (conversation-incomplete-tail-p conversation)))))
  nil)


;;;; -- Conversation Picker Search --

(-> conversation-picker-search-revision-read (pathname) (integer 0))
(defun conversation-picker-search-revision-read (conversation-pathname)
  "Return CONVERSATION-PATHNAME's durable message-search revision, or zero."
  (let ((revision-pathname
          (conversation-picker-search-revision-pathname conversation-pathname)))
    (if (probe-file revision-pathname)
        (handler-case
            (multiple-value-bind (record complete-p)
                (snapshot-read revision-pathname)
              (let ((revision
                      (and complete-p
                           (listp record)
                           (eq (first record)
                               :conversation-picker-search-revision)
                           (= (or (getf (rest record) :version) 0) 1)
                           (getf (rest record) :value))))
                (if (typep revision '(integer 0))
                    revision
                    0)))
          (error ()
            0))
        0)))


(-> conversation-picker-search-revision-write
    (pathname (integer 0))
    (integer 0))
(defun conversation-picker-search-revision-write
    (conversation-pathname revision)
  "Atomically record REVISION before searchable message text changes."
  (let ((revision-pathname
          (conversation-picker-search-revision-pathname conversation-pathname)))
    (ensure-directories-exist revision-pathname)
    (snapshot-write revision-pathname
                    (list :conversation-picker-search-revision
                          :version 1
                          :value revision))
    revision))


(-> conversation-picker-search-invalidate (conversation) (integer 0))
(defun conversation-picker-search-invalidate (conversation)
  "Advance CONVERSATION's message-search revision before its log changes."
  (let ((pathname (conversation-pathname conversation)))
    (handler-case
        (conversation-picker-search-revision-write
         pathname
         (1+ (conversation-picker-search-revision-read pathname)))
      (error (condition)
        (error 'conversation-invariant-error
               :message
               (format nil
                       "Could not invalidate conversation picker search: ~A"
                       condition)
               :pathname pathname
               :sequence (conversation-next-sequence conversation))))))


(-> conversation-picker-search--messages-p (t) boolean)
(defun conversation-picker-search--messages-p (value)
  "Return true when VALUE is a finite proper list of strings."
  (handler-case
      (not
       (null
        (and (listp value)
             (or (null value) (list-length value))
             (every #'stringp value))))
    (type-error ()
      nil)))


(-> conversation-picker-search-index-record
    (conversation-picker-search-index)
    list)
(defun conversation-picker-search-index-record (index)
  "Return INDEX as one portable atomically published search form."
  (list :conversation-picker-search
        :version 1
        :source-revision
        (conversation-picker-search-index-source-revision index)
        :message-count
        (conversation-picker-search-index-message-count index)
        :messages
        (conversation-picker-search-index-messages index)))


(-> conversation-picker-search-index-from-record
    (t)
    (option conversation-picker-search-index))
(defun conversation-picker-search-index-from-record (record)
  "Return the validated picker search INDEX represented by RECORD, or NIL."
  (handler-case
      (when (and (listp record)
                 (eq (first record) :conversation-picker-search)
                 (= (or (getf (rest record) :version) 0) 1))
        (let ((source-revision (getf (rest record) :source-revision))
              (message-count (getf (rest record) :message-count))
              (messages (getf (rest record) :messages)))
          (when (and (typep source-revision '(integer 0))
                     (typep message-count '(integer 0))
                     (conversation-picker-search--messages-p messages)
                     (= message-count (length messages)))
            (make-instance 'conversation-picker-search-index
                           :source-revision source-revision
                           :message-count message-count
                           :messages messages))))
    (error ()
      nil)))


(-> conversation-picker-search-read
    (pathname)
    (option conversation-picker-search-index))
(defun conversation-picker-search-read (pathname)
  "Return PATHNAME's valid current-log message-search sidecar, or NIL."
  (let ((metadata (conversation-picker-metadata-read pathname)))
    (when metadata
      (let ((search-pathname (conversation-picker-search-pathname pathname)))
        (when (probe-file search-pathname)
          (handler-case
              (multiple-value-bind (record complete-p)
                  (snapshot-read search-pathname)
                (let ((index
                        (and complete-p
                             (conversation-picker-search-index-from-record record))))
                  (when (and index
                             (= (conversation-picker-search-revision-read pathname)
                                (conversation-picker-search-index-source-revision
                                 index))
                             (= (conversation-picker-search-index-message-count index)
                                (conversation-picker-metadata-search-message-count
                                 metadata)))
                    index)))
            (error ()
              nil)))))))


(-> conversation-picker-search-write
    (pathname conversation-picker-search-index)
    conversation-picker-search-index)
(defun conversation-picker-search-write (pathname index)
  "Atomically publish INDEX as PATHNAME's validated message-search sidecar."
  (let ((search-pathname (conversation-picker-search-pathname pathname)))
    (ensure-directories-exist search-pathname)
    (snapshot-write search-pathname
                    (conversation-picker-search-index-record index))
    index))


(-> conversation-picker-search-publish (conversation) null)
(defun conversation-picker-search-publish (conversation)
  "Best-effort publish CONVERSATION's durable user/assistant search corpus."
  (ignore-errors
    (let ((pathname (conversation-pathname conversation)))
      (conversation-picker-search-write
       pathname
       (make-instance
        'conversation-picker-search-index
        :source-revision (conversation-picker-search-revision-read pathname)
        :message-count (conversation-picker-search-message-count conversation)
        :messages (copy-list
                   (conversation-picker-search-messages conversation))))))
  nil)

(-> conversation-create
    (configuration &key (:identifier (option string))
                        (:prompt-cache-key (option string))
                        (:storage-root (option pathname))
                        (:created-at (option timestamp)))
    conversation)
(defun conversation-create
    (configuration &key identifier prompt-cache-key storage-root created-at)
  "Create an in-memory conversation that persists under optional STORAGE-ROOT."
  (let* ((created-at (or created-at (get-universal-time)))
         (root (uiop:ensure-directory-pathname
                (or storage-root
                    (configuration-conversation-root configuration))))
         (conversation-id
           (or identifier
               (conversation-identifier-generate root :timestamp created-at)))
         (origin-directory (namestring
                            (configuration-working-directory configuration)))
         (pathname (merge-pathnames
                    (make-pathname :name conversation-id :type "sexp")
                    root)))
    (when (probe-file pathname)
      (error 'conversation-error
             :message (format nil "Conversation ~A already exists." conversation-id)
             :pathname pathname
             :sequence nil))
    (make-instance 'conversation
                   :identifier conversation-id
                   :prompt-cache-key prompt-cache-key
                   :pathname pathname
                   :persisted-p nil
                   :incomplete-tail-p nil
                   :created-at created-at
                   :origin-directory origin-directory
                   :model (configuration-model configuration)
                   :reasoning-effort
                   (configuration-reasoning-effort configuration)
                   :next-sequence 1
                   :input-items nil)))

(-> conversation-append-record (conversation list) list)
(defgeneric conversation-append-record (conversation record)
  (:documentation "Append portable RECORD to CONVERSATION and return the sequenced form."))

(defmethod conversation-append-record ((conversation conversation) (record list))
  "Assign metadata, initialize persistence if needed, and append RECORD."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (unless (keywordp (first record))
      (error 'conversation-invariant-error
             :message "A conversation record must begin with a keyword."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation)))
    (let* ((sequence (conversation-next-sequence conversation))
           (repair-tail-p (conversation-incomplete-tail-p conversation))
           (sequenced (list* (first record)
                             :seq sequence
                             :time (get-universal-time)
                             (rest record)))
           (picker-search-message (conversation--record-preview sequenced)))
      ;; Advance durable sidecar revisions before the log changes so a concurrent
      ;; scan cannot stamp pre-append state with a post-append revision.
      (when picker-search-message
        (conversation-picker-search-invalidate conversation))
      (conversation-picker-metadata-invalidate conversation)
      (handler-case
          (if (conversation-persisted-p conversation)
              (progn
                (unless (probe-file (conversation-pathname conversation))
                  (error 'conversation-invariant-error
                         :message "The persisted conversation file is missing."
                         :pathname (conversation-pathname conversation)
                         :sequence sequence))
                (log-append
                 (conversation-pathname conversation)
                 sequenced
                 :repair-tail-p repair-tail-p)
                (setf (conversation-incomplete-tail-p conversation) nil)
                (when repair-tail-p
                  (incf (conversation-log-generation conversation))))
              (conversation--write-initial-record conversation sequenced))
        (error (condition)
          (error 'conversation-invariant-error
                 :message
                 (format nil
                         "Could not append conversation record: ~A"
                         condition)
                 :pathname (conversation-pathname conversation)
                 :sequence sequence)))
      (incf (conversation-next-sequence conversation))
      (conversation--note-activity conversation sequenced)
      (when picker-search-message
        (conversation--note-picker-search-message
         conversation picker-search-message))
      (when (eq (first sequenced) :goal)
        (setf (conversation-latest-goal-record conversation) sequenced))
      (conversation-picker-metadata-publish conversation)
      (when picker-search-message
        (conversation-picker-search-publish conversation))
      sequenced)))

(-> conversation--append-input-item (conversation json-object) json-object)
(defun conversation--append-input-item (conversation item)
  "Append provider ITEM to CONVERSATION's in-memory chronological projection.

The model active when ITEM is projected identifies the family that
produced it, which lets a later request omit provider-private content
another family cannot read. Replay establishes the same association
because durable configuration records are projected in order."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let ((cell (list item))
          (tail (conversation-input-items-tail conversation))
          (model (conversation-model conversation)))
      (when (non-empty-string-p model)
        (setf (gethash item (conversation-input-item-families conversation))
              (model-family model)))
      (if tail
          (setf (rest tail) cell)
          (setf (conversation-input-items conversation) cell))
      (setf (conversation-input-items-tail conversation) cell)))
  item)

(-> conversation--append-ephemeral-input-item
    (conversation json-object &key (:attachments list))
    json-object)
(defun conversation--append-ephemeral-input-item
    (conversation item &key attachments)
  "Append request-local ITEM and record any owned ATTACHMENTS for cleanup."
  (let ((entries
          (append
           (conversation-ephemeral-input-entries conversation)
           (list (list :item item :attachments attachments)))))
    ;; Publish ownership before mutating the provider projection. An interrupt
    ;; after the projection append can then never leave an untagged item.
    (setf (conversation-ephemeral-input-entries conversation) entries)
    (conversation--append-input-item conversation item))
  item)

(-> conversation-input-items-for-request
    (conversation &key (:include-ephemeral-p boolean))
    list)
(defun conversation-input-items-for-request
    (conversation &key (include-ephemeral-p t))
  "Return a fresh provider projection, optionally excluding request-local items."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (if include-ephemeral-p
        (copy-list (conversation-input-items conversation))
        (let ((ephemeral-items (make-hash-table :test #'eq)))
          (dolist (entry (conversation-ephemeral-input-entries conversation))
            (setf (gethash (getf entry :item) ephemeral-items) t))
          (remove-if
           (lambda (item)
             (gethash item ephemeral-items))
           (conversation-input-items conversation))))))

(-> reasoning-item-p (t) boolean)
(defun reasoning-item-p (item)
  "Return true when ITEM is a Responses reasoning item."
  (and (json-object-p item)
       (string= (or (json-get item "type") "") "reasoning")
       t))

(-> native-compaction-item-p (t) boolean)
(defun native-compaction-item-p (item)
  "Return true when ITEM carries an opaque Responses compaction checkpoint."
  (and (json-object-p item)
       (let ((type (json-get item "type")))
         (and (stringp type)
              (not (null
                    (member type
                            '("compaction"
                              "compaction_summary"
                              "context_compaction")
                            :test #'string=)))
              (non-empty-string-p (json-get item "encrypted_content"))))
       t))

(-> native-compaction-item-canonicalize (t) t)
(defun native-compaction-item-canonicalize (item)
  "Canonicalize ITEM's legacy opaque compaction type when it is a JSON object."
  (when (json-object-p item)
    (let ((type (json-get item "type")))
      (when (and (stringp type)
                 (string= type "compaction_summary"))
        (setf (gethash "type" item) "compaction"))))
  item)

(-> conversation-family-private-item-p (t) boolean)
(defun conversation-family-private-item-p (item)
  "Return true when ITEM can only be read by its producing model family."
  (or (reasoning-item-p item)
      (native-compaction-item-p item)))

(-> conversation-input-item-family (conversation json-object) (option keyword))
(defun conversation-input-item-family (conversation item)
  "Return the model family that produced ITEM, or NIL when it is unknown."
  (values (gethash item (conversation-input-item-families conversation))))

(-> conversation-input-items-for-family
    (conversation keyword &key (:include-ephemeral-p boolean))
    list)
(defun conversation-input-items-for-family
    (conversation family &key (include-ephemeral-p t))
  "Return CONVERSATION's provider projection usable by FAMILY.

A private reasoning or native compaction item carries encrypted content that
only the family which produced it can decrypt, so such items from another
family, or from an unrecorded one, are omitted rather than replayed into a
rejected request. Every other item stays, including a portable compaction
summary, keeping the conversation usable across families."
  (remove-if
   (lambda (item)
     (and (conversation-family-private-item-p item)
          (not (eq (conversation-input-item-family conversation item)
                   family))))
   (conversation-input-items-for-request
    conversation
    :include-ephemeral-p include-ephemeral-p)))

(defparameter *conversation-inherited-reference-boundary*
    (concatenate
     'string
     "Everything before this message is inherited reference context from the "
     "parent task. It is not your current assignment. Use it only as background, "
     "then follow the child role instructions and the user assignment that follows.")
  "The developer boundary separating inherited parent history from child work.")

(-> conversation--inherited-reference-text-part (t) (option json-object))
(defun conversation--inherited-reference-text-part (part)
  "Return a detached portable text PART, or NIL for non-text content."
  (when (json-object-p part)
    (let ((type (json-get part "type"))
          (text (json-get part "text")))
      (when (and (stringp type)
                 (member type '("input_text" "output_text" "text")
                         :test #'string=)
                 (stringp text))
        (json-object "type" type "text" text)))))

(-> conversation--inherited-reference-message (t) (option json-object))
(defun conversation--inherited-reference-message (item)
  "Return ITEM's safe user or final-assistant reference message, or NIL."
  (when (and (json-object-p item)
             (string= (or (json-get item "type") "") "message"))
    (let ((role (json-get item "role"))
          (phase (json-get item "phase"))
          (content (json-get item "content")))
      (when (and (stringp role)
                 (member role '("user" "assistant") :test #'string=)
                 (or (not (string= role "assistant"))
                     (null phase)
                     (and (stringp phase) (string= phase "final_answer")))
                 (vectorp content))
        (let ((parts
                (loop for part across content
                      for copy = (conversation--inherited-reference-text-part part)
                      when copy collect copy)))
          (when parts
            (json-object "type" "message"
                         "role" role
                         "content" (coerce parts 'vector))))))))

(-> conversation--inherited-reference-boundary-item () json-object)
(defun conversation--inherited-reference-boundary-item ()
  "Return the durable developer boundary following inherited parent history."
  (json-object
   "type" "message"
   "role" "developer"
   "content"
   (json-array
    (json-object "type" "input_text"
                 "text" *conversation-inherited-reference-boundary*))))

(-> conversation--inherited-reference-boundary-p (t) boolean)
(defun conversation--inherited-reference-boundary-p (item)
  "Return true when ITEM is the exact inherited-reference developer boundary."
  (and (json-object-p item)
       (string= (or (json-get item "type") "") "message")
       (string= (or (json-get item "role") "") "developer")
       (let ((content (json-get item "content")))
         (and (vectorp content)
              (= (length content) 1)
              (let ((part (aref content 0)))
                (and (json-object-p part)
                     (string= (or (json-get part "type") "") "input_text")
                     (string= (or (json-get part "text") "")
                              *conversation-inherited-reference-boundary*)))))
       t))

(-> conversation--inherited-reference-wire-byte-length (list) (integer 0))
(defun conversation--inherited-reference-wire-byte-length (messages)
  "Return the UTF-8 wire bytes for MESSAGES and their developer boundary."
  (length
   (sb-ext:string-to-octets
    (json-encode
     (coerce
      (append messages
              (list (conversation--inherited-reference-boundary-item)))
      'vector))
    :external-format ':utf-8)))

(-> conversation-inherited-reference-snapshot
    (conversation (integer 1))
    list)
(defun conversation-inherited-reference-snapshot
    (conversation maximum-wire-bytes)
  "Return a bounded detached snapshot of CONVERSATION's reference messages.

The snapshot excludes request-local items, reasoning, tool traffic, images,
intermediate assistant phases, and developer policy. Compaction bridge messages
remain as ordinary portable user text. Whole newest messages are retained in
wire order while the snapshot plus its developer boundary fits within
MAXIMUM-WIRE-BYTES. Only candidate messages within the retained suffix are
copied."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let ((items (coerce (conversation-input-items conversation) 'vector))
          (ephemeral-items (make-hash-table :test #'eq))
          (selected nil)
          (wire-bytes
            (conversation--inherited-reference-wire-byte-length nil)))
      (dolist (entry (conversation-ephemeral-input-entries conversation))
        (setf (gethash (getf entry :item) ephemeral-items) t))
      (loop for index downfrom (1- (length items)) to 0
            for item = (aref items index)
            unless (gethash item ephemeral-items)
              do (let ((message
                         (conversation--inherited-reference-message item)))
                   (when message
                     (let ((candidate-bytes
                             (+ wire-bytes
                                1
                                (length
                                 (sb-ext:string-to-octets
                                  (json-encode message)
                                  :external-format ':utf-8)))))
                       (if (<= candidate-bytes maximum-wire-bytes)
                           (setf wire-bytes candidate-bytes
                                 selected (cons message selected))
                           (return))))))
      selected)))

(-> conversation-append-inherited-reference
    (conversation non-empty-string list)
    list)
(defun conversation-append-inherited-reference
    (conversation source-conversation-identifier items)
  "Persist and project spawn-time parent reference ITEMS into CONVERSATION."
  (let* ((messages
           (loop for item in items
                 for message = (conversation--inherited-reference-message item)
                 when message collect message))
         (projection
           (append messages
                   (list (conversation--inherited-reference-boundary-item))))
         (record
           (conversation-append-record
            conversation
            (list :inherited-reference
                  :source-conversation-id source-conversation-identifier
                  :wire-json (json-encode (coerce projection 'vector))))))
    (dolist (item projection)
      (conversation--append-input-item conversation item))
    record))

(-> conversation-clear-ephemeral-input-items (conversation) null)
(defun conversation-clear-ephemeral-input-items (conversation)
  "Remove all request-local provider items and their owned image artifacts."
  (let ((entries (conversation-ephemeral-input-entries conversation)))
    (when entries
      (let ((ephemeral-items (make-hash-table :test #'eq)))
        (dolist (entry entries)
          (setf (gethash (getf entry :item) ephemeral-items) t))
        (setf (conversation-input-items conversation)
              (remove-if
               (lambda (item)
                 (gethash item ephemeral-items))
               (conversation-input-items conversation))
              (conversation-ephemeral-input-entries conversation) nil))
      (dolist (entry entries)
        (dolist (attachment (getf entry :attachments))
          (ignore-errors
            (when (probe-file (image-attachment-pathname attachment))
              (delete-file (image-attachment-pathname attachment))))))))
  nil)

(-> conversation-image-artifact-root (conversation) pathname)
(defun conversation-image-artifact-root (conversation)
  "Return CONVERSATION's private binary image artifact directory."
  (let* ((conversation-root
           (uiop:pathname-directory-pathname
            (conversation-pathname conversation)))
         (data-root (uiop:pathname-parent-directory-pathname conversation-root)))
    (merge-pathnames
     (format nil "conversation-images/~A/"
             (conversation-identifier conversation))
     data-root)))

(-> user-message-item (string &optional list) json-object)
(defun user-message-item (content &optional attachments)
  "Return a Responses API user message containing CONTENT and ATTACHMENTS."
  (json-object
   "type" "message"
   "role" "user"
   "content"
   (coerce
    (append
     (loop for attachment in attachments
           for label-number from 1
           append (image-input-content-items attachment label-number))
     (when (non-empty-string-p content)
       (list (json-object
              "type" "input_text"
              "text" content))))
    'vector)))

(-> conversation--prepare-images (conversation list) list)
(defun conversation--prepare-images (conversation image-pathnames)
  "Prepare IMAGE-PATHNAMES transactionally for CONVERSATION."
  (let ((attachments nil))
    (handler-case
        (progn
          (dolist (pathname image-pathnames)
            (push (image-input-prepare
                   pathname
                   (conversation-image-artifact-root conversation))
                  attachments))
          (nreverse attachments))
      (error (condition)
        (dolist (attachment attachments)
          (when (probe-file (image-attachment-pathname attachment))
            (delete-file (image-attachment-pathname attachment))))
        (error condition)))))

(-> conversation--delete-image-attachments (list) null)
(defun conversation--delete-image-attachments (attachments)
  "Delete newly prepared ATTACHMENTS after a failed durable append."
  (dolist (attachment attachments)
    (when (probe-file (image-attachment-pathname attachment))
      (delete-file (image-attachment-pathname attachment))))
  nil)

(-> conversation-append-user-message
    (conversation (or string user-message-input))
    (values json-object list))
(defun conversation-append-user-message (conversation input)
  "Persist user INPUT and return its provider item and sequenced record."
  (let* ((submission
           (etypecase input
             (string (user-message-input-create :text input))
             (user-message-input input)))
         (content (user-message-input-text submission))
         (attachments
           (conversation--prepare-images
            conversation
            (user-message-input-image-pathnames submission)))
         (item (user-message-item content attachments))
         (record nil)
         (durable-p nil))
    (unwind-protect
         (progn
           (setf record
                 (conversation-append-record
                  conversation
                  (append
                   (list :message
                         :role :user
                         :content content)
                   (when attachments
                     (list :images
                           (mapcar #'image-attachment-record attachments)))
                   (unless attachments
                     (list :wire-json (json-encode item))))))
           (setf durable-p t
                 (conversation-turn-state conversation) nil)
           (values (conversation--append-input-item conversation item)
                   record))
      (unless durable-p
        (conversation--delete-image-attachments attachments)))))

(-> conversation-append-provider-item
    (conversation json-object
     &key (:persistence tool-conversation-persistence))
    json-object)
(defun conversation-append-provider-item
    (conversation item &key (persistence ':durable))
  "Append one authoritative provider ITEM with the requested PERSISTENCE."
  (ecase persistence
    (:durable
     (conversation-append-record
      conversation
      (list :provider-item
            :wire-json (json-encode item)))
     (conversation--append-input-item conversation item))
    (:next-response
     (conversation--append-ephemeral-input-item conversation item))))

(-> function-call-output-item (string (or string vector)) json-object)
(defun function-call-output-item (call-id output)
  "Return a Responses API function-call output correlated by CALL-ID."
  (json-object
   "type" "function_call_output"
   "call_id" call-id
   "output" output))

(-> conversation--tool-content-output (list) vector)
(defun conversation--tool-content-output (blocks)
  "Return ordered string and image BLOCKS as native tool-output content."
  (coerce
   (mapcar
    (lambda (block)
      (etypecase block
        (string
         (json-object "type" "input_text" "text" block))
        (image-attachment
         (image-input-content-item block))))
    blocks)
   'vector))

(-> conversation--tool-content-block-record (t) list)
(defun conversation--tool-content-block-record (block)
  "Return one portable durable descriptor for provider content BLOCK."
  (etypecase block
    (string
     (list :text block))
    (image-attachment
     (list :image (image-attachment-record block)))))

(-> conversation--tool-content-images (list) list)
(defun conversation--tool-content-images (blocks)
  "Return every image attachment in ordered provider BLOCKS."
  (remove-if-not
   (lambda (block)
     (typep block 'image-attachment))
   blocks))

(defparameter *conversation-interrupted-tool-output*
  "Autolith interrupted this tool call before recording its result. The call may have changed external state. Inspect the relevant state before deciding whether to retry it."
  "The provider-visible result synthesized for a tool call with an unknown outcome.")

(-> conversation-append-tool-result
    (conversation string
     &key (:tool-name string)
          (:output string)
          (:image-attachments list)
          (:content-blocks list)
          (:success-p boolean)
          (:cpu-microseconds (option (integer 0)))
          (:real-microseconds (option (integer 0)))
          (:persistence tool-conversation-persistence))
    json-object)
(defun conversation-append-tool-result
    (conversation call-id
     &key tool-name output image-attachments content-blocks success-p
       cpu-microseconds real-microseconds (persistence ':durable))
  "Append one tool OUTPUT, optional ordered content, timing, and PERSISTENCE."
  (when (and image-attachments content-blocks)
    (error 'conversation-invariant-error
           :message
           "Tool output cannot provide both image attachments and content blocks."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (let* ((blocks
           (or content-blocks
               (when image-attachments
                 (append
                  (when (non-empty-string-p output)
                    (list output))
                  image-attachments))))
         (attachments (conversation--tool-content-images blocks))
         (retained-p nil))
    (unwind-protect
         (progn
           (unless (or (and (null cpu-microseconds)
                            (null real-microseconds))
                       (and (typep cpu-microseconds '(integer 0))
                            (typep real-microseconds '(integer 0))))
             (error 'conversation-invariant-error
                    :message
                    "Tool timing must contain both nonnegative microsecond values."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (unless (every
                    (lambda (block)
                      (or (stringp block)
                          (typep block 'image-attachment)))
                    blocks)
             (error 'conversation-invariant-error
                    :message "Tool output contains an invalid content block."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (when (and attachments (not success-p))
             (error 'conversation-invariant-error
                    :message "A failed tool result cannot contain image output."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (let* ((wire-output
                    (if attachments
                        (conversation--tool-content-output blocks)
                        output))
                  (item (function-call-output-item call-id wire-output)))
             (ecase persistence
               (:durable
                (conversation-append-record
                 conversation
                 (append
                  (list :tool-result
                        :call-id call-id
                        :tool tool-name
                        :status (if success-p :ok :error)
                        :output output)
                  (when attachments
                    (list
                     :content-blocks
                     (mapcar #'conversation--tool-content-block-record blocks)))
                  (when cpu-microseconds
                    (list :cpu-microseconds cpu-microseconds
                          :real-microseconds real-microseconds))
                  (unless attachments
                    (list :wire-json (json-encode item)))))
                (setf retained-p t)
                (conversation--append-input-item conversation item))
               (:next-response
                (conversation--append-ephemeral-input-item
                 conversation
                 item
                 :attachments attachments)
                (setf retained-p t)))
             item))
      (unless retained-p
        (conversation--delete-image-attachments attachments)))))

(-> conversation--wire-item-type-p (json-object string) boolean)
(defun conversation--wire-item-type-p (item type)
  "Return true when provider ITEM has wire TYPE."
  (string= (or (json-get item "type") "") type))

(-> conversation--tool-call-id (conversation json-object) string)
(defun conversation--tool-call-id (conversation item)
  "Return ITEM's non-empty tool call identifier or signal corrupted history."
  (let ((call-id (json-get item "call_id")))
    (unless (non-empty-string-p call-id)
      (error 'conversation-invariant-error
             :message "A persisted tool item has no call identifier."
             :pathname (conversation-pathname conversation)
             :sequence nil))
    call-id))

(-> conversation--tool-call-name (json-object) string)
(defun conversation--tool-call-name (item)
  "Return a readable canonical name for function call ITEM."
  (let ((namespace (json-get item "namespace"))
        (name (json-get item "name")))
    (cond
      ((and (non-empty-string-p namespace) (non-empty-string-p name))
       (format nil "~A.~A" namespace name))
      ((non-empty-string-p name)
       name)
      ((non-empty-string-p namespace)
       namespace)
      (t
       "unknown"))))

(-> conversation--tool-item-tables
    (conversation list)
    (values hash-table hash-table))
(defun conversation--tool-item-tables (conversation items)
  "Return unique function calls and the first correlated outputs in ITEMS.

A late writer can append a real tool result after crash recovery has already
recorded an unknown-outcome result and continued the conversation. Tolerate
only that recognizable ordering and preserve the repair because subsequent
history was produced from its projection.  The late result remains in the
append-only log but must not enter provider replay."
  (let ((calls (make-hash-table :test #'equal))
        (outputs (make-hash-table :test #'equal))
        (outputs-after-call-p (make-hash-table :test #'equal))
        (stale-output-tolerated-p (make-hash-table :test #'equal)))
    (dolist (item items)
      (when (json-object-p item)
        (cond
          ((conversation--wire-item-type-p item "function_call")
           (let ((call-id (conversation--tool-call-id conversation item)))
             (when (gethash call-id calls)
               (error 'conversation-invariant-error
                      :message
                      (format nil "Persisted history repeats tool call ~S."
                              call-id)
                      :pathname (conversation-pathname conversation)
                      :sequence nil))
             (setf (gethash call-id calls) item)))
          ((conversation--wire-item-type-p item "function_call_output")
           (let ((call-id (conversation--tool-call-id conversation item)))
             (multiple-value-bind (existing present-p)
                 (gethash call-id outputs)
               (if (not present-p)
                   (setf (gethash call-id outputs) item
                         (gethash call-id outputs-after-call-p)
                         (not (null (gethash call-id calls))))
                   (let ((existing-output (json-get existing "output")))
                     (if (and
                          (gethash call-id outputs-after-call-p)
                          (not
                           (gethash call-id stale-output-tolerated-p))
                          (stringp existing-output)
                          (string=
                           existing-output
                           *conversation-interrupted-tool-output*))
                         (setf
                          (gethash call-id stale-output-tolerated-p)
                          t)
                         (error
                          'conversation-invariant-error
                          :message
                          (format
                           nil
                           "Persisted history repeats output for tool call ~S."
                           call-id)
                          :pathname (conversation-pathname conversation)
                          :sequence nil))))))))))
    (values calls outputs)))

(-> conversation--repair-incomplete-tool-calls (conversation) null)
(defun conversation--repair-incomplete-tool-calls (conversation)
  "Pair every persisted function call with an output after an interrupted exit.

Existing outputs are moved beside their calls in the provider projection. A
missing output is recorded append-only as an explicit unknown-outcome failure
before the repaired projection can be sent to the provider."
  (let ((items (copy-list (conversation-input-items conversation))))
    (multiple-value-bind (calls outputs)
        (conversation--tool-item-tables conversation items)
      (let ((remaining items)
            (repaired nil))
        (loop while remaining
              for item = (pop remaining)
              do (cond
                   ((and (json-object-p item)
                         (conversation--wire-item-type-p
                          item "function_call_output"))
                    (let ((call-id
                            (conversation--tool-call-id conversation item)))
                      ;; Correlated outputs are emitted with their call group.
                      ;; Orphaned legacy outputs retain their original position.
                      (unless (gethash call-id calls)
                        (push item repaired))))
                   ((and (json-object-p item)
                         (conversation--wire-item-type-p item "function_call"))
                    (let ((group (list item)))
                      (loop while (and remaining
                                       (json-object-p (first remaining))
                                       (conversation--wire-item-type-p
                                        (first remaining) "function_call"))
                            do (setf group
                                     (nconc group (list (pop remaining)))))
                      (dolist (call group)
                        (push call repaired))
                      (dolist (call group)
                        (let* ((call-id
                                 (conversation--tool-call-id conversation call))
                               (output (gethash call-id outputs)))
                          (unless output
                            (setf output
                                  (conversation-append-tool-result
                                   conversation
                                   call-id
                                   :tool-name
                                   (conversation--tool-call-name call)
                                   :output *conversation-interrupted-tool-output*
                                   :success-p nil)
                                  (gethash call-id outputs) output))
                          (push output repaired)))))
                   (t
                    (push item repaired))))
        (setf (conversation-input-items conversation) (nreverse repaired)))))
  nil)

(-> conversation--usage-total (t) (option integer))
(defun conversation--usage-total (usage)
  "Return the total token count carried by portable or wire USAGE data."
  (cond
    ((json-object-p usage)
     (let ((total (json-get usage "total_tokens")))
       (and (integerp total) total)))
    ((listp usage)
     (let ((total (second (assoc "total_tokens" usage :test #'equal))))
       (and (integerp total) total)))
    (t
     nil)))

(-> conversation-append-provider-metadata (conversation list) list)
(defun conversation-append-provider-metadata (conversation metadata)
  "Persist portable provider METADATA that is not part of request history."
  (let ((total (conversation--usage-total (getf metadata :usage))))
    (when total
      (setf (conversation-last-total-tokens conversation) total)))
  (conversation-append-record
   conversation
   (list :provider :metadata metadata)))

(-> conversation--model-selection-p (t t) boolean)
(defun conversation--model-selection-p (model reasoning-effort)
  "Return true when MODEL and REASONING-EFFORT form a restorable selection."
  (and (non-empty-string-p model)
       (non-empty-string-p reasoning-effort)
       (not
        (null
         (member reasoning-effort
                 *supported-reasoning-efforts*
                 :test #'string=)))))

(-> conversation-set-model-selection (conversation string string) null)
(defun conversation-set-model-selection (conversation model reasoning-effort)
  "Remember MODEL and REASONING-EFFORT without persisting an empty conversation."
  (unless (conversation--model-selection-p model reasoning-effort)
    (error 'conversation-invariant-error
           :message "A conversation model selection is invalid."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (unless (and (string= model (or (conversation-model conversation) ""))
               (string= reasoning-effort
                        (or (conversation-reasoning-effort conversation) "")))
    (when (conversation-persisted-p conversation)
      (conversation-append-record
       conversation
       (list :configuration
             :model model
             :reasoning-effort reasoning-effort)))
    (setf (conversation-model conversation) model
          (conversation-reasoning-effort conversation) reasoning-effort))
  nil)

(defparameter *conversation-summary-prefix*
  "A previous segment of this conversation was compacted. The summary below replaces that segment; use it to continue seamlessly without repeating completed work."
  "The bridge text introducing a compaction summary to the model.")

(-> conversation-summary-item (string) json-object)
(defun conversation-summary-item (content)
  "Return the replayable wire item carrying a compaction summary CONTENT."
  (json-object
   "type" "message"
   "role" "user"
   "content" (json-array
              (json-object
               "type" "input_text"
               "text" (format nil "~A~2%~A"
                              *conversation-summary-prefix*
                              content)))))

(-> conversation-append-summary (conversation string) list)
(defun conversation-append-summary (conversation content)
  "Persist a compaction summary and replace CONVERSATION's projection with it.

The durable record covers every record before it, so replay reproduces the
same compacted projection. The provider turn-state token is dropped because
it described the uncompacted context."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let* ((ephemeral-items
             (mapcar
              (lambda (entry)
                (getf entry :item))
              (conversation-ephemeral-input-entries conversation)))
           (record
             (conversation-append-record
              conversation
              (list :summary
                    :through-seq (1- (conversation-next-sequence conversation))
                    :content content))))
      (setf (conversation-input-items conversation)
            (cons (conversation-summary-item content) ephemeral-items)
            (conversation-turn-state conversation) nil
            (conversation-last-total-tokens conversation) 0)
      record)))

(-> conversation-append-native-compaction
    (conversation json-object &key (:family keyword) (:summary string))
    list)
(defun conversation-append-native-compaction
    (conversation item &key family summary)
  "Persist opaque native ITEM and portable SUMMARY as one compaction checkpoint.

ITEM retains private model context for FAMILY. SUMMARY is deliberately kept
alongside it so a later provider family can continue from a readable handoff.
Both replace every preceding durable provider item while pending request-local
items remain available for the next ordinary request."
  (native-compaction-item-canonicalize item)
  (unless (and (keywordp family)
               (native-compaction-item-p item)
               (non-empty-string-p summary))
    (error 'conversation-invariant-error
           :message "A native compaction checkpoint is invalid."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let* ((ephemeral-items
             (mapcar
              (lambda (entry)
                (getf entry :item))
              (conversation-ephemeral-input-entries conversation)))
           (summary-item (conversation-summary-item summary))
         (record
           (conversation-append-record
            conversation
            (list :native-compaction
                  :through-seq (1- (conversation-next-sequence conversation))
                  :family family
                  :wire-json (json-encode item)
                  :summary summary))))
    (setf (conversation-input-items conversation)
          (append (list item summary-item) ephemeral-items)
          (conversation-turn-state conversation) nil
          (conversation-last-total-tokens conversation) 0
          (gethash item (conversation-input-item-families conversation))
          family)
      record)))


;;;; -- Conversation Loading --

(-> conversation--map-records
    (pathname function &key (:start-position (integer 0)))
    (values integer boolean integer))
(defun conversation--map-records (pathname function &key (start-position 0))
  "Call FUNCTION for complete records in PATHNAME from START-POSITION.

Return the next readable file position, whether the final form is incomplete,
and the number of records visited. Storage failures become conversation
invariant errors while callback conditions propagate unchanged."
  (let ((callback-store-error nil))
    (handler-case
        (log-map
         (lambda (record)
           (handler-case
               (funcall function record)
             (store-error (condition)
               (setf callback-store-error condition)
               (error condition))))
         pathname
         :start-position start-position)
      (store-error (condition)
        (if (eq condition callback-store-error)
            (error condition)
            (error 'conversation-invariant-error
                   :message (format nil "Malformed conversation record: ~A"
                                    condition)
                   :pathname pathname
                   :sequence nil))))))

(-> conversation-picker-metadata-scan (pathname)
    (option conversation-picker-metadata))
(defun conversation-picker-metadata-scan (pathname)
  "Scan PATHNAME once to create exact metadata for its resume-picker cache."
  (let ((working-seconds 0)
        (user-turn-count 0)
        (search-message-count 0)
        (last-activity-at nil)
        (preview nil)
        (header-seen-p nil)
        (record-count 0))
    (handler-case
        (multiple-value-bind (initial-size initial-write-date)
            (conversation--file-identity pathname)
          (let ((initial-revision (conversation-picker-revision-read pathname)))
            (multiple-value-bind (position incomplete-tail-p count)
                (conversation--map-records
                 pathname
                 (lambda (record)
                   (if header-seen-p
                       (progn
                         (incf record-count)
                         (multiple-value-setq
                             (working-seconds user-turn-count last-activity-at)
                           (conversation--activity-after-record
                            record
                            :working-seconds working-seconds
                            :user-turn-count user-turn-count
                            :last-activity-at last-activity-at))
                         (let ((record-preview
                                 (conversation--record-preview record)))
                           (when record-preview
                             (incf search-message-count)
                             (setf preview record-preview))))
                       (setf header-seen-p t))))
              (declare (ignore position count))
              (when (and header-seen-p (plusp record-count))
                (multiple-value-bind (final-size final-write-date)
                    (conversation--file-identity pathname)
                  (when (and (= initial-size final-size)
                             (= initial-write-date final-write-date)
                             (= initial-revision
                                (conversation-picker-revision-read pathname)))
                    (make-instance 'conversation-picker-metadata
                                   :source-size initial-size
                                   :source-write-date initial-write-date
                                   :source-revision initial-revision
                                   :working-seconds working-seconds
                                   :user-turn-count user-turn-count
                                   :search-message-count search-message-count
                                   :preview preview
                                   :incomplete-tail-p incomplete-tail-p)))))))
      (error ()
        nil))))

(-> conversation-picker-metadata-find (pathname)
    (option conversation-picker-metadata))
(defun conversation-picker-metadata-find (pathname)
  "Return PATHNAME's validated picker cache, rebuilding a missing cache once."
  (or (conversation-picker-metadata-read pathname)
      (let ((metadata (conversation-picker-metadata-scan pathname)))
        (when metadata
          (ignore-errors
            (conversation-picker-metadata-write pathname metadata)))
        metadata)))


(-> conversation-picker-search-scan
    (pathname)
    (option conversation-picker-search-index))
(defun conversation-picker-search-scan (pathname)
  "Scan PATHNAME once for its complete durable user and assistant text."
  (let ((messages nil)
        (message-count 0)
        (header-seen-p nil)
        (record-count 0))
    (handler-case
        (multiple-value-bind (initial-size initial-write-date)
            (conversation--file-identity pathname)
          (let ((initial-search-revision
                  (conversation-picker-search-revision-read pathname)))
            (multiple-value-bind (position incomplete-tail-p count)
                (conversation--map-records
                 pathname
                 (lambda (record)
                   (if header-seen-p
                       (progn
                         (incf record-count)
                         (let ((message (conversation--record-preview record)))
                           (when message
                             (incf message-count)
                             (push message messages))))
                       (setf header-seen-p t))))
              (declare (ignore position incomplete-tail-p count))
              (when (and header-seen-p (plusp record-count))
                (multiple-value-bind (final-size final-write-date)
                    (conversation--file-identity pathname)
                  (when (and (= initial-size final-size)
                             (= initial-write-date final-write-date)
                             (= initial-search-revision
                                (conversation-picker-search-revision-read
                                 pathname)))
                    (make-instance
                     'conversation-picker-search-index
                     :source-revision initial-search-revision
                     :message-count message-count
                     :messages (nreverse messages))))))))
      (error ()
        nil))))


(-> conversation-picker-search-find
    (pathname)
    (option conversation-picker-search-index))
(defun conversation-picker-search-find (pathname)
  "Return PATHNAME's search index, rebuilding stale picker projections once."
  (or (conversation-picker-search-read pathname)
      (when (conversation-picker-metadata-find pathname)
        (or (conversation-picker-search-read pathname)
            (let ((index (conversation-picker-search-scan pathname)))
              (when index
                (ignore-errors
                  (conversation-picker-search-write pathname index))
                (conversation-picker-search-read pathname)))))))


(-> conversation-picker-search-index-text
    (conversation-picker-search-index)
    string)
(defun conversation-picker-search-index-text (index)
  "Return INDEX's chronological message corpus as one searchable string."
  (format nil
          "~{~A~^~%~}"
          (conversation-picker-search-index-messages index)))

(-> conversation--read-records (pathname) (values list boolean))
(defun conversation--read-records (pathname)
  "Read complete forms and report whether PATHNAME has an incomplete tail."
  (handler-case
      (log-read pathname)
    (error (condition)
      (error 'conversation-invariant-error
             :message (format nil "Malformed conversation record: ~A"
                              condition)
             :pathname pathname
             :sequence nil))))

(-> conversation--tool-content-block-from-record
    (conversation list (option integer))
    t)
(defun conversation--tool-content-block-from-record
    (conversation descriptor sequence)
  "Restore one durable tool content DESCRIPTOR for CONVERSATION."
  (cond
    ((and (listp descriptor)
          (stringp (getf descriptor :text))
          (null (getf descriptor :image)))
     (getf descriptor :text))
    ((and (listp descriptor)
          (getf descriptor :image)
          (null (getf descriptor :text)))
     (image-attachment-from-record
      (getf descriptor :image)
      (conversation-image-artifact-root conversation)))
    (t
     (error 'conversation-invariant-error
            :message "A persisted tool content block is invalid."
            :pathname (conversation-pathname conversation)
            :sequence sequence))))

(-> conversation--property-present-p (list keyword) boolean)
(defun conversation--property-present-p (properties indicator)
  "Return true when property list PROPERTIES contains INDICATOR."
  (loop for tail on properties by #'cddr
        thereis (eq (first tail) indicator)))

(-> conversation--apply-record (conversation list) null)
(defun conversation--apply-record (conversation record)
  "Project one persisted RECORD into CONVERSATION's in-memory state."
  (unless (and (listp record) (keywordp (first record)))
    (error 'conversation-invariant-error
           :message "A persisted conversation record is not a keyword list."
           :pathname (conversation-pathname conversation)
           :sequence nil))
  (let* ((properties (rest record))
         (sequence (getf properties :seq))
         (wire-json (getf properties :wire-json))
         (content-blocks-p
           (conversation--property-present-p properties :content-blocks))
         (images-p
           (conversation--property-present-p properties :images))
         (wire-json-p
           (conversation--property-present-p properties :wire-json))
         (picker-search-message (conversation--record-preview record)))
    (when (eq (first record) :tool-result)
      (when (> (count t (list content-blocks-p images-p wire-json-p)) 1)
        (error 'conversation-invariant-error
               :message
               "A persisted tool result contains multiple wire projections."
               :pathname (conversation-pathname conversation)
               :sequence sequence))
      (when (and (or content-blocks-p images-p)
                 (not (eq (getf properties :status) :ok)))
        (error 'conversation-invariant-error
               :message
               "A failed persisted tool result cannot contain image output."
               :pathname (conversation-pathname conversation)
               :sequence sequence)))
    (conversation--note-activity conversation record)
    (when picker-search-message
      (conversation--note-picker-search-message
       conversation picker-search-message))
    (when (eq (first record) :goal)
      (setf (conversation-latest-goal-record conversation) record))
    (when (integerp sequence)
      (setf (conversation-next-sequence conversation)
            (max (conversation-next-sequence conversation) (1+ sequence))))
    (when (and (eq (first record) :message)
               (getf (rest record) :images))
      (let* ((content (getf (rest record) :content))
             (attachments
               (mapcar
                (lambda (descriptor)
                  (image-attachment-from-record
                   descriptor
                   (conversation-image-artifact-root conversation)))
                (getf (rest record) :images))))
        (unless (stringp content)
          (error 'conversation-invariant-error
                 :message "A persisted image message has invalid text content."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (conversation--append-input-item
         conversation
         (user-message-item content attachments))))
    (when (and (eq (first record) :tool-result)
               content-blocks-p)
      (let ((call-id (getf (rest record) :call-id))
            (descriptors (getf (rest record) :content-blocks)))
        (unless (consp descriptors)
          (error 'conversation-invariant-error
                 :message
                 "A persisted multimodal tool result has no content blocks."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (unless (non-empty-string-p call-id)
          (error 'conversation-invariant-error
                 :message
                 "A persisted multimodal tool result has no call identifier."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (let ((blocks
                (mapcar
                 (lambda (descriptor)
                   (conversation--tool-content-block-from-record
                    conversation descriptor sequence))
                 descriptors)))
          (conversation--append-input-item
           conversation
           (function-call-output-item
            call-id
            (conversation--tool-content-output blocks))))))
    (when (and (eq (first record) :tool-result)
               images-p)
      (let ((call-id (getf (rest record) :call-id))
            (descriptors (getf (rest record) :images)))
        (unless (consp descriptors)
          (error 'conversation-invariant-error
                 :message "A persisted image tool result has no images."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (unless (non-empty-string-p call-id)
          (error 'conversation-invariant-error
                 :message "A persisted image tool result has no call identifier."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (let ((attachments
                (mapcar
                 (lambda (descriptor)
                   (image-attachment-from-record
                    descriptor
                    (conversation-image-artifact-root conversation)))
                 descriptors)))
          (conversation--append-input-item
           conversation
           (function-call-output-item
            call-id
            (conversation--tool-content-output
             (append
              (when (non-empty-string-p
                     (or (getf (rest record) :output) ""))
                (list (getf (rest record) :output)))
              attachments)))))))
    (when (eq (first record) :inherited-reference)
      (unless (and (non-empty-string-p
                    (getf properties :source-conversation-id))
                   (stringp wire-json))
        (error 'conversation-invariant-error
               :message "A persisted inherited reference has invalid metadata."
               :pathname (conversation-pathname conversation)
               :sequence sequence))
      (let ((items
              (handler-case
                  (json-decode wire-json)
                (error ()
                  (error 'conversation-invariant-error
                         :message
                         "A persisted inherited reference contains invalid JSON."
                         :pathname (conversation-pathname conversation)
                         :sequence sequence)))))
        (unless (and (vectorp items)
                     (plusp (length items))
                     (conversation--inherited-reference-boundary-p
                      (aref items (1- (length items)))))
          (error 'conversation-invariant-error
                 :message "A persisted inherited reference has an invalid boundary."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (let ((messages
                (loop for index below (1- (length items))
                      for message =
                        (conversation--inherited-reference-message
                         (aref items index))
                      when message collect message)))
          (unless (= (length messages) (1- (length items)))
            (error 'conversation-invariant-error
                   :message
                   "A persisted inherited reference contains a nonportable item."
                   :pathname (conversation-pathname conversation)
                   :sequence sequence))
          (dolist (item messages)
            (conversation--append-input-item conversation item))
          (conversation--append-input-item
           conversation
           (conversation--inherited-reference-boundary-item)))))
    (when (and (member (first record)
                       '(:message :provider-item :tool-result))
               (stringp wire-json)
               (not images-p)
               (not content-blocks-p))
      (let ((item (json-decode wire-json)))
        (unless (json-object-p item)
          (error 'conversation-invariant-error
                 :message "A persisted provider item is not a JSON object."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (conversation--append-input-item conversation item)))
    (when (eq (first record) :summary)
      (let ((content (getf (rest record) :content)))
        (when (stringp content)
          (setf (conversation-input-items conversation)
                (list (conversation-summary-item content))
                (conversation-last-total-tokens conversation) 0))))
    (when (eq (first record) :native-compaction)
      (let ((family (getf (rest record) :family))
            (wire-json (getf (rest record) :wire-json))
            (summary (getf (rest record) :summary)))
        (unless (and (keywordp family)
                     (stringp wire-json)
                     (non-empty-string-p summary))
          (error 'conversation-invariant-error
                 :message "A persisted native compaction checkpoint is invalid."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (let ((item
                (handler-case
                    (json-decode wire-json)
                  (error ()
                    (error 'conversation-invariant-error
                           :message
                           "A persisted native compaction checkpoint is not JSON."
                           :pathname (conversation-pathname conversation)
                           :sequence sequence)))))
          (native-compaction-item-canonicalize item)
          (unless (native-compaction-item-p item)
            (error 'conversation-invariant-error
                   :message
                   "A persisted native compaction item is unsupported."
                   :pathname (conversation-pathname conversation)
                   :sequence sequence))
          (setf (conversation-input-items conversation)
                (list item (conversation-summary-item summary))
                (conversation-turn-state conversation) nil
                (conversation-last-total-tokens conversation) 0
                (gethash item (conversation-input-item-families conversation))
                family))))
    (when (eq (first record) :provider)
      (let ((total (conversation--usage-total
                    (getf (getf (rest record) :metadata) :usage))))
        (when total
          (setf (conversation-last-total-tokens conversation) total))))
    (when (eq (first record) :configuration)
      (let ((model (getf (rest record) :model))
            (reasoning-effort (getf (rest record) :reasoning-effort)))
        (unless (conversation--model-selection-p model reasoning-effort)
          (error 'conversation-invariant-error
                 :message "A persisted conversation model selection is invalid."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (setf (conversation-model conversation) model
              (conversation-reasoning-effort conversation) reasoning-effort))))
  nil)

(-> conversation-peek-header (pathname) (option list))
(defun conversation-peek-header (pathname)
  "Return PATHNAME's leading conversation header form, or NIL when unreadable.

Only the first top-level form is read, so peeking stays cheap for large
conversation files."
  (handler-case
      (with-open-file (stream pathname :direction :input :external-format :utf-8)
        (let* ((*read-eval* nil)
               (end-marker (cons nil nil))
               (form (read stream nil end-marker)))
          (if (and (listp form)
                   (eq (first form) :conversation))
              form
              nil)))
    (error ()
      nil)))

(-> conversation--from-header (pathname list) conversation)
(defun conversation--from-header (pathname header)
  "Validate HEADER and return its empty in-memory conversation projection."
  (unless (and (listp header)
               (eq (first header) :conversation)
               (= (or (getf (rest header) :version) 0) 1)
               (non-empty-string-p (getf (rest header) :id)))
    (error 'conversation-invariant-error
           :message "The conversation header is missing or unsupported."
           :pathname pathname
           :sequence nil))
  (let* ((directory (getf (rest header) :directory))
         (model (getf (rest header) :model))
         (reasoning-effort (getf (rest header) :reasoning-effort)))
    (unless (or (and (null model) (null reasoning-effort))
                (conversation--model-selection-p model reasoning-effort))
      (error 'conversation-invariant-error
             :message "The conversation header has an invalid model selection."
             :pathname pathname
             :sequence nil))
    (make-instance 'conversation
                   :identifier (getf (rest header) :id)
                   :pathname pathname
                   :persisted-p t
                   :incomplete-tail-p nil
                   :created-at (getf (rest header) :created-at)
                   :origin-directory (and (stringp directory) directory)
                   :model (and (non-empty-string-p model) model)
                   :reasoning-effort
                   (and (non-empty-string-p reasoning-effort)
                        reasoning-effort)
                   :next-sequence 1
                   :input-items nil)))

(-> conversation-load (pathname) conversation)
(defun conversation-load (pathname)
  "Load a conversation from PATHNAME and rebuild its provider input projection."
  (let ((conversation nil))
    (multiple-value-bind (position incomplete-tail-p count)
        (conversation--map-records
         pathname
         (lambda (record)
           (if conversation
               (conversation--apply-record conversation record)
               (setf conversation
                     (conversation--from-header pathname record)))))
      (declare (ignore position count))
      (unless conversation
        (error 'conversation-invariant-error
               :message "The conversation header is missing or unsupported."
               :pathname pathname
               :sequence nil))
      (setf (conversation-incomplete-tail-p conversation)
            incomplete-tail-p)
      (conversation--repair-incomplete-tool-calls conversation)
      conversation)))

(-> conversation-pathname-for-id (configuration string) pathname)
(defun conversation-pathname-for-id (configuration identifier)
  "Return CONFIGURATION's conversation pathname for IDENTIFIER."
  (merge-pathnames (make-pathname
                    :name
                    (conversation-identifier-migration-resolve
                     configuration identifier)
                    :type "sexp")
                   (configuration-conversation-root configuration)))

(-> conversation-load-by-id (configuration string) conversation)
(defun conversation-load-by-id (configuration identifier)
  "Load IDENTIFIER from CONFIGURATION's conversation directory."
  (let ((pathname (conversation-pathname-for-id configuration identifier)))
    (unless (probe-file pathname)
      (error 'conversation-error
             :message (format nil "Conversation ~A does not exist." identifier)
             :pathname pathname
             :sequence nil))
    (conversation-load pathname)))

(-> conversation--pathname-non-empty-p (pathname) boolean)
(defun conversation--pathname-non-empty-p (pathname)
  "Return true when PATHNAME has a header and at least one complete record."
  (handler-case
      (let ((header-seen-p nil))
        (conversation--map-records
         pathname
         (lambda (record)
           (cond
             ((not header-seen-p)
              (unless (and (listp record)
                           (eq (first record) :conversation))
                (return-from conversation--pathname-non-empty-p nil))
              (setf header-seen-p t))
             (t
              (return-from conversation--pathname-non-empty-p t)))))
        nil)
    (error ()
      nil)))

(-> conversation-list (configuration) list)
(defun conversation-list (configuration)
  "Return non-empty conversation pathnames, newest first."
  (let ((root (configuration-conversation-root configuration)))
    (if (probe-file root)
        (sort
         (remove-if-not
          #'conversation--pathname-non-empty-p
          (uiop:directory-files root "*.sexp"))
         #'>
         :key (lambda (pathname)
                (or (file-write-date pathname) 0)))
        nil)))


(-> conversation-activity-summary (pathname)
    (values (integer 0) (integer 0)))
(defun conversation-activity-summary (pathname)
  "Return PATHNAME's cached working seconds and user-turn count.

A missing or stale cache is rebuilt once from durable records. Subsequent
resume-picker reads use the compact sidecar instead of replaying the log."
  (let ((metadata (conversation-picker-metadata-find pathname)))
    (if metadata
        (values (conversation-picker-metadata-working-seconds metadata)
                (conversation-picker-metadata-user-turn-count metadata))
        (values 0 0))))


(defparameter *conversation-delete-directory-tree-function*
  #'uiop:delete-directory-tree
  "Function used to remove private artifact trees after conversation deletion.")


(-> conversation-delete (configuration string) pathname)
(defun conversation-delete (configuration identifier)
  "Delete IDENTIFIER's conversation file and private artifacts.

Returns the removed conversation pathname. Signals CONVERSATION-ERROR when the
file is missing, IDENTIFIER is invalid, or another process owns the
conversation."
  (let* ((normalized
           (conversation-identifier-migration-resolve configuration identifier))
         (pathname (conversation-pathname-for-id configuration normalized))
         (sidecar-pathnames
           (conversation-picker-sidecar-pathnames pathname))
         (task-fragment
           (or (conversation-identifier-path-fragment normalized)
               (string-downcase normalized)))
         (artifact-roots
           (list
            (merge-pathnames
             (format nil "conversation-images/~A/" normalized)
             (configuration-data-root configuration))
            (merge-pathnames
             (format nil "tasks/~A/" task-fragment)
             (configuration-data-root configuration))))
         (lease nil))
    (unwind-protect
         (progn
           (setf lease (conversation-lease-acquire configuration normalized))
           (unless (probe-file pathname)
             (error 'conversation-error
                    :message
                    (format nil "Conversation ~A does not exist."
                            (conversation-identifier-display normalized))
                    :pathname pathname
                    :sequence nil))
           (handler-case
               (delete-file pathname)
             (error (condition)
               (error 'conversation-invariant-error
                      :message
                      (format nil "Could not delete conversation ~A: ~A"
                              (conversation-identifier-display normalized)
                              condition)
                      :pathname pathname
                      :sequence nil)))
           (handler-case
               (progn
                 (dolist (sidecar-pathname sidecar-pathnames)
                   (when (probe-file sidecar-pathname)
                     (delete-file sidecar-pathname)))
                 (dolist (root artifact-roots)
                   (when (probe-file root)
                     (funcall *conversation-delete-directory-tree-function*
                              root
                              :validate t
                              :if-does-not-exist :ignore))))
             (error (condition)
               (error 'conversation-invariant-error
                      :message
                      (format
                       nil
                       "Conversation ~A was deleted, but private artifacts remain: ~A"
                       (conversation-identifier-display normalized)
                       condition)
                      :pathname pathname
                      :sequence nil))))
      (when lease
        (conversation-lease-release lease)))
    pathname))
