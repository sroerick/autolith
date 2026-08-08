(in-package #:autolith)

;;;; -- Recovery Input Vault Conditions --

(define-condition recovery-input-vault-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader recovery-input-vault-error-pathname
    :type pathname
    :documentation "The pending-input or vault pathname involved in the failure.")
   (operation
    :initarg :operation
    :reader recovery-input-vault-error-operation
    :type keyword
    :documentation "The read, validation, publication, or deletion operation that failed.")
   (cause
    :initarg :cause
    :initform nil
    :reader recovery-input-vault-error-cause
    :type t
    :documentation "The underlying condition, when one was available."))
  (:documentation "A failure preserving or reading recovered interactive input."))

(-> application-recovery-input-vault--signal
    (pathname keyword &key (:message string) (:cause t))
    nil)
(defun application-recovery-input-vault--signal
    (pathname operation &key message cause)
  "Signal a structured recovery-input failure for PATHNAME and OPERATION."
  (error 'recovery-input-vault-error
         :message message
         :pathname pathname
         :operation operation
         :cause cause))


;;;; -- Strict Durable Forms --

(-> application-recovery-input-vault--proper-list-p (t) boolean)
(defun application-recovery-input-vault--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (handler-case
      (not (null (list-length value)))
    (error ()
      nil)))

(-> application-recovery-input-vault--properties-p (t list list) boolean)
(defun application-recovery-input-vault--properties-p
    (properties allowed-keys required-keys)
  "Return true when PROPERTIES is a unique bounded property list with valid keys."
  (and (application-recovery-input-vault--proper-list-p properties)
       (evenp (length properties))
       (let ((keys (loop for key in properties by #'cddr collect key)))
         (and (every (lambda (key)
                       (member key allowed-keys :test #'eq))
                     keys)
              (= (length keys) (length (remove-duplicates keys :test #'eq)))
              (every (lambda (key)
                       (member key keys :test #'eq))
                     required-keys)))))

(-> application-recovery-input-vault--read-form (pathname keyword) t)
(defun application-recovery-input-vault--read-form (pathname operation)
  "Read one complete snapshot form from PATHNAME or signal a vault error."
  (handler-case
      (multiple-value-bind (form complete-p)
          (snapshot-read pathname)
        (unless complete-p
          (application-recovery-input-vault--signal
           pathname operation
           :message
           (format nil "Recovered input state at ~A is truncated."
                   (namestring pathname))))
        form)
    (recovery-input-vault-error (condition)
      (error condition))
    (error (condition)
      (application-recovery-input-vault--signal
       pathname operation
       :message
       (format nil "Could not read recovered input state at ~A."
               (namestring pathname))
       :cause condition))))

(-> application-recovery-input-vault--pending-form-shape-p (t) boolean)
(defun application-recovery-input-vault--pending-form-shape-p (form)
  "Return true when FORM has one supported pending-input record shape."
  (and (application-recovery-input-vault--proper-list-p form)
       (eq (first form) ':pending-inputs)
       (let* ((properties (rest form))
              (version (and (application-recovery-input-vault--proper-list-p
                             properties)
                            (getf properties :version))))
         (case version
           (1
            (application-recovery-input-vault--properties-p
             properties
             '(:version :conversation-id :steering :work)
             '(:version :conversation-id :steering :work)))
           (2
            (application-recovery-input-vault--properties-p
             properties
             '(:version :snapshot-identifier :conversation-id :active-work
               :steering-in-flight :steering :work
               :vault-capture-identifiers :steering-promotion-prefix-count)
             '(:version :snapshot-identifier :conversation-id :active-work
               :steering-in-flight :steering :work)))
           (otherwise
            nil)))))

(-> application-recovery-input-vault--pending-payload-form (list) list)
(defun application-recovery-input-vault--pending-payload-form (state)
  "Return STATE's input payload without migration or snapshot identity."
  (let ((form
          (application-input-controller--pending-state-form
           (list :snapshot-identifier "snapshot"
                 :active-work (getf state :active-work)
                 :active-work-identifier (getf state :active-work-identifier)
                 :steering-in-flight-items
                 (getf state :steering-in-flight-items)
                 :steering-items (getf state :steering-items)
                 :work-items (getf state :work-items)
                 :vault-capture-identifiers
                 (getf state :vault-capture-identifiers)
                 :legacy-p nil)
           "conversation")))
    (list :active-work (getf (rest form) :active-work)
          :steering-in-flight (getf (rest form) :steering-in-flight)
          :steering (getf (rest form) :steering)
          :work (getf (rest form) :work)
          :vault-capture-identifiers
          (getf (rest form) :vault-capture-identifiers))))

(-> application-recovery-input-vault--pending-payload-equal-p (list list) boolean)
(defun application-recovery-input-vault--pending-payload-equal-p (left right)
  "Return true when LEFT and RIGHT represent the same accepted input payload."
  (not
   (null
    (equal (application-recovery-input-vault--pending-payload-form left)
           (application-recovery-input-vault--pending-payload-form right)))))

(-> application-recovery-input-vault--read-pending-path
    (application pathname &key (:other-conversation-p boolean))
    (values (option list) keyword))
(defun application-recovery-input-vault--read-pending-path
    (application pathname &key (other-conversation-p nil))
  "Read and normalize PATHNAME for APPLICATION.

The second value is :CURRENT or :OTHER.  :OTHER is returned only when
OTHER-CONVERSATION-P permits a valid legacy record for another conversation."
  (let* ((form
           (application-recovery-input-vault--read-form pathname ':read-pending))
         (conversation (application-conversation application))
         (conversation-identifier (conversation-identifier conversation)))
    (unless (application-recovery-input-vault--pending-form-shape-p form)
      (application-recovery-input-vault--signal
       pathname ':validate-pending
       :message
       (format nil "Recovered pending input at ~A has an unsupported form."
               (namestring pathname))))
    (let ((recorded-identifier (getf (rest form) :conversation-id)))
      (unless (non-empty-string-p recorded-identifier)
        (application-recovery-input-vault--signal
         pathname ':validate-pending
         :message
         (format nil "Recovered pending input at ~A has no conversation identity."
                 (namestring pathname))))
      (unless (string= recorded-identifier conversation-identifier)
        (if other-conversation-p
            (return-from application-recovery-input-vault--read-pending-path
              (values nil ':other))
            (application-recovery-input-vault--signal
             pathname ':validate-pending
             :message
             (format nil "Recovered pending input at ~A belongs to conversation ~A, not ~A."
                     (namestring pathname)
                     recorded-identifier
                     conversation-identifier))))
      (handler-case
          (let ((state
                  (application-input-controller--pending-state
                   form conversation-identifier)))
            (unless state
              (application-recovery-input-vault--signal
               pathname ':validate-pending
               :message
               (format nil "Recovered pending input at ~A is invalid."
                       (namestring pathname))))
            (values
             (application-input-controller--pending-state-filter-persisted
              state conversation)
             ':current))
        (recovery-input-vault-error (condition)
          (error condition))
        (error (condition)
          (application-recovery-input-vault--signal
           pathname ':validate-pending
           :message
           (format nil "Recovered pending input at ~A is invalid."
                   (namestring pathname))
           :cause condition))))))

(-> application-recovery-input-vault--read-pending
    (application)
    (values (option list) (option pathname) (option pathname)))
(defun application-recovery-input-vault--read-pending (application)
  "Return APPLICATION's normalized pending state, source, and legacy cleanup path."
  (let* ((configuration (application-configuration application))
         (conversation (application-conversation application))
         (canonical
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (legacy (configuration-legacy-pending-inputs-path configuration)))
    (cond
      ((probe-file canonical)
       (multiple-value-bind (canonical-state status)
           (application-recovery-input-vault--read-pending-path
            application canonical)
         (declare (ignore status))
         (let ((legacy-cleanup-pathname nil))
           (when (probe-file legacy)
             (let ((legacy-state nil)
                   (legacy-status nil)
                   (legacy-readable-p nil))
               (handler-case
                   (multiple-value-bind (state state-status)
                       (application-recovery-input-vault--read-pending-path
                        application legacy :other-conversation-p t)
                     (setf legacy-state state
                           legacy-status state-status
                           legacy-readable-p t))
                 (recovery-input-vault-error ()
                   ;; A canonical record is authoritative.  An unreadable global
                   ;; legacy record cannot be safely attributed or deleted here.
                   nil))
               (when (and legacy-readable-p (eq legacy-status ':current))
                 (unless
                     (application-recovery-input-vault--pending-payload-equal-p
                      canonical-state legacy-state)
                   (application-recovery-input-vault--signal
                    legacy ':validate-pending
                    :message
                    (format nil "Legacy pending input at ~A conflicts with the conversation-scoped snapshot."
                            (namestring legacy))))
                 (setf legacy-cleanup-pathname legacy))))
           (values canonical-state canonical legacy-cleanup-pathname))))
      ((probe-file legacy)
       (multiple-value-bind (legacy-state status)
           (application-recovery-input-vault--read-pending-path
            application legacy :other-conversation-p t)
         (if (eq status ':current)
             (values legacy-state legacy legacy)
             (values nil nil nil))))
      (t
       (values nil nil nil)))))


;;;; -- Vault Captures --

(-> application-recovery-input-vault--path (application) pathname)
(defun application-recovery-input-vault--path (application)
  "Return APPLICATION's exact conversation-scoped recovery vault pathname."
  (let ((configuration (application-configuration application))
        (conversation (application-conversation application)))
    (configuration-recovery-input-vault-path
     configuration (conversation-pathname conversation))))

(-> application-recovery-input-vault--capture-from-form
    (t string pathname)
    (option list))
(defun application-recovery-input-vault--capture-from-form
    (form conversation-identifier pathname)
  "Return one validated normalized vault capture from FORM."
  (when (and (application-recovery-input-vault--proper-list-p form)
             (application-recovery-input-vault--properties-p
              form
              '(:id :captured-at :active-work :steering-in-flight :steering :work)
              '(:id :captured-at :active-work :steering-in-flight :steering :work)))
    (let ((identifier (getf form :id))
          (captured-at (getf form :captured-at)))
      (when (and (non-empty-string-p identifier)
                 (typep captured-at '(integer 0)))
        (handler-case
            (let ((state
                    (application-input-controller--pending-state
                     (list :pending-inputs
                           :version 2
                           :snapshot-identifier identifier
                           :conversation-id conversation-identifier
                           :active-work (getf form :active-work)
                           :steering-in-flight (getf form :steering-in-flight)
                           :steering (getf form :steering)
                           :work (getf form :work)
                           :vault-capture-identifiers nil)
                     conversation-identifier)))
              (when (and state
                         (plusp
                          (application-input-controller--pending-state-input-count
                           state)))
                (list :id (copy-seq identifier)
                      :captured-at captured-at
                      :active-work (getf state :active-work)
                      :active-work-identifier
                      (getf state :active-work-identifier)
                      :steering-in-flight-items
                      (getf state :steering-in-flight-items)
                      :steering-items (getf state :steering-items)
                      :work-items (getf state :work-items)
                      :vault-capture-identifiers nil
                      :legacy-p nil)))
          (error (condition)
            (application-recovery-input-vault--signal
             pathname ':validate-vault
             :message
             (format nil "Recovery vault capture ~A at ~A is invalid."
                     identifier (namestring pathname))
             :cause condition)))))))

(-> application-recovery-input-vault--capture-form (list string) list)
(defun application-recovery-input-vault--capture-form
    (capture conversation-identifier)
  "Return the portable durable form for normalized CAPTURE."
  (let* ((identifier (getf capture :id))
         (pending-form
           (application-input-controller--pending-state-form
            (list :snapshot-identifier identifier
                  :active-work (getf capture :active-work)
                  :active-work-identifier
                  (getf capture :active-work-identifier)
                  :steering-in-flight-items
                  (getf capture :steering-in-flight-items)
                  :steering-items (getf capture :steering-items)
                  :work-items (getf capture :work-items)
                  :vault-capture-identifiers nil
                  :legacy-p nil)
            conversation-identifier)))
    (list :id (copy-seq identifier)
          :captured-at (getf capture :captured-at)
          :active-work (getf (rest pending-form) :active-work)
          :steering-in-flight (getf (rest pending-form) :steering-in-flight)
          :steering (getf (rest pending-form) :steering)
          :work (getf (rest pending-form) :work))))

(-> application-recovery-input-vault--capture-payload-equal-p (list list) boolean)
(defun application-recovery-input-vault--capture-payload-equal-p (left right)
  "Return true when LEFT and RIGHT carry the same captured pending input."
  (not
   (null
    (equal
     (application-recovery-input-vault--pending-payload-form left)
     (application-recovery-input-vault--pending-payload-form right)))))

(-> application-recovery-input-vault--sort-captures (list) list)
(defun application-recovery-input-vault--sort-captures (captures)
  "Return a stable chronological copy of CAPTURES."
  (stable-sort (copy-list captures)
               #'<
               :key (lambda (capture)
                      (getf capture :captured-at))))

(-> application-recovery-input-vault--read-captures (application) list)
(defun application-recovery-input-vault--read-captures (application)
  "Strictly read and validate APPLICATION's conversation-scoped vault captures."
  (let ((pathname (application-recovery-input-vault--path application)))
    (if (not (probe-file pathname))
        nil
        (let* ((form
                 (application-recovery-input-vault--read-form pathname ':read-vault))
               (properties
                 (and (application-recovery-input-vault--proper-list-p form)
                      (rest form)))
               (conversation-identifier
                 (conversation-identifier
                  (application-conversation application))))
          (unless
              (and (application-recovery-input-vault--proper-list-p form)
                   (eq (first form) ':recovery-input-vault)
                   (application-recovery-input-vault--properties-p
                    properties
                    '(:version :conversation-id :captures)
                    '(:version :conversation-id :captures))
                   (eql (getf properties :version) 1)
                   (stringp (getf properties :conversation-id))
                   (string= (getf properties :conversation-id)
                            conversation-identifier)
                   (application-recovery-input-vault--proper-list-p
                    (getf properties :captures)))
            (application-recovery-input-vault--signal
             pathname ':validate-vault
             :message
             (format nil "Recovery input vault at ~A has an unsupported form."
                     (namestring pathname))))
          (let* ((capture-forms (getf properties :captures))
                 (captures
                   (mapcar
                    (lambda (capture-form)
                      (or
                       (application-recovery-input-vault--capture-from-form
                        capture-form conversation-identifier pathname)
                       (application-recovery-input-vault--signal
                        pathname ':validate-vault
                        :message
                        (format nil "Recovery input vault at ~A contains an invalid capture."
                                (namestring pathname)))))
                    capture-forms))
                 (identifiers (mapcar (lambda (capture)
                                        (getf capture :id))
                                      captures)))
            (unless (= (length identifiers)
                       (length (remove-duplicates identifiers :test #'string=)))
              (application-recovery-input-vault--signal
               pathname ':validate-vault
               :message
               (format nil "Recovery input vault at ~A contains duplicate capture identifiers."
                       (namestring pathname))))
            (application-recovery-input-vault--sort-captures captures))))))

(-> application-recovery-input-vault-captures (application) list)
(defun application-recovery-input-vault-captures (application)
  "Return APPLICATION's validated recovery input captures chronologically."
  (application-recovery-input-vault--read-captures application))

(-> application-recovery-input-vault--form (application list) list)
(defun application-recovery-input-vault--form (application captures)
  "Return the complete version-one vault form for CAPTURES."
  (let ((conversation-identifier
          (conversation-identifier (application-conversation application))))
    (list :recovery-input-vault
          :version 1
          :conversation-id conversation-identifier
          :captures
          (mapcar (lambda (capture)
                    (application-recovery-input-vault--capture-form
                     capture conversation-identifier))
                  (application-recovery-input-vault--sort-captures captures)))))

(-> application-recovery-input-vault--write-captures (application list) null)
(defun application-recovery-input-vault--write-captures (application captures)
  "Atomically replace APPLICATION's vault with CAPTURES, deleting an empty vault."
  (let ((pathname (application-recovery-input-vault--path application)))
    (handler-case
        (if captures
            (progn
              (ensure-directories-exist pathname)
              (snapshot-write
               pathname
               (application-recovery-input-vault--form application captures)
               :mode #o600))
            (when (probe-file pathname)
              (delete-file pathname)))
      (error (condition)
        (application-recovery-input-vault--signal
         pathname ':write-vault
         :message
         (format nil "Could not publish the recovery input vault at ~A."
                 (namestring pathname))
         :cause condition))))
  nil)


;;;; -- Recovery Import Transaction --

(-> application-recovery-input-vault--capture-state (list integer) list)
(defun application-recovery-input-vault--capture-state (state captured-at)
  "Return a vault capture for normalized pending STATE at CAPTURED-AT."
  (list :id (copy-seq (getf state :snapshot-identifier))
        :captured-at captured-at
        :active-work (getf state :active-work)
        :active-work-identifier (getf state :active-work-identifier)
        :steering-in-flight-items (getf state :steering-in-flight-items)
        :steering-items (getf state :steering-items)
        :work-items (getf state :work-items)
        :vault-capture-identifiers nil
        :legacy-p nil))

(-> application-recovery-input-vault--replacement-captures
    (application list list)
    list)
(defun application-recovery-input-vault--replacement-captures
    (application captures pending-state)
  "Return the vault state after transactionally importing PENDING-STATE."
  (let* ((vault-pathname (application-recovery-input-vault--path application))
         (represented-identifiers
           (getf pending-state :vault-capture-identifiers))
         (remaining
           (remove-if
            (lambda (capture)
              (member (getf capture :id)
                      represented-identifiers
                      :test #'string=))
            captures)))
    (if (zerop
         (application-input-controller--pending-state-input-count pending-state))
        remaining
        (let* ((identifier (getf pending-state :snapshot-identifier))
               (existing
                 (find identifier remaining
                       :key (lambda (capture) (getf capture :id))
                       :test #'string=))
               (capture
                 (application-recovery-input-vault--capture-state
                  pending-state (get-universal-time))))
          (cond
            ((null existing)
             (append remaining (list capture)))
            ((application-recovery-input-vault--capture-payload-equal-p
              existing capture)
             remaining)
            (t
             (application-recovery-input-vault--signal
              vault-pathname ':validate-vault
              :message
              (format nil "Recovery vault capture ~A conflicts with the pending snapshot at ~A."
                      identifier (namestring vault-pathname)))))))))

(-> application-recovery-input-vault--write-canonical-pending
    (application list pathname)
    null)
(defun application-recovery-input-vault--write-canonical-pending
    (application state pathname)
  "Publish normalized pending STATE at canonical PATHNAME."
  (handler-case
      (progn
        (ensure-directories-exist pathname)
        (snapshot-write
         pathname
         (application-input-controller--pending-state-form
          state
          (conversation-identifier (application-conversation application)))
         :mode #o600))
    (error (condition)
      (application-recovery-input-vault--signal
       pathname ':write-pending
       :message
       (format nil "Could not normalize recovered pending input at ~A."
               (namestring pathname))
       :cause condition)))
  nil)

(-> application-recovery-input-vault--delete (pathname keyword) null)
(defun application-recovery-input-vault--delete (pathname operation)
  "Delete PATHNAME for OPERATION or signal a recovery-input error."
  (handler-case
      (when (probe-file pathname)
        (delete-file pathname))
    (error (condition)
      (application-recovery-input-vault--signal
       pathname operation
       :message
       (format nil "Could not delete recovered input state at ~A."
               (namestring pathname))
       :cause condition)))
  nil)

(-> application-recovery-input-vault--import (application) null)
(defun application-recovery-input-vault--import (application)
  "Import APPLICATION's pending snapshot into its vault without enqueueing it."
  (multiple-value-bind (pending-state source-pathname legacy-cleanup-pathname)
      (application-recovery-input-vault--read-pending application)
    ;; Both durable inputs are fully validated before the first mutation.
    (let ((captures (application-recovery-input-vault--read-captures application)))
      (when pending-state
        (let* ((configuration (application-configuration application))
               (conversation (application-conversation application))
               (canonical-pathname
                 (configuration-pending-inputs-path
                  configuration (conversation-pathname conversation)))
               (replacement-captures
                 (application-recovery-input-vault--replacement-captures
                  application captures pending-state)))
          (when (or (getf pending-state :legacy-p)
                    (not (equal source-pathname canonical-pathname)))
            (application-recovery-input-vault--write-canonical-pending
             application pending-state canonical-pathname))
          (application-recovery-input-vault--write-captures
           application replacement-captures)
          (when legacy-cleanup-pathname
            (application-recovery-input-vault--delete
             legacy-cleanup-pathname ':delete-legacy-pending))
          (application-recovery-input-vault--delete
           canonical-pathname ':delete-pending)))))
  nil)

(-> application-recovery-input-vault-import (application) boolean)
(defun application-recovery-input-vault-import (application)
  "Vault recovered pending input for APPLICATION and record any safe failure."
  (handler-case
      (progn
        (application-recovery-input-vault--import application)
        (setf (application-recovery-input-vault-failure application) nil)
        t)
    (recovery-input-vault-error (condition)
      (setf (application-recovery-input-vault-failure application) condition)
      nil)
    (error (cause)
      (let* ((pathname (application-recovery-input-vault--path application))
             (condition
               (make-condition
                'recovery-input-vault-error
                :message
                (format nil "Could not preserve recovered input for conversation ~A."
                        (conversation-identifier
                         (application-conversation application)))
                :pathname pathname
                :operation ':import
                :cause cause)))
        (setf (application-recovery-input-vault-failure application) condition)
        nil))))
