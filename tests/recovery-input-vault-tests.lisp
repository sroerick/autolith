(in-package #:autolith)

;;;; -- Recovery Input Vault Test Support --

(-> recovery-input-vault-tests--octets (pathname) (simple-array (unsigned-byte 8) (*)))
(defun recovery-input-vault-tests--octets (pathname)
  "Return the exact octets stored at PATHNAME."
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(-> recovery-input-vault-tests--application
    (configuration string)
    application)
(defun recovery-input-vault-tests--application (configuration identifier)
  "Return a minimal application for one durable test conversation."
  (let ((conversation
          (conversation-create configuration :identifier identifier)))
    (conversation-append-user-message conversation "seed")
    (make-instance 'application
                   :configuration configuration
                   :conversation conversation)))


;;;; -- Recovery Import --

(-> test-recovery-input-vault-import () null)
(defun test-recovery-input-vault-import ()
  "Test recovery import filters provenance and remains idempotent."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-import"))
         (conversation (application-conversation application))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (pending-form
           (list :pending-inputs
                 :version 2
                 :snapshot-identifier "snapshot-import"
                 :conversation-id (conversation-identifier conversation)
                 :active-work
                 '(:identifier "active-input"
                   :work (:message "active work"))
                 :steering-in-flight
                 '((:identifier "already-durable" :input "persisted steering")
                   (:identifier "in-flight" :input "in-flight steering"))
                 :steering '("queued steering")
                 :work '((:message "queued follow-up"))
                 :vault-capture-identifiers nil)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message
            conversation "persisted steering"
            :pending-input-identifier "already-durable")
           (ensure-directories-exist pending-pathname)
           (snapshot-write pending-pathname pending-form :mode #o600)
           (test-assert
            (application-recovery-input-vault-import application)
            "recovery import succeeds for one valid pending snapshot")
           (test-assert
            (and (null (application-recovery-input-vault-failure application))
                 (null (application-input-controller application))
                 (not (probe-file pending-pathname)))
            "recovery import neither reports failure nor creates or loads a controller")
           (let* ((captures
                    (application-recovery-input-vault-captures application))
                  (capture (first captures)))
             (test-assert
              (and (= (length captures) 1)
                   (string= (getf capture :id) "snapshot-import")
                   (equal (getf capture :active-work)
                          '(:message "active work"))
                   (equal
                    (mapcar #'agent-steering-input-content
                            (getf capture :steering-in-flight-items))
                    '("in-flight steering"))
                   (equal (getf capture :steering-items)
                          '("queued steering"))
                   (equal (getf capture :work-items)
                          '((:message "queued follow-up"))))
              "recovery import filters durable steering and preserves remaining input")
             ;; Give the existing capture a recognizable timestamp, then replay the
             ;; same pending snapshot as if recovery crashed after vault publication.
             (setf (getf capture :captured-at) 123)
             (application-recovery-input-vault--write-captures
              application captures))
           (snapshot-write pending-pathname pending-form :mode #o600)
           (test-assert
            (application-recovery-input-vault-import application)
            "repeating an imported snapshot succeeds idempotently")
           (let ((captures
                   (application-recovery-input-vault-captures application)))
             (test-assert
              (and (= (length captures) 1)
                   (= (getf (first captures) :captured-at) 123))
              "repeated import keeps the original capture rather than duplicating it"))
           (let ((conflicting-form (copy-tree pending-form))
                 (vault-octets (recovery-input-vault-tests--octets vault-pathname)))
             (setf (getf (getf (rest conflicting-form) :active-work) :work)
                   '(:message "different active work"))
             (snapshot-write pending-pathname conflicting-form :mode #o600)
             (test-assert
              (not (application-recovery-input-vault-import application))
              "a reused snapshot identifier with different input is rejected")
             (test-assert
              (and (typep (application-recovery-input-vault-failure application)
                          'recovery-input-vault-error)
                   (probe-file pending-pathname)
                   (equalp vault-octets
                           (recovery-input-vault-tests--octets vault-pathname)))
              "a conflicting replay preserves both the pending snapshot and vault")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-recovery-input-vault-legacy-isolation () null)
(defun test-recovery-input-vault-legacy-isolation ()
  "Test legacy migration cleanup never consumes another conversation's input."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (first-application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-first"))
         (second-application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-second"))
         (second-conversation
           (application-conversation second-application))
         (legacy-pathname
           (configuration-legacy-pending-inputs-path configuration))
         (legacy-form
           (list :pending-inputs
                 :version 1
                 :conversation-id
                 (conversation-identifier second-conversation)
                 :steering '("legacy steering")
                 :work '((:message "legacy work")))))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (snapshot-write legacy-pathname legacy-form :mode #o600)
           (test-assert
            (application-recovery-input-vault-import first-application)
            "a valid global legacy snapshot for another conversation is ignored")
           (test-assert
            (and (probe-file legacy-pathname)
                 (null
                  (application-recovery-input-vault-captures first-application)))
            "another conversation's legacy pending input remains untouched")
           (test-assert
            (application-recovery-input-vault-import second-application)
            "the owning conversation imports its global legacy snapshot")
           (let ((captures
                   (application-recovery-input-vault-captures second-application)))
             (test-assert
              (and (= (length captures) 1)
                   (non-empty-string-p (getf (first captures) :id))
                   (null (getf (first captures) :steering-items))
                   (equal (getf (first captures) :work-items)
                          '((:message "legacy steering")
                            (:message "legacy work")))
                   (not (probe-file legacy-pathname)))
              "legacy import generates stable identity and deletes only owned state"))
           ;; Recreate the interrupted migration window: canonical v2 is durable
           ;; while its equivalent legacy source still exists.
           (let* ((third-application
                    (recovery-input-vault-tests--application
                     configuration "recovery-vault-migration"))
                  (third-conversation
                    (application-conversation third-application))
                  (canonical-pathname
                    (configuration-pending-inputs-path
                     configuration (conversation-pathname third-conversation)))
                  (third-legacy-form
                    (list :pending-inputs
                          :version 1
                          :conversation-id
                          (conversation-identifier third-conversation)
                          :steering '("migration steering")
                          :work '((:message "migration work"))))
                  (canonical-form
                    (list :pending-inputs
                          :version 2
                          :snapshot-identifier "migration-snapshot"
                          :conversation-id
                          (conversation-identifier third-conversation)
                          :active-work nil
                          :steering-in-flight nil
                          :steering '("migration steering")
                          :work '((:message "migration work"))
                          :vault-capture-identifiers nil)))
             (snapshot-write legacy-pathname third-legacy-form :mode #o600)
             (ensure-directories-exist canonical-pathname)
             (snapshot-write canonical-pathname canonical-form :mode #o600)
             (test-assert
              (application-recovery-input-vault-import third-application)
              "recovery completes an interrupted legacy-to-canonical migration")
             (test-assert
              (and (not (probe-file legacy-pathname))
                   (not (probe-file canonical-pathname))
                   (string=
                    (getf
                     (first
                      (application-recovery-input-vault-captures
                       third-application))
                     :id)
                    "migration-snapshot"))
              "migration recovery reuses canonical identity and removes both sources")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-recovery-input-vault-corruption () null)
(defun test-recovery-input-vault-corruption ()
  "Test truncated vault state blocks import without modifying either file."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-corrupt"))
         (conversation (application-conversation application))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (pending-form
           (list :pending-inputs
                 :version 2
                 :snapshot-identifier "corrupt-vault-pending"
                 :conversation-id (conversation-identifier conversation)
                 :active-work nil
                 :steering-in-flight nil
                 :steering nil
                 :work '((:message "preserve me"))
                 :vault-capture-identifiers nil)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (ensure-directories-exist pending-pathname)
           (snapshot-write pending-pathname pending-form :mode #o600)
           (ensure-directories-exist vault-pathname)
           (with-open-file (stream vault-pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (write-string "(:recovery-input-vault :version 1" stream))
           (let ((pending-octets
                   (recovery-input-vault-tests--octets pending-pathname))
                 (vault-octets
                   (recovery-input-vault-tests--octets vault-pathname)))
             (test-assert
              (not (application-recovery-input-vault-import application))
              "truncated vault state rejects recovery import")
             (let ((failure
                     (application-recovery-input-vault-failure application)))
               (test-assert
                (and (typep failure 'recovery-input-vault-error)
                     (equal (recovery-input-vault-error-pathname failure)
                            vault-pathname)
                     (equalp pending-octets
                             (recovery-input-vault-tests--octets
                              pending-pathname))
                     (equalp vault-octets
                             (recovery-input-vault-tests--octets
                              vault-pathname)))
                "corrupt vault failure leaves pending and vault bytes unchanged"))
             (test-assert
              (handler-case
                  (progn
                    (application-recovery-input-vault-captures application)
                    nil)
                (recovery-input-vault-error ()
                  t))
              "strict vault inspection reports the same corruption")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> run-recovery-input-vault-tests () boolean)
(defun run-recovery-input-vault-tests ()
  "Run focused recovery input vault tests and return true on success."
  (test-recovery-input-vault-import)
  (test-recovery-input-vault-legacy-isolation)
  (test-recovery-input-vault-corruption)
  t)
