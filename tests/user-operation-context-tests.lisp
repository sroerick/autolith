(in-package #:autolith)

;;;; -- Durable User Operation Context Tests --

(-> user-operation-context-tests--source (list) string)
(defun user-operation-context-tests--source (record)
  "Return the source string from user-operation RECORD."
  (getf (rest record) :source))

(-> user-operation-context-tests--invalid-replay-p
    (configuration string list)
    boolean)
(defun user-operation-context-tests--invalid-replay-p
    (configuration identifier properties)
  "Persist PROPERTIES as one raw user operation and report replay rejection."
  (let ((conversation
          (conversation-create configuration :identifier identifier)))
    (conversation-append-record
     conversation (list* :user-operation properties))
    (handler-case
        (progn
          (conversation-load (conversation-pathname conversation))
          nil)
      (conversation-invariant-error ()
        t))))

(-> test-user-operation-persistence-and-context () null)
(defun test-user-operation-persistence-and-context ()
  "Test bounded durable user-operation replay and request-local projection."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "user-operation-context")))
    (unwind-protect
         (progn
           (conversation-append-user-operation
            conversation
            :kind ':lisp
            :source "(setf *example* 1)"
            :status ':ok
            :result "⇒ 1")
           (conversation-append-user-operation
            conversation
            :kind ':command
            :source "/help"
            :status ':ok
            :result "loop action: continue")
           (let* ((records (conversation-user-operation-snapshot conversation))
                  (provider-items-before
                    (copy-tree (conversation-input-items conversation)))
                  (*context-contributors* nil)
                  (*context-next-request-delivered*
                    (make-hash-table :test #'equal))
                  (*context-last-deliveries*
                    (make-hash-table :test #'equal))
                  (*context-last-delivery-order* nil))
             (test-assert
              (and (= (length records) 2)
                   (equal (mapcar #'user-operation-context-tests--source records)
                          '("(setf *example* 1)" "/help"))
                   (every (lambda (record)
                            (eq (first record) ':user-operation))
                          records)
                   (null provider-items-before))
              "local operations persist outside ordinary provider input history")
             (setf (char (user-operation-context-tests--source (first records)) 0)
                   #\X)
             (test-assert
              (string= (user-operation-context-tests--source
                        (first
                         (conversation-user-operation-snapshot conversation)))
                       "(setf *example* 1)")
              "user-operation snapshots detach mutable source strings")
             (register-context-contributor "recent-user-operations"
                                           'user-operation-context
                                           :source ':built-in)
             (let* ((delivery
                      (context-resolve-request configuration conversation #()))
                    (contribution
                      (find "recent-user-operations"
                            (context-delivery-contributions delivery)
                            :test #'string=
                            :key #'context-contribution-identifier))
                    (evidence
                      (and contribution
                           (context-contribution-evidence contribution))))
               (test-assert
                (and contribution
                     (eq (context-contribution-class contribution) ':mandatory)
                     (search "outside normal provider conversation history"
                             (context-contribution-instruction contribution))
                     (< (search "/help" evidence)
                        (search "(setf *example* 1)" evidence))
                     (equal provider-items-before
                            (conversation-input-items conversation)))
                "request context exposes newest-first untrusted operation evidence without mutating provider history"))
             (let ((*user-operation-context-evidence-limit* 80))
               (let ((contribution
                       (user-operation-context
                        (make-instance
                         'request-context
                         :configuration configuration
                         :conversation conversation
                         :tool-namespaces #()))))
                 (test-assert
                  (= (length (context-contribution-evidence contribution)) 80)
                  "user-operation evidence obeys its exact character bound")))
             (let ((records
                     (list
                      (list :user-operation
                            :seq 1
                            :time (get-universal-time)
                            :kind ':lisp
                            :source (make-string 4000 :initial-element #\s)
                            :status ':ok
                            :result (make-string 4000 :initial-element #\r)))))
               (let ((*user-operation-context-evidence-limit* 5000))
                 (test-assert
                  (= (length (user-operation-context--evidence records))
                     *context-contribution-evidence-limit*)
                  "operation evidence cannot exceed the context protocol bound"))
               (let ((*user-operation-context-evidence-limit* -1))
                 (test-assert
                  (= (length (user-operation-context--evidence records)) 1900)
                  "an invalid live evidence limit falls back without suppressing context")))
             (test-assert
              (null
               (user-operation-context
                (make-instance
                 'request-context
                 :configuration configuration
                 :conversation conversation
                 :tool-namespaces #()
                 :compaction-p t)))
              "side-channel compaction omits recent user-operation context"))
           (let* ((*conversation-user-operation-source-limit* 1)
                  (*conversation-user-operation-result-limit* 0)
                  (loaded
                    (conversation-load
                     (conversation-pathname conversation)))
                  (loaded-records
                    (conversation-user-operation-snapshot loaded)))
             (test-assert
              (and (= (length loaded-records) 2)
                   (equal (mapcar #'user-operation-context-tests--source
                                  loaded-records)
                          '("(setf *example* 1)" "/help"))
                   (null (conversation-input-items loaded)))
              "conversation replay reconstructs user-operation context without provider messages"))
           (let ((empty
                   (conversation-create configuration
                                        :identifier "user-operation-empty")))
             (test-assert
              (null
               (user-operation-context
                (make-instance
                 'request-context
                 :configuration configuration
                 :conversation empty
                 :tool-namespaces #())))
              "an empty conversation contributes no user-operation context")))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)

(-> test-user-operation-bounds-and-validation () null)
(defun test-user-operation-bounds-and-validation ()
  "Test exact text bounds, recent-window eviction, and strict durable replay."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "user-operation-bounds"))
                  (*conversation-user-operation-source-limit* 40)
                  (*conversation-user-operation-result-limit* 30)
                  (record
                    (conversation-append-user-operation
                     conversation
                     :kind ':lisp
                     :source (make-string 100 :initial-element #\s)
                     :status ':ok
                     :result (make-string 100 :initial-element #\r)))
                  (properties (rest record)))
             (test-assert
              (and (= (length (getf properties :source)) 40)
                   (= (length (getf properties :result)) 30)
                   (uiop:string-suffix-p (getf properties :source)
                                         "... [truncated]")
                   (uiop:string-suffix-p (getf properties :result)
                                         "... [truncated]"))
              "durable operation text truncates within exact configured bounds"))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-zero-bound")))
             (let ((*conversation-user-operation-source-limit* 0))
               (test-assert
                (and
                 (handler-case
                     (progn
                       (conversation-append-user-operation
                        conversation
                        :kind ':lisp
                        :source "(values)"
                        :status ':ok
                        :result "")
                       nil)
                   (configuration-error ()
                     t))
                 (= (conversation-next-sequence conversation) 1)
                 (not (conversation-persisted-p conversation)))
                "an invalid source bound fails before any durable append")))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-zero-count")))
             (let ((*conversation-user-operation-count-limit* 0))
               (conversation-append-user-operation
                conversation
                :kind ':command
                :source "/help"
                :status ':ok
                :result "loop action: continue")
               (let ((loaded
                       (conversation-load
                        (conversation-pathname conversation))))
                 (test-assert
                  (and (conversation-persisted-p conversation)
                       (= (conversation-next-sequence conversation) 2)
                       (null (conversation-user-operation-snapshot conversation))
                       (null (conversation-user-operation-snapshot loaded)))
                  "a zero count limit preserves durable records but retains no context"))))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-zero-characters")))
             (let ((*conversation-user-operation-character-limit* 0))
               (conversation-append-user-operation
                conversation
                :kind ':command
                :source "/help"
                :status ':ok
                :result "loop action: continue")
               (let ((loaded
                       (conversation-load
                        (conversation-pathname conversation))))
                 (test-assert
                  (and (conversation-persisted-p conversation)
                       (= (conversation-next-sequence conversation) 2)
                       (null (conversation-user-operation-snapshot conversation))
                       (null (conversation-user-operation-snapshot loaded)))
                  "a zero character limit preserves durable records but retains no context"))))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-retention")))
             (let ((*conversation-user-operation-count-limit* 2)
                   (*conversation-user-operation-character-limit* 1000))
               (dolist (source '("one" "two" "three"))
                 (conversation-append-user-operation
                  conversation
                  :kind ':command
                  :source source
                  :status ':ok
                  :result "x"))
               (test-assert
                (equal (mapcar #'user-operation-context-tests--source
                               (conversation-user-operation-snapshot conversation))
                       '("two" "three"))
                "the recent operation projection evicts oldest records by count"))
             (let ((*conversation-user-operation-count-limit* 16)
                   (*conversation-user-operation-character-limit* 8))
               (conversation-append-user-operation
                conversation
                :kind ':command
                :source "four"
                :status ':ok
                :result "1234")
               (test-assert
                (equal (mapcar #'user-operation-context-tests--source
                               (conversation-user-operation-snapshot conversation))
                       '("four"))
                "the recent operation projection evicts oldest records by aggregate characters")))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-future-field")))
             (conversation-append-record
              conversation
              (list :user-operation
                    :kind ':lisp
                    :source "(values :future)"
                    :status ':ok
                    :result "⇒ :FUTURE"
                    :future-field "ignored"))
             (let ((loaded
                     (conversation-load
                      (conversation-pathname conversation))))
               (test-assert
                (equal
                 (mapcar #'user-operation-context-tests--source
                         (conversation-user-operation-snapshot loaded))
                 '("(values :future)"))
                "replay tolerates bounded unknown user-operation fields")))
           (test-assert
            (user-operation-context-tests--invalid-replay-p
             configuration
             "user-operation-invalid-kind"
             (list :kind ':tool
                   :source "invalid"
                   :status ':ok
                   :result ""))
            "replay rejects an unsupported durable user-operation kind")
           (test-assert
            (user-operation-context-tests--invalid-replay-p
             configuration
             "user-operation-oversized"
             (list :kind ':lisp
                   :source
                   (make-string (1+ (* 64 1024))
                                :initial-element #\x)
                   :status ':ok
                   :result ""))
            "replay rejects oversized durable user-operation text")
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "user-operation-circular"))
                  (record
                    (list :user-operation
                          :seq 2
                          :time (get-universal-time)
                          :kind ':lisp
                          :source "(values)"
                          :status ':ok
                          :result ""))
                  (properties (rest record))
                  (tail (last properties)))
             (conversation-append-user-operation
              conversation
              :kind ':lisp
              :source "(values :valid)"
              :status ':ok
              :result "⇒ :VALID")
             (setf (rest tail) properties)
             (unwind-protect
                  (progn
                    (with-open-file
                        (stream (conversation-pathname conversation)
                                :direction :output
                                :if-exists :append
                                :external-format ':utf-8)
                      (terpri stream)
                      (write record :stream stream :readably t :circle t)
                      (terpri stream))
                    (test-assert
                     (handler-case
                         (progn
                           (conversation-load
                            (conversation-pathname conversation))
                           nil)
                       (conversation-invariant-error ()
                         t))
                     "replay rejects a circular conversation record without hanging"))
               (setf (rest tail) nil)))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-circular-header")))
             (conversation-append-user-operation
              conversation
              :kind ':lisp
              :source "(values :valid)"
              :status ':ok
              :result "⇒ :VALID")
             (let* ((records
                      (conversation--read-records
                       (conversation-pathname conversation)))
                    (header (copy-list (first records)))
                    (durable-record (second records))
                    (properties (rest header))
                    (tail (last properties)))
               (setf (rest tail) properties)
               (unwind-protect
                    (progn
                      (with-open-file
                          (stream (conversation-pathname conversation)
                                  :direction :output
                                  :if-exists :supersede
                                  :external-format ':utf-8)
                        (write header :stream stream :readably t :circle t)
                        (terpri stream)
                        (write durable-record :stream stream :readably t)
                        (terpri stream))
                      (test-assert
                       (and
                        (null
                         (conversation-peek-header
                          (conversation-pathname conversation)))
                        (handler-case
                            (progn
                              (conversation-load
                               (conversation-pathname conversation))
                              nil)
                          (conversation-invariant-error ()
                            t)))
                       "header peeking and replay reject a circular header without hanging"))
                 (setf (rest tail) nil))))
           (test-assert
            (user-operation-context-tests--invalid-replay-p
             configuration
             "user-operation-missing-status"
             (list :kind ':lisp
                   :source "(values)"
                   :result ""))
            "replay rejects an incomplete durable user-operation property list"))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)

(-> run-user-operation-context-tests () null)
(defun run-user-operation-context-tests ()
  "Run durable local user-operation projection tests."
  (test-user-operation-persistence-and-context)
  (test-user-operation-bounds-and-validation)
  nil)
