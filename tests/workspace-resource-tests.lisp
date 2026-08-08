(in-package #:autolith)

;;;; -- Workspace Resource Test Support --

(-> workspace-resource-tests--write-text (pathname string) null)
(defun workspace-resource-tests--write-text (path text)
  "Replace PATH with exact UTF-8 TEXT for a workspace resource test."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-sequence (sb-ext:string-to-octets text :external-format :utf-8)
                    stream))
  nil)

(-> workspace-resource-tests--field (string string) string)
(defun workspace-resource-tests--field (content label)
  "Return the single-line value following LABEL in tool result CONTENT."
  (let* ((start (search label content))
         (value-start (and start (+ start (length label))))
         (end (and value-start
                   (or (position #\Newline content :start value-start)
                       (length content)))))
    (unless (and value-start end)
      (error "Missing result field ~S in ~S." label content))
    (subseq content value-start end)))

(-> workspace-resource-tests--call
    (tool-registry tool-context string string &rest t)
    tool-result)
(defun workspace-resource-tests--call
    (registry context namespace name &rest arguments)
  "Execute NAMESPACE.NAME with ARGUMENTS through REGISTRY and CONTEXT."
  (tool-registry-execute-call
   registry
   (json-object "namespace" namespace
                "name" name
                "arguments" (json-encode (apply #'json-object arguments)))
   context))

(-> workspace-resource-tests--operation (string &rest t) json-object)
(defun workspace-resource-tests--operation (name &rest fields)
  "Return one JSON resource edit operation named NAME with FIELDS."
  (apply #'json-object "op" name fields))

(defclass workspace-resource-tests-coordination ()
  ((lock
    :initform (make-lock "workspace resource read coordination")
    :reader workspace-resource-tests-coordination-lock
    :documentation "The lock protecting coordinated test state.")
   (condition
    :initform (make-condition-variable)
    :reader workspace-resource-tests-coordination-condition
    :documentation "The condition reporting coordinated observation progress.")
   (observed-p
    :initform nil
    :accessor workspace-resource-tests-coordination-observed-p
    :type boolean
    :documentation "Whether the coordinated resource observation completed."))
  (:documentation "Synchronization state for a deterministic resource.read race test."))

(defclass workspace-resource-tests-coordinated-resource
    (workspace-file-resource)
  ((coordination
    :initarg :coordination
    :reader workspace-resource-tests-coordinated-resource-coordination
    :type workspace-resource-tests-coordination
    :documentation "The synchronization state signaled after exact observation."))
  (:documentation "A workspace resource reporting when its exact observation completes."))

(defclass workspace-resource-tests-coordinated-resolver
    (workspace-file-resolver)
  ((coordination
    :initarg :coordination
    :reader workspace-resource-tests-coordinated-resolver-coordination
    :type workspace-resource-tests-coordination
    :documentation "The synchronization state shared with resolved resources."))
  (:documentation "Resolve coordinated workspace resources for serialization testing."))

(defclass workspace-resource-tests-other-observation-state
    (resource-observation-state)
  ()
  (:documentation "A non-workspace observation state sharing conversation storage."))

(defmethod resource-resolver-resolve
    ((resolver workspace-resource-tests-coordinated-resolver) identifier context)
  "Resolve IDENTIFIER as a coordinated workspace resource under CONTEXT."
  (let ((path (workspace-tool-path context (url-decode identifier))))
    (make-instance 'workspace-resource-tests-coordinated-resource
                   :uri          (workspace-file--canonical-uri context path)
                   :pathname     path
                   :coordination
                   (workspace-resource-tests-coordinated-resolver-coordination
                    resolver))))

(defmethod resource-observe :around
    ((resource workspace-resource-tests-coordinated-resource)
     (context tool-context))
  "Report completion after observing coordinated RESOURCE under CONTEXT."
  (let* ((observation (call-next-method))
         (coordination
           (workspace-resource-tests-coordinated-resource-coordination resource)))
    (with-lock-held ((workspace-resource-tests-coordination-lock coordination))
      (setf (workspace-resource-tests-coordination-observed-p coordination) t)
      (condition-notify
       (workspace-resource-tests-coordination-condition coordination)))
    observation))


;;;; -- Workspace Resource Tests --

(-> test-workspace-file-resources () null)
(defun test-workspace-file-resources ()
  "Test revision-gated workspace file resources and model-facing tools."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (workspace (merge-pathnames "workspace/" root))
         (configuration nil)
         (registry (make-default-tool-registry)))
    (ensure-directories-exist workspace)
    (setf configuration
          (configuration--clone base-configuration
                                :working-directory workspace))
    (unwind-protect
         (let* ((first-conversation
                  (conversation-create configuration :identifier "resource-first"))
                (second-conversation
                  (conversation-create configuration :identifier "resource-second"))
                (heterogeneous-conversation
                  (conversation-create configuration
                                       :identifier "resource-heterogeneous"))
                (first-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation first-conversation))
                (second-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation second-conversation))
                (heterogeneous-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation heterogeneous-conversation)))
           (labels ((call (context namespace name &rest arguments)
                      "Execute one resource fixture call."
                      (apply #'workspace-resource-tests--call
                             registry context namespace name arguments))

                    (read-resource (context uri &key start-line line-count)
                      "Read URI and return result, canonical URI, and revision."
                      (let ((arguments (list "uri" uri)))
                        (when start-line
                          (setf arguments
                                (append arguments (list "start-line" start-line))))
                        (when line-count
                          (setf arguments
                                (append arguments (list "line-count" line-count))))
                        (let* ((result (apply #'call context "resource" "read"
                                              arguments))
                               (content (tool-result-content result)))
                          (values result
                                  (and (tool-result-success-p result)
                                       (workspace-resource-tests--field
                                        content "URI: "))
                                  (and (tool-result-success-p result)
                                       (workspace-resource-tests--field
                                        content "Revision: "))))))

                    (edit-resource (context uri revision operations)
                      "Edit URI at REVISION with OPERATIONS."
                      (call context "resource" "edit"
                            "uri" uri
                            "base-revision" revision
                            "operations" (coerce operations 'vector))))
              (let ((path (merge-pathnames "descriptor-growth.txt" workspace))
                    (descriptor nil))
                (unwind-protect
                     (progn
                       (workspace-resource-tests--write-text path "before")
                       (setf descriptor
                             (sb-posix:open (namestring path) sb-posix:o-rdonly))
                       (let ((before (sb-posix:fstat descriptor)))
                         (with-open-file (stream path
                                                 :direction :output
                                                 :if-exists :append
                                                 :element-type '(unsigned-byte 8))
                           (write-sequence
                            (sb-ext:string-to-octets
                             "after"
                             :external-format :utf-8)
                            stream))
                         (test-assert
                          (not (workspace-file--stable-file-stat-p
                                before
                                (sb-posix:fstat descriptor)))
                          "descriptor observations reject files that grow during a read")))
                  (when descriptor
                    (sb-posix:close descriptor))))
             (let* ((path (merge-pathnames "heterogeneous.txt" workspace))
                    (other-observation
                      (make-instance 'resource-observation
                                     :uri "workspace:heterogeneous.txt"
                                     :revision "foreign-revision"
                                     :content 42))
                    (other-state
                      (make-instance
                       'workspace-resource-tests-other-observation-state
                       :alias "Rforeign"
                       :observation other-observation)))
               (workspace-resource-tests--write-text path "workspace")
               (with-recursive-lock-held
                   ((conversation-resource-observation-lock
                     heterogeneous-conversation))
                 (setf
                  (gethash
                   "Rforeign"
                   (conversation-resource-observations
                    heterogeneous-conversation))
                  other-state
                  (conversation-resource-observation-order
                   heterogeneous-conversation)
                  (list "Rforeign")))
               (let ((*workspace-file-resource-maximum-observations* 1))
                 (multiple-value-bind (result uri revision)
                     (read-resource heterogeneous-context
                                    "workspace:heterogeneous.txt")
                   (test-assert (tool-result-success-p result)
                                "workspace observations ignore other state classes")
                   (with-recursive-lock-held
                       ((conversation-resource-observation-lock
                         heterogeneous-conversation))
                     (let ((states
                             (conversation-resource-observations
                              heterogeneous-conversation)))
                       (test-assert
                        (eq (resource-observation-state-find
                             states "Rforeign"
                             'workspace-resource-tests-other-observation-state)
                            other-state)
                        "workspace observation expiry preserves other resource states")
                       (test-assert
                        (and (= (hash-table-count states) 2)
                             (= (workspace-file--observation-state-count states) 1)
                             (typep (gethash revision states)
                                    'workspace-file-observation-state))
                        "workspace and other resource observation states coexist")))
                   (test-assert
                    (handler-case
                        (progn
                          (workspace-file--find-observation-state
                           heterogeneous-conversation uri "Rforeign")
                          nil)
                      (resource-revision-stale ()
                        t))
                    "workspace observation lookup rejects another state class"))))
             (let* ((resolver
                      (gethash "workspace"
                               (resource-registry-resolvers
                                (tool-registry-resource-registry registry))))
                    (spaced-path (merge-pathnames "space name.txt" workspace)))
               (workspace-resource-tests--write-text spaced-path "content")
               (test-assert (typep resolver 'workspace-file-resolver)
                            "default registries install the workspace resolver")
               (let ((resource
                       (resource-registry-resolve
                        (tool-registry-resource-registry registry)
                        "workspace:space%20name.txt"
                        first-context)))
                 (test-assert
                  (string= (resource-uri resource)
                           "workspace:space%20name.txt")
                  "workspace resolution returns a canonical percent-encoded URI")
                 (test-assert
                  (equal (truename (workspace-file-resource-pathname resource))
                         (truename spaced-path))
                  "workspace resolution reuses workspace-relative path semantics")))
             (let ((*workspace-tool-readable-roots* (list workspace)))
               (test-assert
                (handler-case
                    (progn
                      (resource-registry-resolve
                       (tool-registry-resource-registry registry)
                       (format nil "workspace:~A"
                               (workspace-file--encode-identifier
                                (namestring (user-homedir-pathname))))
                       first-context)
                      nil)
                  (tool-error () t))
                "workspace resource URIs do not bypass readable-root confinement"))
             (let ((escape (merge-pathnames "escape" workspace)))
               (unwind-protect
                    (progn
                      (sb-posix:symlink
                       (namestring (user-homedir-pathname))
                       (namestring escape))
                      (let ((*workspace-tool-readable-roots* (list workspace)))
                        (test-assert
                         (handler-case
                             (progn
                               (resource-registry-resolve
                                (tool-registry-resource-registry registry)
                                "workspace:escape"
                                first-context)
                               nil)
                           (tool-error () t))
                         "workspace resource resolution rejects symlink escapes")))
                 (when (probe-file escape)
                   (sb-posix:unlink (namestring escape)))))
             (let ((oversized (merge-pathnames "oversized.txt" workspace)))
               (workspace-resource-tests--write-text oversized "123456789")
               (let ((*workspace-file-resource-maximum-bytes* 8))
                 (let ((result
                         (read-resource first-context
                                        "workspace:oversized.txt")))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "snapshot" (tool-result-content result)))
                    "resource.read rejects oversized exact snapshots with bounded guidance"))))
             (let ((invalid (merge-pathnames "invalid-utf8.txt" workspace)))
               (with-open-file (stream invalid
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :element-type '(unsigned-byte 8))
                 (write-byte #xFF stream))
               (let ((result
                       (read-resource first-context
                                      "workspace:invalid-utf8.txt")))
                 (test-assert
                  (and (not (tool-result-success-p result))
                       (search "valid UTF-8" (tool-result-content result)))
                  "resource.read rejects invalid UTF-8 without retaining a snapshot")))
             (let ((fifo (merge-pathnames "not-regular" workspace)))
               (unwind-protect
                    (progn
                      (sb-posix:mkfifo (namestring fifo) #o600)
                      (let ((result
                              (read-resource first-context
                                             "workspace:not-regular")))
                        (test-assert
                         (and (not (tool-result-success-p result))
                              (search "not a regular file"
                                      (tool-result-content result)))
                         "resource.read rejects special files before opening them")))
                 (when (probe-file fifo)
                   (sb-posix:unlink (namestring fifo)))))
             (let ((path (merge-pathnames "visible.txt" workspace)))
               (workspace-resource-tests--write-text
                path (format nil "one~%two~%three~%four~%five~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:visible.txt"
                                  :start-line 2 :line-count 2)
                 (test-assert (tool-result-success-p read-result)
                              "resource.read observes existing workspace files")
                 (test-assert
                  (and (search "Visible lines: 2-3 of 5"
                               (tool-result-content read-result))
                       (search "Elided: 1-1 before; 4-5 after"
                               (tool-result-content read-result))
                       (search "     2  two" (tool-result-content read-result)))
                  "resource.read reports URI, revision, visible range, elisions, and numbered content")
                 (test-assert
                  (not (tool-result-success-p
                        (edit-resource
                         first-context uri revision
                         (list
                          (workspace-resource-tests--operation
                           "replace-lines"
                           "start-line" 1
                           "end-line" 1
                           "content" "ONE")))))
                  "resource.edit rejects original lines outside the exact visible range")
                 (let ((isolated
                         (edit-resource
                          second-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-lines"
                            "start-line" 2
                            "end-line" 2
                            "content" "TWO")))))
                   (test-assert
                    (and (not (tool-result-success-p isolated))
                         (search "not observed in this conversation"
                                 (tool-result-content isolated)))
                    "resource revision aliases are isolated by conversation"))
                 (conversation-append-user-message
                  first-conversation "persist without resource snapshots")
                 (let* ((reloaded
                          (conversation-load-by-id configuration "resource-first"))
                        (reloaded-context
                          (make-instance 'tool-context
                                         :configuration configuration
                                         :worker nil
                                         :conversation reloaded))
                        (expired
                          (edit-resource
                           reloaded-context uri revision
                           (list
                            (workspace-resource-tests--operation
                             "replace-lines"
                             "start-line" 2
                             "end-line" 2
                             "content" "TWO")))))
                   (test-assert
                    (and (not (tool-result-success-p expired))
                         (zerop (hash-table-count
                                 (conversation-resource-observations reloaded))))
                    "resource observations stay out of conversation persistence and expire after reload"))))
             (let ((path (merge-pathnames "empty.txt" workspace)))
               (workspace-resource-tests--write-text path "")
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:empty.txt")
                 (test-assert
                  (and (tool-result-success-p read-result)
                       (search "Visible lines: none of 0"
                               (tool-result-content read-result)))
                  "resource.read establishes an exact revision for an empty file")
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-empty" "content"
                            (format nil "first~%second~%"))))))
                   (test-assert (tool-result-success-p result)
                                "resource.edit can populate an observed empty file")
                   (test-assert
                    (string= (workspace-file--read-content path)
                             (format nil "first~%second~%"))
                    "replace-empty preserves supplied content and final newline"))))
             (let ((path (merge-pathnames "multi.txt" workspace)))
               (workspace-resource-tests--write-text
                path (format nil "one~%two~%three~%four~%five~%six~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:multi.txt")
                 (declare (ignore read-result))
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-lines"
                            "start-line" 2
                            "end-line" 3
                            "content" (format nil "TWO~%THREE")
                            )
                           (workspace-resource-tests--operation
                            "insert-after"
                            "line" 5
                            "content" (format nil "five-and-a-half"))))))
                   (test-assert (tool-result-success-p result)
                                "resource.edit applies non-overlapping multi-operations")
                   (test-assert
                    (string= (workspace-file--read-content path)
                             (format nil
                                     "one~%TWO~%THREE~%four~%five~%five-and-a-half~%six~%"))
                    "resource.edit interprets every operation against original line numbers")
                   (let ((new-revision
                           (workspace-resource-tests--field
                            (tool-result-content result) "Revision: ")))
                     (test-assert (not (string= revision new-revision))
                                  "successful edits return a fresh revision alias")
                     (test-assert
                      (search "Visible lines:"
                              (tool-result-content result))
                      "successful edits return a refreshed nearby observation")
                     (test-assert
                      (tool-result-success-p
                       (edit-resource
                        first-context uri new-revision
                        (list
                         (workspace-resource-tests--operation
                          "replace-lines"
                          "start-line" 4
                          "end-line" 4
                          "content" "FOUR"))))
                      "the refreshed observation supports the next nearby edit")))))
             (let ((path (merge-pathnames "overlap.txt" workspace)))
               (workspace-resource-tests--write-text
                path (format nil "a~%b~%c~%d~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:overlap.txt")
                 (declare (ignore read-result))
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "delete-lines" "start-line" 2 "end-line" 3)
                           (workspace-resource-tests--operation
                            "insert-before" "line" 3 "content" "x")))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "overlap or ambiguously share"
                                 (tool-result-content result)))
                    "resource.edit rejects overlapping or ambiguous original-line operations")
                   (test-assert
                    (string= (workspace-file--read-content path)
                             (format nil "a~%b~%c~%d~%"))
                    "overlap rejection leaves the file unchanged"))))
             (let ((path (merge-pathnames "stale.txt" workspace)))
               (workspace-resource-tests--write-text path (format nil "old~%value~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:stale.txt")
                 (declare (ignore read-result))
                 (workspace-resource-tests--write-text
                  path (format nil "external~%value~%"))
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-lines"
                            "start-line" 1
                            "end-line" 1
                            "content" "new")))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "Reread" (tool-result-content result)))
                    "resource.edit rejects stale current content with actionable reread guidance")
                   (test-assert
                    (string= (workspace-file--read-content path)
                             (format nil "external~%value~%"))
                    "stale revision rejection preserves externally changed content")))
               (let ((expired
                       (edit-resource
                        first-context "workspace:stale.txt" "Rexpired"
                        (list
                         (workspace-resource-tests--operation
                          "replace-lines"
                          "start-line" 1
                          "end-line" 1
                          "content" "new")))))
                 (test-assert
                  (and (not (tool-result-success-p expired))
                       (search "expired" (tool-result-content expired)))
                  "unrecorded or expired revision aliases require a reread")))
             (let* ((crlf (format nil "~C~C" #\Return #\Newline))
                    (path (merge-pathnames "crlf.txt" workspace))
                    (original (format nil "one~Atwo~A" crlf crlf)))
               (workspace-resource-tests--write-text path original)
               (sb-posix:chmod (namestring path) #o640)
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:crlf.txt")
                 (declare (ignore read-result))
                 (test-assert
                  (tool-result-success-p
                   (edit-resource
                    first-context uri revision
                    (list
                     (workspace-resource-tests--operation
                      "replace-lines"
                      "start-line" 2
                      "end-line" 2
                      "content" "TWO"))))
                  "resource.edit rewrites CRLF files")
                 (test-assert
                  (string= (workspace-file--read-content path)
                           (format nil "one~ATWO~A" crlf crlf))
                  "resource.edit preserves CRLF and the final newline")
                 (test-assert (= (logand #o777
                                         (sb-posix:stat-mode
                                          (sb-posix:stat (namestring path))))
                                 #o640)
                              "resource.edit preserves file permissions where practical"))
             (let* ((crlf (format nil "~C~C" #\Return #\Newline))
                    (path (merge-pathnames "mixed-line-endings.txt" workspace))
                    (original (format nil "one~%two~Athree~%" crlf)))
               (workspace-resource-tests--write-text path original)
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context
                                  "workspace:mixed-line-endings.txt")
                 (declare (ignore read-result))
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-lines" "start-line" 2 "end-line" 2
                            "content" "TWO")))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "mixed LF and CRLF"
                                 (tool-result-content result)))
                    "resource.edit refuses to normalize untouched mixed line endings")
                   (test-assert
                    (string= (workspace-file--read-content path) original)
                    "mixed-line-ending refusal preserves exact content"))))
             (let ((path (merge-pathnames "no-final-newline.txt" workspace)))
               (workspace-resource-tests--write-text path (format nil "one~%two"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:no-final-newline.txt")
                 (declare (ignore read-result))
                 (test-assert
                  (tool-result-success-p
                   (edit-resource
                    first-context uri revision
                    (list
                     (workspace-resource-tests--operation
                      "insert-after" "line" 1 "content" "middle"))))
                  "resource.edit rewrites files without final newlines")
                 (test-assert
                  (string= (workspace-file--read-content path)
                           (format nil "one~%middle~%two"))
                  "resource.edit preserves absence of a final newline")))
             (let ((path (merge-pathnames "oversized-edit.txt" workspace)))
               (workspace-resource-tests--write-text path (format nil "a~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:oversized-edit.txt")
                 (declare (ignore read-result))
                 (let ((publish-called-p nil)
                       (*workspace-file-resource-maximum-bytes* 6)
                       (*workspace-file-resource-publish-function*
                         (lambda (source target)
                           (declare (ignore source target))
                           (setf publish-called-p t))))
                   (let ((result
                           (edit-resource
                            first-context uri revision
                            (list
                             (workspace-resource-tests--operation
                              "replace-lines"
                              "start-line" 1
                              "end-line" 1
                              "content"
                              (make-string 3
                                           :initial-element (code-char #xE9)))))))
                     (test-assert
                      (and (not (tool-result-success-p result))
                           (search "UTF-8 bytes" (tool-result-content result)))
                      "resource.edit preflights the complete UTF-8 replacement size")
                     (test-assert
                      (and (not publish-called-p)
                           (string= (workspace-file--read-content path)
                                    (format nil "a~%"))
                           (null
                            (directory
                             (merge-pathnames
                              ".*.autolith-resource-*.tmp" workspace))))
                      "oversized replacement rejection writes nothing and preserves the original")))))
             (let ((path (merge-pathnames "publish-failure.txt" workspace)))
               (workspace-resource-tests--write-text path (format nil "before~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:publish-failure.txt")
                 (declare (ignore read-result))
                 (let ((*workspace-file-resource-publish-function*
                         (lambda (source target)
                           (declare (ignore source target))
                           (error "simulated publish failure"))))
                   (test-assert
                    (not (tool-result-success-p
                          (edit-resource
                           first-context uri revision
                           (list
                            (workspace-resource-tests--operation
                             "replace-lines"
                             "start-line" 1
                             "end-line" 1
                             "content" "after")))))
                    "resource.edit reports a failed atomic publish"))
                 (test-assert
                  (string= (workspace-file--read-content path)
                           (format nil "before~%"))
                  "failed atomic publication leaves original content intact")
                 (test-assert
                  (null (directory (merge-pathnames ".*.autolith-resource-*.tmp"
                                                    workspace)))
                  "failed atomic publication cleans its same-directory temporary file")))
             (let* ((path (merge-pathnames "serialized.txt" workspace))
                    (gate (make-lock "workspace resource serialization test"))
                    (condition (make-condition-variable))
                    (publish-entered-p nil)
                    (release-publish-p nil)
                    (resource-result nil)
                    (write-result nil)
                    (resource-thread nil)
                    (write-thread nil)
                    (original-publish *workspace-file-resource-publish-function*))
               (workspace-resource-tests--write-text path (format nil "before~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:serialized.txt")
                 (declare (ignore read-result))
                 (unwind-protect
                      (progn
                        (setf *workspace-file-resource-publish-function*
                              (lambda (source target)
                                (with-lock-held (gate)
                                  (setf publish-entered-p t)
                                  (condition-notify condition)
                                  (loop until release-publish-p
                                        do (condition-wait condition gate)))
                                (funcall original-publish source target))
                              resource-thread
                              (make-thread
                               (lambda ()
                                 (setf resource-result
                                       (edit-resource
                                        first-context uri revision
                                        (list
                                         (workspace-resource-tests--operation
                                          "replace-lines"
                                          "start-line" 1
                                          "end-line" 1
                                          "content" "resource")))))
                               :name "workspace resource serialized edit"))
                        (with-lock-held (gate)
                          (loop until publish-entered-p
                                unless (condition-wait condition gate :timeout 2)
                                  do (error "Timed out waiting for resource publication.")))
                        (setf write-thread
                              (make-thread
                               (lambda ()
                                 (setf write-result
                                       (call second-context "fs" "write"
                                             "path" (namestring path)
                                             "content" (format nil "fs~%"))))
                               :name "workspace serialized fs write"))
                        (sleep 0.05)
                        (test-assert (null write-result)
                                     "native fs writes wait for revision-gated publication")
                        (with-lock-held (gate)
                          (setf release-publish-p t)
                          (condition-notify condition))
                        (join-thread resource-thread)
                        (join-thread write-thread)
                        (test-assert
                         (and (tool-result-success-p resource-result)
                              (tool-result-success-p write-result)
                              (string= (workspace-file--read-content path)
                                       (format nil "fs~%")))
                         "serialized native mutations do not lose a concurrent fs write"))
                   (setf *workspace-file-resource-publish-function* original-publish)
                   (with-lock-held (gate)
                     (setf release-publish-p t)
                     (condition-notify condition))
                   (when (and resource-thread (thread-alive-p resource-thread))
                     (join-thread resource-thread))
                   (when (and write-thread (thread-alive-p write-thread))
                     (join-thread write-thread)))))
             (let* ((path (merge-pathnames "serialized-read.txt" workspace))
                    (coordination
                      (make-instance 'workspace-resource-tests-coordination))
                    (resource-registry
                      (tool-registry-resource-registry registry))
                    (previous-resolver
                      (gethash "workspace"
                               (resource-registry-resolvers resource-registry)))
                    (read-result nil)
                    (write-result nil)
                    (write-started-p nil)
                    (read-thread nil)
                    (write-thread nil))
               (workspace-resource-tests--write-text path (format nil "before~%"))
               (unwind-protect
                    (progn
                      (resource-registry-register
                       resource-registry
                       (make-instance
                        'workspace-resource-tests-coordinated-resolver
                        :scheme       "workspace"
                        :coordination coordination))
                      (with-recursive-lock-held
                          ((conversation-resource-observation-lock
                            first-conversation))
                        (setf read-thread
                              (make-thread
                               (lambda ()
                                 (setf read-result
                                       (read-resource
                                        first-context
                                        "workspace:serialized-read.txt")))
                               :name "serialized resource read"))
                        (with-lock-held
                            ((workspace-resource-tests-coordination-lock
                              coordination))
                          (loop until
                                (workspace-resource-tests-coordination-observed-p
                                 coordination)
                                unless
                                (condition-wait
                                 (workspace-resource-tests-coordination-condition
                                  coordination)
                                 (workspace-resource-tests-coordination-lock
                                  coordination)
                                 :timeout 2)
                                  do (error
                                      "Timed out waiting for resource observation.")))
                        (setf write-thread
                              (make-thread
                               (lambda ()
                                 (with-lock-held
                                     ((workspace-resource-tests-coordination-lock
                                       coordination))
                                   (setf write-started-p t)
                                   (condition-notify
                                    (workspace-resource-tests-coordination-condition
                                     coordination)))
                                 (setf write-result
                                       (call second-context "fs" "write"
                                             "path" (namestring path)
                                             "content" (format nil "fs~%"))))
                               :name "serialized read fs write"))
                        (with-lock-held
                            ((workspace-resource-tests-coordination-lock
                              coordination))
                          (loop until write-started-p
                                unless
                                (condition-wait
                                 (workspace-resource-tests-coordination-condition
                                  coordination)
                                 (workspace-resource-tests-coordination-lock
                                  coordination)
                                 :timeout 2)
                                  do (error
                                      "Timed out waiting for concurrent fs.write.")))
                        (sleep 0.05)
                        (test-assert
                         (null write-result)
                         "fs.write waits while resource.read installs observation state"))
                      (join-thread read-thread)
                      (join-thread write-thread)
                      (let ((revision
                              (workspace-resource-tests--field
                               (tool-result-content read-result) "Revision: ")))
                        (test-assert
                         (and (tool-result-success-p read-result)
                              (tool-result-success-p write-result)
                              (with-recursive-lock-held
                                  ((conversation-resource-observation-lock
                                    first-conversation))
                                (workspace-file--find-observation-state
                                 first-conversation
                                 "workspace:serialized-read.txt"
                                 revision))
                              (string= (workspace-file--read-content path)
                                       (format nil "fs~%")))
                         "resource.read installs its exact observation before fs.write proceeds")))
                 (resource-registry-register resource-registry previous-resolver)
                 (when (and read-thread (thread-alive-p read-thread))
                   (join-thread read-thread))
                 (when (and write-thread (thread-alive-p write-thread))
                   (join-thread write-thread))))
             (let* ((protected-root (merge-pathnames "protected-source/" root))
                    (recovery-path (merge-pathnames "recovery/core.bin"
                                                    protected-root))
                    (protected-configuration nil)
                    (protected-conversation nil)
                    (protected-context nil))
               (workspace-resource-tests--write-text recovery-path "stable")
               (setf protected-configuration
                     (test-configuration-for-source-root protected-root)
                     protected-conversation
                     (conversation-create protected-configuration
                                          :identifier "resource-protected")
                     protected-context
                     (make-instance 'tool-context
                                    :configuration protected-configuration
                                    :worker nil
                                    :conversation protected-conversation))
               (multiple-value-bind (read-result uri revision)
                   (read-resource protected-context "workspace:recovery/core.bin")
                 (declare (ignore read-result))
                 (let ((result
                         (edit-resource
                          protected-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-lines"
                            "start-line" 1
                            "end-line" 1
                            "content" "changed")))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "stable launcher or recovery artifact"
                                 (tool-result-content result)))
                    "resource.edit preserves protected workspace write policy")
                   (test-assert
                    (string= (workspace-file--read-content recovery-path) "stable")
                    "protected resource edits leave stable artifacts unchanged"))))
             (let* ((schemas (tool-registry-provider-schemas registry))
                    (resource-namespace
                      (find "resource" schemas
                            :key (lambda (schema) (json-get schema "name"))
                            :test #'string=))
                    (tools (and resource-namespace
                                (json-get resource-namespace "tools")))
                    (read-schema
                      (and tools
                           (find "read" tools
                                 :key (lambda (schema) (json-get schema "name"))
                                 :test #'string=)))
                    (edit-schema
                      (and tools
                           (find "edit" tools
                                 :key (lambda (schema) (json-get schema "name"))
                                 :test #'string=))))
               (test-assert (and resource-namespace (= (length tools) 2))
                            "provider schemas expose the resource namespace and both tools")
               (test-assert
                (equalp (json-get (json-get read-schema "parameters") "required")
                        #( "uri"))
                "resource.read schema requires the URI")
               (test-assert
                (equalp (json-get (json-get edit-schema "parameters") "required")
                        #( "uri" "base-revision" "operations"))
                "resource.edit schema requires URI, base revision, and operations")
               (let* ((properties
                        (json-get (json-get edit-schema "parameters")
                                  "properties"))
                      (operation-items
                        (json-get (json-get properties "operations") "items"))
                      (variants (json-get operation-items "oneOf")))
                 (test-assert (= (length variants) 11)
                              "resource.edit schema exposes workspace, agenda, and memory variants")
                 (test-assert
                  (every (lambda (variant)
                           (and (json-get variant "required")
                                (eq (json-get variant "additionalProperties") false)))
                         variants)
                  "every resource edit operation schema requires its fields and rejects extras")))
             (let ((path (merge-pathnames "fs-compatible.txt" workspace)))
               (workspace-resource-tests--write-text path "old")
               (test-assert
                (tool-result-success-p
                 (call first-context "fs" "edit"
                       "path" (namestring path)
                       "old-text" "old"
                       "new-text" "new"))
                "existing fs.edit remains available as a compatibility fallback")
               (test-assert
                (tool-result-success-p
                 (call first-context "fs" "write"
                       "path" (namestring path)
                       "content" "complete"))
                "existing fs.write remains available for complete replacement")))))
      (tool-registry-close-runtime-state registry)
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)
