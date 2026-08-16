(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test--write-sparse-lisp-core (pathname) pathname)
(defun test--write-sparse-lisp-core (pathname)
  "Write a sparse file large enough to pass saved-core shape validation."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :element-type '(unsigned-byte 8))
    (file-position stream (minimum-lisp-image-core-size))
    (write-byte 0 stream))
  pathname)

(-> test-lisp-image-manifests () null)
(defun test-lisp-image-manifests ()
  "Test immutable saved worker-image manifests, notes, and compatibility."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (identifier "instrumented-compiler")
         (directory (lisp-image--directory configuration identifier))
         (core (merge-pathnames "worker.core" directory)))
    (unwind-protect
         (progn
           (test--write-sparse-lisp-core core)
           (let ((image
                   (lisp-image-publish-manifest
                    configuration
                    :identifier identifier
                    :parent-identifier (pristine-lisp-image-identifier)
                    :note "Traces compiler type derivation for comparison."
                    :core-pathname core
                    :source-commit "0123456789abcdef")))
             (test-assert (string= (lisp-image-identifier image) identifier)
                          "saved Lisp images retain their identifier")
             (test-assert
              (string= (lisp-image-note image)
                       "Traces compiler type derivation for comparison.")
              "saved Lisp images retain their durable note")
             (test-assert (lisp-image-compatible-p image)
                          "a manifest written by this runtime is compatible")
             (test-assert
             (search "instrumented-compiler"
                      (lisp-image-render-inventory configuration))
              "the image inventory reminds the model about saved images")
             (test-assert
              (search "Traces compiler type derivation"
                      (lisp-image-prompt-notes configuration))
              "the prompt inventory includes durable image notes"))
           (handler-case
               (progn
                 (lisp-image-publish-manifest
                  configuration
                  :identifier identifier
                  :parent-identifier (pristine-lisp-image-identifier)
                  :note "A duplicate image."
                  :core-pathname core)
                 (test-assert nil "saved image identifiers are immutable"))
             (lisp-image-error ()
               (test-assert t "saved image identifiers are immutable")))
           (handler-case
               (progn
                 (lisp-image--validate-identifier "pristine")
                 (test-assert nil "the pristine image name is reserved"))
             (lisp-image-error ()
               (test-assert t "the pristine image name is reserved"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-lisp-worker-protocol () null)
(defun test-lisp-worker-protocol ()
  "Test portable worker request execution and condition reporting."
  (let ((success
          (worker-handle-request
           '(:request :id 1 :operation :eval :arguments (:form "(+ 20 22)"))))
        (failure
          (worker-handle-request
           '(:request :id 2 :operation :eval :arguments (:form "(/ 1 0)")))))
    (test-assert (eq (getf (rest success) :status) :ok)
                 "the worker evaluates a valid request")
    (test-assert (equal (getf (rest success) :values) '("42"))
                 "the worker returns rendered values")
    (test-assert (eq (getf (rest failure) :status) :error)
                 "the worker turns evaluation conditions into protocol errors")
    (test-assert (non-empty-string-p (getf (rest failure) :message))
                 "worker protocol errors carry a readable condition report"))
  (let ((source
          (worker-handle-request
           '(:request :id 3 :operation :source
             :arguments (:name "CL:MAPCAR" :kind "function")))))
    (test-assert (eq (getf (rest source) :status) :ok)
                 "the worker resolves implementation definition source")
    (test-assert (search "src/code/list.lisp"
                         (getf (rest source) :output))
                 "implementation source comes from the exact managed tree")
    (test-assert (search "(define-list-map mapcar"
                         (getf (rest source) :output)
                         :test #'char-equal)
                 "implementation source includes the complete recorded form"))
  (let ((previous-command (uiop:getenv "AUTOLITH_SBCL")))
    (unwind-protect
         (progn
           (sb-posix:setenv "AUTOLITH_SBCL" "/tmp/autolith-test-sbcl" 1)
           (test-assert (string= (lisp-worker-sbcl-command)
                                 "/tmp/autolith-test-sbcl")
                        "the disposable worker honors the configured SBCL")
           (sb-posix:setenv "AUTOLITH_SBCL" "" 1)
           (test-assert (string= (lisp-worker-sbcl-command) "sbcl")
                        "the disposable worker falls back to PATH"))
      (if previous-command
          (sb-posix:setenv "AUTOLITH_SBCL" previous-command 1)
          (sb-posix:unsetenv "AUTOLITH_SBCL"))))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (worker (lisp-worker-create configuration)))
    (unwind-protect
         (let ((evaluation
                 (lisp-worker-request worker :eval '(:form "(+ 40 2)")))
               (source
                 (lisp-worker-request
                  worker
                  :source
                  '(:name "CL:MAPCAR" :kind "function"))))
           (test-assert (eq (getf (rest evaluation) :status) :ok)
                        "the named worker starts through its direct active loader")
           (test-assert (equal (getf (rest evaluation) :values) '("42"))
                        "the launched worker completes its isolated protocol request")
           (test-assert
            (and (eq (getf (rest source) :status) :ok)
                 (search "src/code/list.lisp" (getf (rest source) :output)))
            "a launched worker can read its matching implementation source"))
      (lisp-worker-stop worker)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  (let* ((source-root (asdf:system-source-directory :autolith))
         (launcher (merge-pathnames "bin/autolith" source-root))
         (output
           (with-input-from-string
               (input
                (format nil
                        "(:request :id 7 :operation :source :arguments (:name ~
                         \"CL:MAPCAR\" :kind \"function\"))~%"))
             (uiop:run-program
              (list "env"
                    "-u"
                    "AUTOLITH_SBCL_SOURCE_ROOT"
                    (namestring launcher)
                    "--worker")
              :input input
              :output ':string
              :error-output *error-output*))))
    (let ((*read-eval* nil))
      (with-input-from-string (stream output)
        (let ((handshake (read stream t nil))
              (response (read stream t nil)))
          (test-assert (and (eq (first handshake) :autolith-worker)
                            (eq (getf (rest response) :status) :ok)
                            (search "src/code/list.lisp"
                                    (getf (rest response) :output)))
                       "the stable launcher exports matching source to workers")))))
  (let* ((source-root (asdf:system-source-directory :autolith))
         (launcher (merge-pathnames "bin/autolith" source-root))
         (runtime-source (uiop:getenv "AUTOLITH_SBCL_SOURCE_ROOT"))
         (temporary-root
           (merge-pathnames
            (format nil "autolith-inherited-source-~A/" (make-identifier))
            (uiop:temporary-directory)))
         (data-home (merge-pathnames "data/" temporary-root))
         (state-home (merge-pathnames "state/" temporary-root)))
    (unwind-protect
         (progn
           (unless (non-empty-string-p runtime-source)
             (error "The test runtime has no matching SBCL source root."))
           (ensure-directories-exist data-home)
           (ensure-directories-exist state-home)
           (let ((output
                   (with-input-from-string
                       (input
                        (format nil
                                "(:request :id 8 :operation :source :arguments ~
                                 (:name \"CL:MAPCAR\" :kind \"function\"))~%"))
                     (uiop:run-program
                      (list "env"
                            (format nil "XDG_DATA_HOME=~A" data-home)
                            (format nil "XDG_STATE_HOME=~A" state-home)
                            (format nil "AUTOLITH_SBCL_SOURCE_ROOT=~A"
                                    runtime-source)
                            (namestring launcher)
                            "--worker")
                      :input input
                      :output ':string
                      :error-output *error-output*))))
             (let ((*read-eval* nil))
               (with-input-from-string (stream output)
                 (let ((handshake (read stream t nil))
                       (response (read stream t nil)))
                   (test-assert
                    (and (eq (first handshake) :autolith-worker)
                         (eq (getf (rest response) :status) :ok)
                         (search "src/code/list.lisp"
                                 (getf (rest response) :output)))
                    "the stable launcher preserves inherited matching source"))))))
      (uiop:delete-directory-tree temporary-root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pool (lisp-worker-pool-create configuration)))
    (unwind-protect
         (let* ((alpha (lisp-worker-pool-start pool "alpha" "pristine"))
                (beta (lisp-worker-pool-start pool "beta" "pristine")))
           (lisp-worker-request alpha :eval '(:form "(defparameter *pool-value* 41)"))
           (let ((alpha-result
                   (lisp-worker-request alpha :eval '(:form "(1+ *pool-value*)")))
                 (beta-result
                   (lisp-worker-request beta :eval '(:form "(boundp '*pool-value*)"))))
             (test-assert (equal (getf (rest alpha-result) :values) '("42"))
                          "one named REPL retains its own heap state")
             (test-assert (equal (getf (rest beta-result) :values) '("NIL"))
                          "named REPLs do not share heap state"))
           (let* ((registry (make-default-tool-registry))
                  (conversation
                    (conversation-create configuration :identifier "repl-routing"))
                  (context (make-instance 'tool-context
                                          :configuration configuration
                                          :worker pool
                                          :conversation conversation))
                  (result
                    (tool-execute
                     (tool-registry-find registry "lisp" "eval")
                     context
                     (json-object "form" "(1+ *pool-value*)"
                                  "repl" "alpha"))))
             (test-assert (and (tool-result-success-p result)
                               (search "42" (tool-result-content result)))
                          "lisp.eval routes requests to the named REPL"))
           (test-assert (search "alpha  running  image pristine"
                                (lisp-worker-pool-render pool))
                        "the worker pool lists each active REPL and image")
           (let* ((workspace (merge-pathnames "moved-workspace/" root))
                  (moved-configuration nil))
             (ensure-directories-exist workspace)
             (setf moved-configuration
                   (configuration-with-working-directory configuration workspace))
             (lisp-worker-pool-change-working-directory pool moved-configuration)
             (let ((marker
                     (lisp-worker-request alpha :eval
                                          '(:form "(1+ *pool-value*)")))
                   (worker-directory
                     (lisp-worker-request
                      alpha :eval '(:form "(namestring (uiop:getcwd))")))
                   (default-directory
                     (lisp-worker-request
                      alpha :eval
                      '(:form "(namestring *default-pathname-defaults*)"))))
               (test-assert (equal (getf (rest marker) :values) '("42"))
                            "moving a REPL preserves its heap state")
               (test-assert
                (search (namestring workspace)
                        (first (getf (rest worker-directory) :values)))
                "moving a REPL changes its process working directory")
               (test-assert
                (search (namestring workspace)
                        (first (getf (rest default-directory) :values)))
                "moving a REPL changes its pathname defaults"))
             (let* ((gamma (lisp-worker-pool-start pool "gamma" "pristine"))
                    (gamma-directory
                      (lisp-worker-request
                       gamma :eval '(:form "(namestring (uiop:getcwd))"))))
               (test-assert
                (search (namestring workspace)
                        (first (getf (rest gamma-directory) :values)))
                "new REPLs start in the moved pool workspace"))
             (lisp-worker-pool-change-working-directory pool configuration))
           (let ((invalid-configuration
                   (configuration--clone
                    configuration
                    :working-directory (merge-pathnames "missing/" root))))
             (test-assert
              (handler-case
                  (progn
                    (lisp-worker-pool-change-working-directory
                     pool invalid-configuration)
                    nil)
                (worker-error ()
                  t))
              "a failed REPL workspace change reports a worker error")
             (test-assert
              (equal (lisp-worker-pool-configuration pool) configuration)
              "a failed REPL workspace change retains the pool configuration")
             (let ((marker
                     (lisp-worker-request alpha :eval
                                          '(:form "(1+ *pool-value*)"))))
               (test-assert (equal (getf (rest marker) :values) '("42"))
                            "a failed REPL workspace change preserves heap state")))
           (handler-case
               (progn
                 (lisp-worker-pool-start pool "alpha" "another-image")
                 (test-assert nil
                              "an existing REPL never switches images implicitly"))
             (worker-error ()
               (test-assert t
                            "an existing REPL never switches images implicitly")))
           (lisp-worker-pool-reset pool "alpha" "pristine")
           (let ((result
                   (lisp-worker-request
                    (lisp-worker-pool-worker pool "alpha")
                    :eval
                    '(:form "(boundp '*pool-value*)"))))
             (test-assert (equal (getf (rest result) :values) '("NIL"))
                          "reset replaces only the selected REPL heap"))
           (lisp-worker-pool-stop pool "beta")
           (test-assert (not (search "beta" (lisp-worker-pool-render pool)))
                        "stopping one REPL leaves it out of the pool"))
      (lisp-worker-pool-stop-all pool)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-lisp-execution-jobs () null)
(defun test-lisp-execution-jobs ()
  "Test inspectable Lisp jobs, named REPL affinity, cancellation, and restart."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pool (lisp-worker-pool-create configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (run-tool (tool-registry-find registry "task" "run"))
         (orchestrator (task-run-tool-orchestrator run-tool))
         (primary
           (task-tests--primary-agent
            configuration "lisp-execution-primary" registry))
         (context
           (make-instance
            'tool-context
            :configuration configuration
            :worker pool
            :conversation (agent-conversation primary)
            :registry registry
            :agent primary
            :call-id "lisp-execution-test"))
         (cancel-marker (merge-pathnames "lisp-cancel-started" root))
         (alpha nil)
         (beta nil)
         (cancel-worker nil)
         (scratch-worker nil))
    (labels ((run-lisp (name &rest arguments)
               "Execute lisp.NAME with decoded test ARGUMENTS."
               (tool-execute
                (tool-registry-find registry "lisp" name)
                context
                (apply #'json-object arguments)))

             (handoff-job (result)
               "Return RESULT's visible handed-off execution job."
               (let* ((details (tool-result-details result))
                      (record (and (listp details)
                                   (getf (rest details) :job)))
                      (identifier (and record (getf record :id))))
                 (and identifier
                      (task-orchestrator-find-visible-job
                       orchestrator identifier primary "lisp.eval"))))

             (worker-values (worker form)
               "Evaluate FORM directly in WORKER and return rendered values."
               (getf
                (rest
                 (lisp-worker-request worker :eval (list :form form)))
                :values))

             (async-schema-p (name)
               "Return true when lisp.NAME advertises one async Boolean."
               (let* ((tool (tool-registry-find registry "lisp" name))
                      (properties
                        (and tool
                             (json-get (tool-parameters tool) "properties")))
                      (property
                        (and properties (gethash "async" properties))))
                 (and (json-object-p property)
                      (string= (json-get property "type") "boolean")))))
      (unwind-protect
           (progn
             (setf alpha (lisp-worker-pool-start pool "alpha" "pristine")
                   beta (lisp-worker-pool-start pool "beta" "pristine")
                   cancel-worker
                   (lisp-worker-pool-start pool "cancel" "pristine")
                   scratch-worker
                   (lisp-worker-pool-start pool "scratch-async" "pristine"))
              (test-assert
               (every #'async-schema-p
                      '("eval" "load-system" "run-tests" "scratchpad-run"))
               "only execution-oriented Lisp tools advertise asynchronous jobs")
              (test-assert
               (notany #'async-schema-p
                       '("describe" "source" "reset" "start" "stop" "repls"
                         "images" "save-image"))
               "Lisp inspection and lifecycle tools stay synchronous")
              (let* ((eval-tool (tool-registry-find registry "lisp" "eval"))
                     (properties (json-get (tool-parameters eval-tool) "properties"))
                     (compile-property (json-get properties "compile")))
                (test-assert
                 (and (null (tool-registry-find registry "lisp" "compile"))
                      (json-object-p compile-property)
                      (string= (json-get compile-property "type") "boolean"))
                 "lisp.eval owns compilation mode and lisp.compile is absent"))
             (let* ((*tool-execution-blocking-grace-seconds* 5)
                    (result
                      (run-lisp "eval"
                                "form" "(+ 20 22)"
                                "repl" "alpha")))
               (test-assert
                (and (tool-result-success-p result)
                     (not (typep result 'task-tool-result))
                     (search "42" (tool-result-content result)))
                "a fast default Lisp evaluation returns its ordinary result"))
             (let* ((*tool-execution-blocking-grace-seconds* 0.01)
                    (result
                      (run-lisp
                       "eval"
                       "form"
                       "(progn (defparameter *async-once* (1+ (if (boundp '*async-once*) *async-once* 0))) (sleep 1) *async-once*)"
                       "repl" "alpha"))
                    (details (tool-result-details result))
                    (job (handoff-job result))
                    (identifier (and job (session-job-identifier job))))
               (test-assert
                (and (typep result 'task-tool-result)
                     (eq (getf (rest details) :handoff-reason)
                         :grace-expired)
                     job
                     (session-job-detached-p job))
                "a slow default Lisp evaluation hands off its existing job")
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job 5)
                 (test-assert
                  (and terminal-p
                       (eq (getf snapshot :state) :completed)
                       (string= identifier (getf snapshot :job-id))
                       (equal (worker-values alpha "*async-once*") '("1"))
                       (equal (worker-values beta "(boundp '*async-once*)")
                              '("NIL")))
                  "the handed-off evaluation runs once in its selected named REPL")))
              (let* ((result
                       (run-lisp
                        "eval"
                        "form" "(progn (sleep 1) (+ 2 3))"
                        "compile" t
                        "repl" "beta"
                        "async" t))
                    (details (tool-result-details result))
                    (job (handoff-job result)))
               (test-assert
                (and (typep result 'task-tool-result)
                     (eq (getf (rest details) :handoff-reason) :requested)
                     job)
                "explicit async returns an inspectable Lisp compilation job")
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job 5)
                 (test-assert
                  (and terminal-p
                       (eq (getf snapshot :state) :completed)
                       (search "5" (getf (getf snapshot :result) :content)))
                  "the asynchronous Lisp compilation retains its result")))
             (let* ((form
                      (format nil
                              "(progn (with-open-file (stream ~A :direction :output :if-exists :supersede :if-does-not-exist :create) (write-string \"started\" stream)) (sleep 30) :done)"
                              (prin1-to-string cancel-marker)))
                    (result
                      (run-lisp "eval"
                                "form" form
                                "repl" "cancel"
                                "async" t))
                    (job (handoff-job result))
                    (identifier (and job (session-job-identifier job))))
               (test-assert
                (and job
                     (task-tests--wait-until
                      (lambda () (probe-file cancel-marker)) 5))
                "an asynchronous Lisp evaluation reaches its worker before cancellation")
               (let* ((cancel-result
                        (tool-execute
                         (tool-registry-find registry "job" "cancel")
                         context
                         (json-object "id" identifier)))
                      (details (rest (tool-result-details cancel-result))))
                 (test-assert
                  (and (tool-result-success-p cancel-result)
                       (getf details :accepted-p))
                  "job.cancel accepts cancellation of a running Lisp request"))
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job 5)
                 (test-assert
                  (and terminal-p
                       (eq (getf snapshot :state) :aborted)
                       (eq (getf (getf snapshot :result) :status) :aborted)
                       (not (lisp-worker-running-p cancel-worker)))
                  "cancelling a Lisp job aborts it and stops the interrupted REPL")))
             (let* ((*tool-execution-blocking-grace-seconds* 5)
                    (result
                      (run-lisp "eval"
                                "form" "(+ 40 2)"
                                "repl" "cancel")))
               (test-assert
                (and (tool-result-success-p result)
                     (search "42" (tool-result-content result))
                     (lisp-worker-running-p cancel-worker)
                     (eq cancel-worker
                         (lisp-worker-pool-worker pool "cancel")))
                "the cancelled named REPL restarts safely for its next request"))
             (let ((load-result
                     (let ((*tool-execution-blocking-grace-seconds* 5))
                       (run-lisp "load-system"
                                 "system" "asdf"
                                 "repl" "alpha"))))
               (test-assert
                (tool-result-success-p load-result)
                "lisp.load-system uses the shared execution path"))
              (let* ((read-result
                       (workspace-resource-tests--call
                        registry context "resource" "read"
                        "uri" "scratchpad:async-program.lisp"))
                     (revision
                       (workspace-resource-tests--field
                        (tool-result-content read-result) "Revision: "))
                     (write-result
                       (workspace-resource-tests--call
                        registry context "resource" "edit"
                        "uri" "scratchpad:async-program.lisp"
                        "base-revision" revision
                        "operations"
                        (vector
                         (workspace-resource-tests--operation
                          "replace-empty"
                          "content"
                          (format nil
                                  "(defparameter *async-scratchpad-value* 41)~%~
                                   (sleep 1)~%~
                                   (incf *async-scratchpad-value*)~%")))))
                    (run-result
                      (run-lisp "scratchpad-run"
                                "path" "async-program.lisp"
                                "repl" "scratch-async"
                                "async" t))
                    (job (handoff-job run-result)))
               (test-assert
                (and (tool-result-success-p write-result)
                     (typep run-result 'task-tool-result)
                     job)
                "lisp.scratchpad-run returns an inspectable execution job")
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job 5)
                 (test-assert
                  (and terminal-p
                       (eq (getf snapshot :state) :completed)
                       (equal
                        (worker-values
                         scratch-worker "*async-scratchpad-value*")
                        '("42")))
                  "the asynchronous scratchpad loads once into its selected REPL"))))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (ignore-errors (lisp-worker-pool-stop-all pool))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)


(-> test-lisp-scratchpad-tools () null)
(defun test-lisp-scratchpad-tools ()
  "Test conversation-scoped scratchpad resources, execution, and deletion."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (pool          (lisp-worker-pool-create configuration))
         (registry      (make-default-tool-registry))
         (conversation  (conversation-create configuration))
         (other         (conversation-create configuration))
         (context       (make-instance 'tool-context
                                       :configuration configuration
                                       :worker        pool
                                       :conversation  conversation))
         (other-context (make-instance 'tool-context
                                       :configuration configuration
                                       :worker        pool
                                       :conversation  other)))
    (labels ((call-resource (authority name &rest arguments)
               "Execute resource.NAME with ARGUMENTS under AUTHORITY."
               (apply #'workspace-resource-tests--call
                      registry authority "resource" name arguments))

             (read-resource (authority uri)
               "Read URI under AUTHORITY."
               (call-resource authority "read" "uri" uri))

             (observation-field (result label)
               "Return LABEL from successful resource RESULT."
               (workspace-resource-tests--field
                (tool-result-content result) label))

             (edit-resource (authority result &rest operations)
               "Edit the exact resource observation in RESULT with OPERATIONS."
               (call-resource
                authority "edit"
                "uri" (observation-field result "URI: ")
                "base-revision" (observation-field result "Revision: ")
                "operations" (coerce operations 'vector)))

             (create-resource (authority uri content)
               "Create missing scratchpad URI with exact CONTENT."
               (edit-resource
                authority
                (read-resource authority uri)
                (workspace-resource-tests--operation
                 "replace-empty" "content" content))))
      (unwind-protect
           (progn
             (lisp-worker-pool-start pool "scratch" "pristine")
             (test-assert
              (and (tool-registry-find registry "lisp" "scratchpad-run")
                   (every
                    #'null
                    (mapcar
                     (lambda (name)
                       (tool-registry-find registry "lisp" name))
                     '("scratchpad-list" "scratchpad-read" "scratchpad-write"
                       "scratchpad-edit" "scratchpad-delete"))))
              "scratchpad resources replace the five flat Lisp file tools")
             (let* ((other-root (read-resource other-context "scratchpad:."))
                    (replacement
                      (edit-resource
                       other-context other-root
                       (workspace-resource-tests--operation
                        "replace-empty" "content" "not a directory"))))
               (test-assert
                (and (tool-result-success-p other-root)
                     (search "Kind: missing" (tool-result-content other-root))
                     (not (tool-result-success-p replacement))
                     (eq (workspace-file--path-kind
                          (lisp-scratchpad-root other-context))
                         ':missing))
                "scratchpad:. observes a missing root without creating it as a file"))
             (let ((result
                     (create-resource
                      context
                      "scratchpad:program.lisp"
                      (format nil
                              "(defparameter *scratchpad-value* 40)~%~
                               (incf *scratchpad-value* 2)~%"))))
               (test-assert
                (and (tool-result-success-p result)
                     (probe-file (merge-pathnames
                                  "program.lisp"
                                  (lisp-scratchpad-root context))))
                "resource.edit creates a missing conversation scratchpad file"))
             (let* ((observed (read-resource context "scratchpad:program.lisp"))
                    (edited
                      (edit-resource
                       context observed
                       (workspace-resource-tests--operation
                        "replace-lines"
                        "start-line" 1
                        "end-line" 1
                        "content" "(defparameter *scratchpad-value* 41)"))))
               (test-assert
                (and (tool-result-success-p edited)
                     (search "*scratchpad-value* 41"
                             (tool-result-content edited)))
                "scratchpad resources inherit revision-gated original-line edits"))
             (let ((result
                     (create-resource context
                                      "scratchpad:broken.lisp"
                                      "(list 1")))
               (test-assert
                (and (tool-result-success-p result)
                     (search "delimiter checking found 1 unmatched"
                             (tool-result-content result)))
                "scratchpad resource edits preserve non-fatal delimiter warnings"))
             (create-resource context "scratchpad:utf8.txt" "λ café")
             (let ((result (read-resource context "scratchpad:utf8.txt")))
               (test-assert
                (and (tool-result-success-p result)
                     (search "λ café" (tool-result-content result)))
                "scratchpad resources decode exact UTF-8 content"))
             (create-resource context "scratchpad:oversized.txt" "123456789")
             (let* ((*workspace-file-resource-maximum-bytes* 8)
                    (result (read-resource context "scratchpad:oversized.txt")))
               (test-assert
                (and (not (tool-result-success-p result))
                     (search "up to 8 bytes"
                             (tool-result-content result)))
                "scratchpad resources reject files above the exact byte limit"))
             (let ((result
                     (tool-execute
                      (tool-registry-find registry "lisp" "scratchpad-run")
                      context
                      (json-object "path" "program.lisp"
                                   "repl" "scratch"))))
               (test-assert
                (tool-result-success-p result)
                "lisp.scratchpad-run loads a resource-created file"))
             (let ((result
                     (lisp-worker-request
                      (lisp-worker-pool-worker pool "scratch")
                      :eval
                      '(:form "*scratchpad-value*"))))
               (test-assert
                (equal (getf (rest result) :values) '("43"))
                "scratchpad execution retains definitions in the selected REPL"))
             (let ((result (read-resource other-context "scratchpad:.")))
               (test-assert
                (and (tool-result-success-p result)
                     (not (search "program.lisp" (tool-result-content result)))
                     (not (equal (lisp-scratchpad-root context)
                                 (lisp-scratchpad-root other-context))))
                "different conversations resolve isolated scratchpad roots"))
             (let* ((outside (merge-pathnames "outside/" root))
                    (escape (merge-pathnames "escape"
                                             (lisp-scratchpad-root context))))
               (ensure-directories-exist (merge-pathnames "secret.txt" outside))
               (with-open-file (stream (merge-pathnames "secret.txt" outside)
                                       :direction ':output
                                       :if-exists ':supersede
                                       :if-does-not-exist ':create
                                       :external-format ':utf-8)
                 (write-string "secret" stream))
               (sb-posix:symlink (namestring outside) (namestring escape))
               (test-assert
                (handler-case
                    (progn
                      (resource-registry-resolve
                       (tool-registry-resource-registry registry)
                       "scratchpad:escape/new.lisp"
                       context)
                      nil)
                  (tool-error ()
                    t))
                "scratchpad URI resolution rejects missing descendants through escaping symlinks")
               (delete-file escape))
             (let* ((observed (read-resource context "scratchpad:program.lisp"))
                    (path (merge-pathnames "program.lisp"
                                           (lisp-scratchpad-root context))))
               (with-open-file (stream path
                                       :direction ':output
                                       :if-exists ':append
                                       :external-format ':utf-8)
                 (write-string "; changed" stream))
               (let ((stale
                       (edit-resource
                        context observed
                        (workspace-resource-tests--operation
                         "scratchpad-delete"))))
                 (test-assert
                  (and (not (tool-result-success-p stale))
                       (search "stale" (string-downcase
                                        (tool-result-content stale)))
                       (probe-file path))
                  "scratchpad deletion rejects an exact stale file observation"))
               (let* ((fresh (read-resource context "scratchpad:program.lisp"))
                      (deleted
                        (edit-resource
                         context fresh
                         (workspace-resource-tests--operation
                          "scratchpad-delete"))))
                 (test-assert
                  (and (tool-result-success-p deleted)
                       (not (probe-file path)))
                  "scratchpad-delete removes an exactly observed file")))
             (create-resource context "scratchpad:delete-me/child.txt" "child")
             (let* ((directory (read-resource context "scratchpad:delete-me/"))
                    (deleted
                      (edit-resource
                       context directory
                       (workspace-resource-tests--operation
                        "scratchpad-delete"))))
               (test-assert
                (and (tool-result-success-p deleted)
                     (not (uiop:directory-exists-p
                           (merge-pathnames "delete-me/"
                                            (lisp-scratchpad-root context)))))
                "scratchpad-delete recursively removes an observed directory"))
             (create-resource context "scratchpad:crowded/a.txt" "a")
             (create-resource context "scratchpad:crowded/b.txt" "b")
             (let* ((*workspace-file-resource-maximum-directory-entries* 1)
                    (directory (read-resource context "scratchpad:crowded/"))
                    (rejected
                      (edit-resource
                       context directory
                       (workspace-resource-tests--operation
                        "scratchpad-delete"))))
               (test-assert
                (and (search "[directory listing truncated]"
                             (tool-result-content directory))
                     (not (tool-result-success-p rejected))
                     (uiop:directory-exists-p
                      (merge-pathnames "crowded/"
                                       (lisp-scratchpad-root context))))
                "scratchpad-delete refuses a truncated directory observation"))
             (let* ((scratchpad-root (read-resource context "scratchpad:."))
                    (deleted
                      (edit-resource
                       context scratchpad-root
                       (workspace-resource-tests--operation
                        "scratchpad-delete"))))
               (test-assert
                (and (tool-result-success-p deleted)
                     (not (uiop:directory-exists-p
                           (lisp-scratchpad-root context))))
                "scratchpad-delete clears the exactly observed conversation root")))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (ignore-errors (lisp-worker-pool-stop-all pool))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> test-lisp-worker-image-snapshot () null)
(defun test-lisp-worker-image-snapshot ()
  "Test saving a modified REPL core and starting an independent clone from it."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pool (lisp-worker-pool-create configuration)))
    (unwind-protect
         (let ((source (lisp-worker-pool-start pool "source" "pristine")))
           (lisp-worker-request
            source
            :eval
            '(:form "(defparameter *saved-worker-marker* 9001)"))
           (let ((image
                   (lisp-worker-save-image
                    configuration
                    source
                    :identifier "diddled"
                    :note
                    "Carries a marker proving the modified SBCL heap was retained.")))
             (test-assert
              (and (string= (lisp-image-identifier image) "diddled")
                   (lisp-image--plausible-core-p
                    (lisp-image-core-pathname image)))
              "saving a named REPL publishes a plausible immutable core")
             (test-assert (lisp-worker-running-p source)
                          "saving an image leaves the parent REPL running")
             (let* ((clone (lisp-worker-pool-start pool "clone" "diddled"))
                    (clone-result
                      (lisp-worker-request
                       clone
                       :eval
                       '(:form "*saved-worker-marker*")))
                    (pristine
                      (lisp-worker-pool-start pool "control" "pristine"))
                    (pristine-result
                      (lisp-worker-request
                       pristine
                       :eval
                       '(:form "(boundp '*saved-worker-marker*)"))))
               (test-assert
                (equal (getf (rest clone-result) :values) '("9001"))
                "a REPL started from the saved image inherits its modified heap")
               (test-assert
                (equal (getf (rest pristine-result) :values) '("NIL"))
                "a pristine comparison REPL excludes saved-image modifications")
               (test-assert
                (search "clone  running  image diddled"
                        (lisp-worker-pool-render pool))
                "the pool identifies which REPL uses the modified image"))))
      (lisp-worker-pool-stop-all pool)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)
