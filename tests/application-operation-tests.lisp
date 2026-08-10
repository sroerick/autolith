(in-package #:autolith)

;;;; -- User Operation Test Tool --

(defclass application-operation-test-tool (tool)
  ((calls
    :initform 0
    :accessor application-operation-test-tool-calls
    :type integer
    :documentation "The number of test executions observed.")
   (arguments
    :initform nil
    :accessor application-operation-test-tool-arguments
    :type t
    :documentation "The newest decoded argument object.")
   (context
    :initform nil
    :accessor application-operation-test-tool-context
    :type t
    :documentation "The newest local-user tool context."))
  (:documentation "A deterministic tool exercising the local operation boundary."))

(defmethod tool-execute
    ((tool application-operation-test-tool) (context tool-context) arguments)
  "Record one local operation execution and return its authorization decisions."
  (incf (application-operation-test-tool-calls tool))
  (setf (application-operation-test-tool-arguments tool) arguments
        (application-operation-test-tool-context tool) context)
  (tool-success
   (format nil
           "~(~A~) ~(~A~): ~A"
           (tool-context-authorize-command
            context "printf operation" (configuration-working-directory
                                         (tool-context-configuration context)))
           (tool-context-authorize-tool context tool arguments)
           (or (tool-argument arguments "text") "missing"))))


;;;; -- Operation Surface Tests --

(-> application-operation-tests--application
    ()
    (values application pathname recording-terminal application-operation-test-tool))
(defun application-operation-tests--application ()
  "Return one isolated application and its local operation test resources."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "operation-surface"))
         (registry (make-default-tool-registry))
         (tool
           (make-instance
            'application-operation-test-tool
            :namespace "test-operation"
            :name "echo"
            :description "Echo one local operation test value."
            :parameters
            (tool-object-schema
             (json-object
              "text" (tool-string-property "Text to echo.")
              "enabled" (json-object "type" "boolean")
              "items" (json-object "type" "array"))
             '("text"))))
         (terminal (make-instance 'recording-terminal :columns 100))
         (ui (terminal-ui-create :terminal terminal)))
    (tool-registry-register registry tool)
    (tool-registry-register
     registry
     (make-instance 'task-yield-tool
                    :namespace "yield"
                    :name "submit"
                    :description "Child-only test yield."
                    :parameters (tool-object-schema (json-object) nil)))
    (values
     (make-instance 'application
                    :configuration configuration
                    :conversation conversation
                    :provider nil
                    :tool-registry registry
                    :worker nil
                    :agent nil
                    :ui ui)
     root
     terminal
     tool)))

(-> run-application-operation-tests () boolean)
(defun run-application-operation-tests ()
  "Run focused unified command and tool operation tests."
  (multiple-value-bind (application root terminal tool)
      (application-operation-tests--application)
    (unwind-protect
         (let* ((operations (application-operation-list application))
                (names (mapcar #'application-operation-name operations)))
           (test-assert (member "help" names :test #'string=)
                        "interactive commands appear as canonical Lisp operations")
           (test-assert (member "fs.list" names :test #'string=)
                        "model tools appear in the same user operation registry")
           (test-assert (member "test-operation.echo" names :test #'string=)
                        "per-session tools appear in the local operation registry")
           (test-assert (not (member "yield.submit" names :test #'string=))
                        "the child-only yield operation stays hidden from users")
           (test-assert
            (eq (application-operation-kind
                 (application-operation-find application 'help))
                ':command)
            "operation lookup resolves command symbols case-insensitively")
           (test-assert
            (eq (application-operation-kind
                 (application-operation-find application "FS.LIST"))
                ':tool)
            "operation lookup resolves dotted tool names case-insensitively")
           (let ((command-authorizations 0)
                 (tool-authorizations 0))
             (test-call-with-function-replacements
              (list
               (list 'application-authorize-command
                     (lambda (observed command directory)
                       (test-assert (eq observed application)
                                    "local shell authority stays with the application")
                       (test-assert (string= command "printf operation")
                                    "local tools preserve exact shell authorization text")
                       (test-assert
                        (equal directory
                               (configuration-working-directory
                                (application-configuration application)))
                        "local tools preserve the authorized working directory")
                       (incf command-authorizations)
                       ':sandboxed))
               (list 'application-authorize-tool
                     (lambda (observed observed-tool arguments)
                       (test-assert (eq observed application)
                                    "local external-tool authority stays with the application")
                       (test-assert (eq observed-tool tool)
                                    "local authorization receives the authoritative tool object")
                       (test-assert (json-object-p arguments)
                                    "local authorization receives decoded JSON arguments")
                       (incf tool-authorizations)
                       ':allow)))
              (lambda ()
                (let ((evaluation
                        (application-lisp-evaluate
                         "(test-operation.echo :text \"hello\" :enabled nil :items '(1 :two))"
                         :application application)))
                  (test-assert
                   (and (eq (application-lisp-evaluation-status evaluation) ':ok)
                        (equal (application-lisp-evaluation-values evaluation)
                               '("\"sandboxed allow: hello\"")))
                   "a typed tool form executes through the ordinary decoder and method"))))
             (test-assert (= (application-operation-test-tool-calls tool) 1)
                          "a local tool form executes its tool object exactly once")
             (test-assert (= command-authorizations 1)
                          "a local tool reuses shell authorization exactly once")
             (test-assert (= tool-authorizations 1)
                          "a local tool reuses external authorization exactly once"))
           (let* ((arguments (application-operation-test-tool-arguments tool))
                  (items (tool-argument arguments "items")))
             (test-assert (null (tool-argument arguments "enabled"))
                          "Lisp NIL crosses the local tool boundary as JSON false")
             (test-assert
              (and (vectorp items)
                   (= (length items) 2)
                   (= (aref items 0) 1)
                   (string= (aref items 1) "two"))
              "local Lisp sequences and keyword enum values cross as JSON values"))
           (let ((context (application-operation-test-tool-context tool)))
             (test-assert
              (and (typep context 'tool-context)
                   (eq (tool-context-conversation context)
                       (application-conversation application))
                   (eq (tool-context-registry context)
                       (application-tool-registry application))
                   (null (tool-context-agent context)))
              "local operation execution receives the primary application context"))
           (application-operation-install-bindings application)
           (dolist (name '(trace papercut-close fs.list test-operation.echo))
             (test-assert (fboundp name)
                          (format nil "canonical operation ~S has a Lisp function binding"
                                  name)))
           (recording-terminal-reset terminal)
           (let ((evaluation
                   (application-lisp-evaluate
                    "(progn (help) :finished)"
                    :application application)))
             (test-assert
              (and (eq (application-lisp-evaluation-status evaluation) ':ok)
                   (equal (application-lisp-evaluation-values evaluation)
                          '(":FINISHED"))
                   (search "/help" (recording-terminal-output terminal)))
              "registered command functions remain callable inside arbitrary Lisp"))
           (let ((evaluation
                   (application-lisp-evaluate "(quit)" :application application)))
             (test-assert
              (and (eq (application-lisp-evaluation-status evaluation) ':ok)
                   (eq (application-lisp-evaluation-loop-action evaluation) ':quit)
                   (null (application-lisp-evaluation-values evaluation)))
              "a canonical quit operation transfers control to the application loop"))
           (let ((evaluation
                   (application-lisp-evaluate
                    "(test-operation.echo :text)"
                    :application application)))
             (test-assert
              (and (eq (application-lisp-evaluation-status evaluation) ':aborted)
                   (search "alternating keyword and value"
                           (or (application-lisp-evaluation-condition evaluation) "")))
              "malformed local tool argument plists fail through the Lisp condition boundary"))
           t)
      (ignore-errors (terminal-ui-stop (application-ui application)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
