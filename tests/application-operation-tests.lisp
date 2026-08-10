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
              "items" (json-object "type" "array")
              "odd key)" (tool-string-property "Escaped completion key."))
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
           (test-assert (member "eval-now" names :test #'string=)
                        "the immediate local evaluator is a discoverable operation")
           (test-assert
            (eq (application-operation-kind
                 (application-operation-find application 'eval-now))
                ':local)
            "operation lookup identifies the immediate local form")
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
           (let* ((provider (terminal-ui-completion-function
                             (application-ui application)))
                  (entries (and provider (funcall provider)))
                  (entry-names (mapcar (lambda (entry) (getf entry :name))
                                       entries))
                  (fs-entry (find-if
                             (lambda (entry)
                               (string= (getf entry :name) "(fs.list"))
                             entries))
                  (test-entry (find-if
                               (lambda (entry)
                                 (string= (getf entry :name)
                                          "(test-operation.echo"))
                               entries)))
             (test-assert (functionp provider)
                          "applications install a dynamic operation completion provider")
             (test-assert (member "/help" entry-names :test #'string=)
                          "slash compatibility completion retains canonical commands")
             (test-assert (member "(help)" entry-names :test #'string=)
                          "completion offers a canonical no-argument Lisp command")
             (test-assert (member "(eval-now" entry-names :test #'string=)
                          "completion offers the explicit immediate local form")
             (test-assert
              (and fs-entry (search ":path" (or (getf fs-entry :argument) "")))
              "completion exposes dotted tool names with Lisp keyword arguments")
             (test-assert
              (and test-entry
                   (search ":|odd key)| VALUE"
                           (or (getf test-entry :argument) "")))
              "completion escapes punctuation and whitespace in property names")
             (test-assert (not (member "/fs.list" entry-names :test #'string=))
                          "slash compatibility does not invent tool spellings")
             (test-assert
              (not (find-if (lambda (name)
                              (uiop:string-prefix-p "(yield.submit" name))
                            entry-names))
              "completion hides child-only operations"))
           (let ((unclassified
                   (make-instance
                    'tool
                    :namespace "unclassified"
                    :name "safe-looking"
                    :description "An intentionally unclassified test tool."
                    :parameters (tool-object-schema (json-object) nil))))
             (test-assert (eq (tool-active-turn-action unclassified) ':hold)
                          "unclassified tools wait regardless of their names"))
           (dolist (case
                    '(("(help)" :execute)
                      ("(goal \"pause\")" :hold)
                      ("(quit)" :cancel)
                      ("(fs.list :path \".\")" :execute)
                      ("(shell.run :command \"true\")" :hold)
                      ("(test-operation.echo :text \"hello\")" :hold)
                      ("(self.status)" :execute)
                      ("(self.eval :form \"(+ 1 2)\")" :hold)
                      ("(fs.list :path (progn (setf *print-base* 8) \".\"))"
                       :hold)
                      ("(eval-now (setf *print-base* 8))" :execute)))
             (destructuring-bind (source expected) case
               (test-assert
                (eq (application-operation-source-active-turn-action
                     application source)
                    expected)
                (format nil "~A has active-turn action ~S" source expected))))
           (let ((evaluation
                   (application-lisp-evaluate
                    "(eval-now :not-local-input)"
                    :application application)))
             (test-assert
              (and (eq (application-lisp-evaluation-status evaluation) ':aborted)
                   (search "explicit local Lisp input"
                           (or (application-lisp-evaluation-condition evaluation) "")))
              "eval-now rejects noninteractive evaluator callers"))
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
           (let* ((evaluation
                    (application-lisp-evaluate
                     "(progn (help) :finished)"
                     :application application))
                  (output (recording-terminal-output terminal)))
             (test-assert
              (and (eq (application-lisp-evaluation-status evaluation) ':ok)
                   (equal (application-lisp-evaluation-values evaluation)
                          '(":FINISHED"))
                   (search "(help)" output)
                   (search "(fs.list" output)
                   (search "Slash commands remain compatibility spellings."
                           output))
              "nested help shows canonical command and tool operations"))
           (recording-terminal-reset terminal)
           (test-assert (eq (application--run-command-input application "/help")
                            ':continue)
                        "slash compatibility still executes the command backend")
           (test-assert
            (search "Prefer (help)." (recording-terminal-output terminal))
            "the first slash spelling shows its canonical Lisp form")
           (recording-terminal-reset terminal)
           (application--run-command-input application "/help")
           (test-assert
            (not (search "Prefer (help)." (recording-terminal-output terminal)))
            "a command's preferred Lisp spelling appears only once per session")
           (recording-terminal-reset terminal)
           (test-assert
            (and (eq (application--run-command-input application "/exit") ':quit)
                 (search "Prefer (quit)." (recording-terminal-output terminal)))
            "slash aliases hint the canonical command operation name")
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
