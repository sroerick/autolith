(in-package #:autolith)

;;;; -- Lisp Machine Test Support --

(defvar *lisp-machine-test-value* nil
  "Active-image value used to verify direct local evaluation side effects.")

(defvar *lisp-machine-test-activity* nil
  "Local activity captured from inside one explicit active-image evaluation.")

(-> lisp-machine-tests--application () (values application pathname))
(defun lisp-machine-tests--application ()
  "Return a temporary application with a recording terminal and its data root."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier (make-identifier)))
         (ui
           (terminal-ui-create
            :terminal (make-instance 'recording-terminal
                                     :columns 100
                                     :styled-p t)))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :tool-registry (make-default-tool-registry)
                          :ui ui)))
    (values application root)))

(-> lisp-machine-tests--controller (application) application-input-controller)
(defun lisp-machine-tests--controller (application)
  "Return a local input controller attached to APPLICATION."
  (let ((controller
          (make-instance 'application-input-controller
                         :application application
                         :later-state (make-instance 'later-state)
                         :pending-later-entries nil
                         :main-thread (current-thread))))
    (setf (application-input-controller application) controller)
    controller))


;;;; -- Direct Evaluation --

(-> test-application-lisp-evaluation () null)
(defun test-application-lisp-evaluation ()
  "Test one-form reading, output, values, side effects, conditions, and restarts."
  (test-assert (application-lisp-input-incomplete-p "(list 1")
               "an unterminated top-level form requests another input line")
  (test-assert (application-lisp-input-incomplete-p "(format nil \"open")
               "an unterminated string requests another input line")
  (test-assert (not (application-lisp-input-incomplete-p "(list 1)"))
               "a complete top-level form is ready for evaluation")
  (let ((evaluation
          (application-lisp-evaluate
           "(progn (format t \"hello~%\") (values 1 2))")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':ok)
          (string= (application-lisp-evaluation-output evaluation)
                   (format nil "hello~%"))
          (equal (application-lisp-evaluation-values evaluation) '("1" "2")))
     "local evaluation captures output and every returned value"))
  (let ((evaluation (application-lisp-evaluate "(values)")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':ok)
          (null (application-lisp-evaluation-values evaluation)))
     "local evaluation preserves a zero-value return"))
  (setf *lisp-machine-test-value* nil)
  (application-lisp-evaluate "(setf *lisp-machine-test-value* :changed)")
  (test-assert (eq *lisp-machine-test-value* ':changed)
               "local evaluation mutates the active image directly")
  (let ((evaluation
          (application-lisp-evaluate "(list 1) (list 2)")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':aborted)
          (search "exactly one Common Lisp form"
                  (application-lisp-evaluation-condition evaluation)
                  :test #'char-equal))
     "local evaluation rejects trailing forms without entering the debugger"))
  (let ((evaluation
          (application-lisp-evaluate
           "(restart-case (error \"missing\") (use-value (value) value))"
           :restart-selector
           (lambda (condition restarts)
             (declare (ignore condition))
             (values (find 'use-value restarts :key #'restart-name)
                     "42")))))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':ok)
          (equal (application-lisp-evaluation-values evaluation) '("42"))
          (member "USE-VALUE"
                  (application-lisp-evaluation-restart-names evaluation)
                  :test #'string=))
     "a selected live restart receives evaluated Lisp arguments"))
  (let ((evaluation (application-lisp-evaluate "(error \"stop\")")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':aborted)
          (string= (application-lisp-evaluation-condition evaluation) "stop")
          (member "ABORT-LISP-EVALUATION"
                  (application-lisp-evaluation-restart-names evaluation)
                  :test #'string=))
     "declining restart selection aborts only the local evaluation"))
  nil)


(-> test-application-lisp-activity () null)
(defun test-application-lisp-activity ()
  "Test local evaluation remains visible without replacing provider activity."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let ((ui (application-ui application)))
      (unwind-protect
           (progn
             (terminal-ui-start ui)
             (terminal-ui-set-status ui "provider active")
             (setf *lisp-machine-test-activity* nil)
             (test-assert
              (eq (application-run-lisp-input
                   application
                   "(setf *lisp-machine-test-activity* (terminal-ui-local-activity (application-ui *application-operation-application*)))")
                  ':continue)
              "explicit local Lisp completes beside provider activity")
             (test-assert
              (and (string= *lisp-machine-test-activity*
                            "evaluating local Lisp")
                   (string= (terminal-ui-status ui) "provider active")
                   (null (terminal-ui-local-activity ui)))
              "local activity is visible during evaluation and clears without clobbering status"))
        (ignore-errors (terminal-ui-stop ui))
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry application)))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist :ignore))))
  nil)


;;;; -- Responsive Input Integration --

(-> test-application-lisp-input-routing () null)
(defun test-application-lisp-input-routing ()
  "Test exact Lisp classification, multiline continuation, queues, and execution."
  (test-assert
   (and (null (application--message-input "(values 1 2)"))
        (equal (application-input-controller--input-work "(values 1 2)")
               '(:lisp "(values 1 2)"))
        (equal
         (application-input-controller--restore-work-item
          (application-input-controller--pending-work-entry-form
           '(:lisp "(values 1 2)")))
         '(:lisp "(values 1 2)")))
   "explicit Lisp stays outside model routing and survives pending persistence")
  (test-assert
   (string= (application--message-input " (values 1 2)")
            " (values 1 2)")
   "leading whitespace preserves prose routing")
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let* ((ui (application-ui application))
           (terminal (terminal-ui-terminal ui))
           (controller (lisp-machine-tests--controller application)))
      (unwind-protect
           (progn
             (terminal-ui-start ui)
             (terminal-ui-set-input ui "(list 1")
             (application-input-controller--process-event controller ':submit)
             (test-assert
              (and (string= (line-editor-text (terminal-ui-editor ui))
                            (format nil "(list 1~%"))
                   (null (application-input-controller-work-items controller)))
              "Enter continues an incomplete Lisp form without submitting work")
             (terminal-ui-set-input ui "(values 3 4)")
             (application-input-controller--process-event controller ':submit)
             (test-assert
              (equal (application-input-controller-work-items controller)
                     '((:lisp "(values 3 4)")))
              "a complete Lisp form enters the durable local work queue")
             (setf (application-input-controller-work-items controller) nil
                   (application-input-controller-active-p controller) t)
             (recording-terminal-reset terminal)
             (application-input-controller--handle-submission
              controller "(fs.list :path \".\")")
             (let ((output
                     (clinedi:ansi-strip
                      (recording-terminal-output terminal))))
               (test-assert
                (and (null (application-input-controller-work-items controller))
                     (search "(fs.list :path \".\")" output)
                     (not (search "scheduled" output :test #'char-equal)))
                "a nonconflicting registered operation runs beside the active turn"))
             (recording-terminal-reset terminal)
             (setf *lisp-machine-test-value* nil)
             (application-input-controller--handle-submission
              controller
              "(eval-now (setf *lisp-machine-test-value* :immediate))")
             (let ((output
                     (clinedi:ansi-strip
                      (recording-terminal-output terminal))))
               (test-assert
                (and (eq *lisp-machine-test-value* ':immediate)
                     (null (application-input-controller-work-items controller))
                     (search "⇒ :IMMEDIATE" output))
                "eval-now explicitly forces arbitrary local evaluation immediately"))
             (recording-terminal-reset terminal)
             (application-input-controller--handle-submission
              controller "(self.eval :form \"(+ 1 2)\")")
             (test-assert
              (and (equal (application-input-controller-work-items controller)
                          '((:lisp "(self.eval :form \"(+ 1 2)\")")))
                   (search "local evaluation scheduled"
                           (recording-terminal-output terminal)
                           :test #'char-equal))
              "active-image tools wait for the serialized application boundary")
             (setf (application-input-controller-work-items controller) nil)
             (recording-terminal-reset terminal)
             (let ((authorization-calls 0))
               (test-call-with-function-replacements
                (list
                 (list 'application-authorize-command
                       (lambda (observed command directory)
                         (declare (ignore observed command directory))
                         (incf authorization-calls)
                         ':sandboxed)))
                (lambda ()
                  (application-input-controller--handle-submission
                   controller "(shell.run :command \"true\")")))
               (test-assert
                (and (zerop authorization-calls)
                     (equal (application-input-controller-work-items controller)
                            '((:lisp "(shell.run :command \"true\")")))
                     (search "local evaluation scheduled"
                             (recording-terminal-output terminal)
                             :test #'char-equal))
                "approval-requiring tools wait instead of joining the reader"))
             (setf (application-input-controller-work-items controller) nil)
             (recording-terminal-reset terminal)
             (let ((previous-reader
                     (application-input-controller-reader-thread controller)))
               (unwind-protect
                    (progn
                      (setf (application-input-controller-reader-thread controller)
                            (current-thread))
                      (application-input-controller--handle-submission
                       controller
                       "(eval-now (shell.run :command \"true\"))"))
                 (setf (application-input-controller-reader-thread controller)
                       previous-reader)))
             (let ((output
                     (clinedi:ansi-strip
                      (recording-terminal-output terminal))))
               (test-assert
                (and (null (application-input-controller-work-items controller))
                     (search "Terminal-owning work cannot pause" output)
                     (search "aborted" output :test #'char-equal))
                "eval-now rejects modal authorization instead of self-joining"))
             (setf (application-input-controller-work-items controller) nil)
             (recording-terminal-reset terminal)
             (application-input-controller--handle-submission
              controller "(values :later)")
             (test-assert
              (and (equal (application-input-controller-work-items controller)
                          '((:lisp "(values :later)")))
                   (null (application-input-controller-steering-items controller))
                   (search "local evaluation scheduled"
                           (recording-terminal-output terminal)
                           :test #'char-equal))
              "busy arbitrary Lisp waits for the idle boundary instead of steering")
             (setf (application-input-controller-active-p controller) nil
                   (application-input-controller-work-items controller) nil
                   (application-input-controller-stopping-p controller) t)
             (recording-terminal-reset terminal)
             (let ((provider-items-before
                     (copy-list
                      (conversation-input-items
                       (application-conversation application)))))
               (application-input-controller--run-work
                controller '(:lisp "(values 7 8)"))
               (let ((output
                       (clinedi:ansi-strip
                        (recording-terminal-output terminal))))
                 (test-assert
                  (and (search "(values 7 8)" output)
                       (search "⇒ 7" output)
                       (search "⇒ 8" output)
                       (equal provider-items-before
                              (conversation-input-items
                               (application-conversation application))))
                  "local work shows exact source and values without model context")))
             (let ((quit-controller
                     (lisp-machine-tests--controller application)))
               (test-call-with-function-replacements
                (list
                 (list 'application-input-controller-call-with-reader-paused
                       (lambda (ignored function)
                         (declare (ignore ignored))
                         (funcall function))))
                (lambda ()
                  (application-input-controller--run-work
                   quit-controller '(:lisp "(quit)"))))
               (test-assert
                (and (application-input-controller-stopping-p quit-controller)
                     (eq (application-input-controller-exit-reason quit-controller)
                         ':quit))
                "a Lisp quit operation exits through the responsive controller")))
        (ignore-errors (terminal-ui-stop ui))
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry application)))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist :ignore))))
  nil)

(-> run-lisp-machine-tests () null)
(defun run-lisp-machine-tests ()
  "Run direct local Lisp evaluation and responsive routing tests."
  (test-application-lisp-evaluation)
  (test-application-lisp-activity)
  (test-application-lisp-input-routing)
  nil)
