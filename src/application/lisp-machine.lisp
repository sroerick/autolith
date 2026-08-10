(in-package #:autolith)

;;;; -- Local Common Lisp Evaluation --

(deftype application-lisp-evaluation-status ()
  "The terminal outcome of one explicit local Common Lisp evaluation."
  '(member :ok :aborted :error))

(defstruct (application-lisp-evaluation
             (:constructor application-lisp-evaluation-create
                 (&key status output values condition restart-names loop-action)))
  "Captured outcome of one explicit active-image Common Lisp evaluation."
  (status ':ok :type application-lisp-evaluation-status)
  (output "" :type string)
  (values nil :type list)
  (condition nil :type (option string))
  (restart-names nil :type list)
  (loop-action nil :type (option (member :quit))))

(defparameter *application-lisp-value-restart-names*
  '(use-value store-value)
  "Restart names whose ordinary interactive use expects a Lisp value.")

(-> application-lisp-input-incomplete-p (string) boolean)
(defun application-lisp-input-incomplete-p (source)
  "Return true when SOURCE begins a Common Lisp form that needs more input.

The probe disables read-time evaluation so submission classification cannot run
SOURCE twice. Reader errors other than end of file are complete submissions and
are presented by the evaluator."
  (handler-case
      (progn
        (self-read-form source :read-eval nil)
        nil)
    (end-of-file ()
      t)
    (reader-error ()
      nil)
    (error ()
      nil)))

(-> application-lisp-input-with-text
    ((or string user-message-input) string)
    (or string user-message-input))
(defun application-lisp-input-with-text (input text)
  "Return INPUT with exact replacement TEXT and preserved image attachments."
  (etypecase input
    (string
     (copy-seq text))
    (user-message-input
     (user-message-input-create
      :text text
      :image-pathnames (user-message-input-image-pathnames input)))))

(-> application-lisp--control-condition-p (serious-condition) boolean)
(defun application-lisp--control-condition-p (condition)
  "Return true when CONDITION belongs to Autolith's own control boundary."
  (typep condition
         '(or application-operation-loop-action
              application-turn-cancelled
              application-input-failed
              rollback-requested
              agent-loop-error
              conversation-invariant-error
              active-image-corruption)))

(-> application-lisp--selectable-restarts (serious-condition) list)
(defun application-lisp--selectable-restarts (condition)
  "Return CONDITION's named restarts without Autolith's outer ABORT restart."
  (remove-if
   (lambda (restart)
     (let ((name (restart-name restart)))
       (or (null name)
           (eq name 'abort))))
   (compute-restarts condition)))

(-> application-lisp--restart-report (restart) string)
(defun application-lisp--restart-report (restart)
  "Return RESTART's printable report without terminal control characters."
  (sanitize-text (princ-to-string restart) :single-line-p t))

(-> application-lisp--restart-arguments (string) list)
(defun application-lisp--restart-arguments (source)
  "Evaluate one Lisp SOURCE form and return all its values as restart arguments."
  (multiple-value-list (eval (self-read-form source))))

(-> application-lisp-evaluate
    (string &key (:restart-selector (option function))
                 (:application (option application)))
    application-lisp-evaluation)
(defun application-lisp-evaluate (source &key restart-selector application)
  "Evaluate exactly one SOURCE form in the active AUTOLITH package.

RESTART-SELECTOR receives the signaling condition and selectable restart
objects. It returns a selected restart and, optionally, a Lisp source form whose
multiple values become restart arguments. Returning NIL aborts only this local
evaluation. APPLICATION enables its registered command and tool function
bindings. Output and every returned value are captured without mutation
journaling or provider conversation projection."
  (let ((output-stream (make-string-output-stream))
        (raw-values nil)
        (status ':ok)
        (condition-text nil)
        (restart-names nil)
        (loop-action nil)
        (handling-condition-p nil))
    (labels ((abort-evaluation ()
               (let ((restart (find-restart 'abort-lisp-evaluation)))
                 (if restart
                     (invoke-restart restart)
                     (setf status ':aborted))))

             (handle-condition (condition)
               (unless (or handling-condition-p
                           (application-lisp--control-condition-p condition))
                 (let* ((handling-condition-p t)
                        (restarts
                          (application-lisp--selectable-restarts condition)))
                   (setf condition-text (princ-to-string condition)
                         restart-names
                         (mapcar (lambda (restart)
                                   (symbol-name (restart-name restart)))
                                 restarts))
                   (multiple-value-bind (selected argument-source)
                       (and restart-selector
                            (funcall restart-selector condition restarts))
                     (if (and selected (member selected restarts :test #'eq))
                         (if (non-empty-string-p argument-source)
                             (apply #'invoke-restart
                                    selected
                                    (application-lisp--restart-arguments
                                     argument-source))
                             (invoke-restart selected))
                         (abort-evaluation)))))))
      (handler-case
          (let ((*standard-output* output-stream)
                (*error-output* output-stream)
                (*trace-output* output-stream)
                (*package* (find-package '#:autolith))
                (*application-operation-application* application))
            (when application
              (application-operation-install-bindings application))
            (restart-case
                (handler-bind ((serious-condition #'handle-condition))
                  (setf raw-values
                        (multiple-value-list
                         (eval (self-read-form source)))))
              (abort-lisp-evaluation ()
                :report "Return to the Autolith prompt."
                (setf status ':aborted
                      raw-values nil))))
        (application-operation-loop-action (condition)
          (setf status ':ok
                loop-action
                (application-operation-loop-action-action condition)
                raw-values nil))
        ((or application-turn-cancelled
             application-input-failed
             rollback-requested
             agent-loop-error
             conversation-invariant-error
             active-image-corruption)
         (condition)
          (error condition))
        (serious-condition (condition)
          (setf status ':error
                condition-text (princ-to-string condition)
                raw-values nil))))
    (application-lisp-evaluation-create
     :status status
     :output (get-output-stream-string output-stream)
     :values (mapcar #'sbcl-worker-render-value raw-values)
     :condition condition-text
     :restart-names restart-names
     :loop-action loop-action)))


;;;; -- Local Evaluation Presentation --

(-> application-lisp--source-entry (string) list)
(defun application-lisp--source-entry (source)
  "Return exact submitted SOURCE as one syntax-highlighted local transcript entry."
  (append
   (list (terminal-span ':lisp-prompt "* "))
   (or (syntax--highlight-spans source
                               :language *application-common-lisp-language*)
       (list (terminal-span ':code source)))))

(-> application-lisp--output-spans (string) list)
(defun application-lisp--output-spans (output)
  "Return captured OUTPUT as a marked local evaluation section."
  (when (non-empty-string-p output)
    (append
     (list (terminal-span ':notice "output")
           (terminal-span ':plain (string #\Newline))
           (terminal-span ':plain output))
     (unless (char= (char output (1- (length output))) #\Newline)
       (list (terminal-span ':plain (string #\Newline)))))))

(-> application-lisp--result-entry (application-lisp-evaluation) list)
(defun application-lisp--result-entry (evaluation)
  "Return EVALUATION's output, values, or condition as terminal spans."
  (append
   (application-lisp--output-spans
    (application-lisp-evaluation-output evaluation))
   (case (application-lisp-evaluation-status evaluation)
     (:ok
      (let ((values (application-lisp-evaluation-values evaluation)))
        (if values
            (loop for value in values
                  for first-p = t then nil
                  append
                  (append
                   (unless first-p
                     (list (terminal-span ':plain (string #\Newline))))
                   (list (terminal-span ':success "⇒ ")
                         (terminal-span ':code value))))
            (list (terminal-span ':success "⇒ ")
                  (terminal-span ':dim "no values")))))
      (:aborted
       (append
        (list (terminal-span ':failure "aborted"))
        (when (application-lisp-evaluation-condition evaluation)
          (list
           (terminal-span
            ':plain
            (format nil ": ~A"
                    (application-lisp-evaluation-condition evaluation)))))))
     (:error
      (list
       (terminal-span ':failure "error: ")
       (terminal-span
        ':plain
        (or (application-lisp-evaluation-condition evaluation)
            "unknown local evaluation failure")))))))

(-> application-lisp--restart-items (list) list)
(defun application-lisp--restart-items (restarts)
  "Return modal selector items for ordered RESTARTS."
  (loop for restart in restarts
        for index from 0
        collect
        (list :name (format nil "~D" index)
              :argument nil
              :description
              (format nil "~(~A~)  ~A"
                      (restart-name restart)
                      (application-lisp--restart-report restart)))))

(-> application-lisp--read-restart-value (application) (option string))
(defun application-lisp--read-restart-value (application)
  "Read one restart argument form while preserving the user's current draft."
  (let* ((ui (application-ui application))
         (editor (terminal-ui-editor ui))
         (saved-input
           (terminal-ui--submission-input ui (line-editor-text editor))))
    (unwind-protect
         (progn
           (terminal-ui-set-input ui "")
           (application-present
            application
            (list (terminal-span ':hint
                                 "Enter one Lisp form. Its multiple values become restart arguments.")))
           (loop
             (multiple-value-bind (action payload)
                 (terminal-ui-process-event ui (terminal-ui-read-event ui))
               (case action
                 (:submit
                  (return (user-message-input-text payload)))
                 ((:interrupt :end-of-input :escape)
                  (return nil))))))
      (terminal-ui-set-input ui saved-input))))

(-> application-lisp--select-restart
    (application serious-condition list)
    (values (option restart) (option string)))
(defun application-lisp--select-restart (application condition restarts)
  "Let the local user choose one live RESTART for CONDITION."
  (let* ((ui (application-ui application))
         (terminal (and ui (terminal-ui-terminal ui))))
    (unless (and ui terminal (terminal-interactive-p terminal))
      (return-from application-lisp--select-restart (values nil nil)))
    (let* ((choice
             (terminal-ui-select
              ui
              :title (text-cell-prefix
                      (sanitize-text (princ-to-string condition)
                                     :single-line-p t)
                      72)
              :items (application-lisp--restart-items restarts)
              :resize-callback #'application-pending-terminal-size))
           (index (and choice (parse-integer choice :junk-allowed t)))
           (restart (and index (nth index restarts))))
      (if (and restart
               (member (restart-name restart)
                       *application-lisp-value-restart-names*
                       :test #'eq))
          (let ((source (application-lisp--read-restart-value application)))
            (if (non-empty-string-p source)
                (values restart source)
                (values nil nil)))
          (values restart nil)))))

(-> application-run-lisp-input (application string) keyword)
(defun application-run-lisp-input (application source)
  "Evaluate explicit local Lisp SOURCE and present it outside provider context."
  (application-present application (application-lisp--source-entry source))
  (application-set-activity application "evaluating local Lisp")
  (let ((evaluation
          (unwind-protect
               (application-lisp-evaluate
                source
                :application application
                :restart-selector
                (lambda (condition restarts)
                  (application-lisp--select-restart
                   application condition restarts)))
            (application-set-activity application nil))))
    (application-present application
                         (application-lisp--result-entry evaluation))
    (case (application-lisp-evaluation-status evaluation)
      (:ok
       (or (application-lisp-evaluation-loop-action evaluation) ':continue))
      (:aborted ':aborted)
      (:error ':failed))))
