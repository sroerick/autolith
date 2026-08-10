(in-package #:autolith)

;;;; -- User-Facing Operation Protocol --

(defclass application-operation ()
  ((name
    :initarg :name
    :reader application-operation-name
    :type non-empty-string
    :documentation "The canonical lowercase Lisp function name.")
   (description
    :initarg :description
    :reader application-operation-description
    :type non-empty-string
    :documentation "The concise user-facing operation description.")
   (kind
    :initarg :kind
    :reader application-operation-kind
    :type (member :command :tool)
    :documentation "The authoritative backend family for this operation.")
   (backend
    :initarg :backend
    :reader application-operation-backend
    :type (or application-command tool)
    :documentation "The exact registered command or tool object invoked."))
  (:documentation
   "One immutable user-facing projection of a registered command or tool."))

(define-condition application-operation-loop-action (error)
  ((action
    :initarg :action
    :reader application-operation-loop-action-action
    :type (member :quit)
    :documentation "The application loop action requested by the operation."))
  (:report
   (lambda (condition stream)
     (format stream "The local operation requested ~S."
             (application-operation-loop-action-action condition))))
  (:documentation
   "A local operation requested control transfer to the application loop."))

(defvar *application-operation-application* nil
  "The application whose registered operations are callable during local Lisp.")

(defvar *application-operation-bindings* (make-hash-table :test #'eq)
  "Function bindings installed for canonical operation symbols.")

(-> tool-user-callable-p (tool) boolean)
(defgeneric tool-user-callable-p (tool)
  (:documentation "Return whether TOOL belongs to the local user's operation surface."))

(defmethod tool-user-callable-p ((tool tool))
  "Expose ordinary primary-session tools to explicit local user calls."
  (declare (ignore tool))
  t)

(defmethod tool-user-callable-p ((tool task-yield-tool))
  "Hide the child-only terminal yield operation from the primary user surface."
  (declare (ignore tool))
  nil)


;;;; -- Registry Projection --

(-> application-operation--command-name (application-command) non-empty-string)
(defun application-operation--command-name (command)
  "Return COMMAND's canonical Lisp operation name without the leading slash."
  (subseq (application-command-name command) 1))

(-> application-operation--from-command
    (application-command)
    application-operation)
(defun application-operation--from-command (command)
  "Project registered COMMAND into one user-facing operation."
  (make-instance 'application-operation
                 :name (application-operation--command-name command)
                 :description (application-command-description command)
                 :kind ':command
                 :backend command))

(-> application-operation--from-tool (tool) application-operation)
(defun application-operation--from-tool (tool)
  "Project registered TOOL into one user-facing operation."
  (make-instance 'application-operation
                 :name (tool-canonical-name tool)
                 :description (tool-description tool)
                 :kind ':tool
                 :backend tool))

(-> application-operation--tools (application) list)
(defun application-operation--tools (application)
  "Return APPLICATION's ordered locally callable tools."
  (let ((registry
          (and (slot-boundp application 'tool-registry)
               (application-tool-registry application))))
    (if (typep registry 'tool-registry)
        (remove-if-not #'tool-user-callable-p
                       (copy-list (tool-registry-tools registry)))
        nil)))

(-> application-operation--validate-unique-names (list) list)
(defun application-operation--validate-unique-names (operations)
  "Return OPERATIONS after rejecting a duplicate canonical name."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (operation operations)
      (let ((name (string-downcase (application-operation-name operation))))
        (when (gethash name seen)
          (error 'configuration-error
                 :message
                 (format nil
                         "Registered user operations repeat canonical name ~S."
                         name)))
        (setf (gethash name seen) t)))
    operations))

(-> application-operation-list (application) list)
(defun application-operation-list (application)
  "Return APPLICATION's registered commands and locally callable tools in order."
  (application-operation--validate-unique-names
   (append (mapcar #'application-operation--from-command
                   (application-command-list))
           (mapcar #'application-operation--from-tool
                   (application-operation--tools application)))))

(-> application-operation-find
    (application (or string symbol))
    (option application-operation))
(defun application-operation-find (application identifier)
  "Return APPLICATION's operation named by case-insensitive IDENTIFIER."
  (let ((name (string-downcase (string identifier))))
    (find name
          (application-operation-list application)
          :test #'string=
          :key (lambda (operation)
                 (string-downcase (application-operation-name operation))))))


;;;; -- Lisp Argument Boundary --

(-> application-operation--proper-list-p (t) boolean)
(defun application-operation--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (handler-case
      (and (listp value) (integerp (list-length value)))
    (type-error ()
      nil)))

(-> application-operation--json-key (t) non-empty-string)
(defun application-operation--json-key (key)
  "Return KEY as one lowercase JSON object member name."
  (let ((name
          (etypecase key
            (string key)
            (symbol (symbol-name key)))))
    (unless (non-empty-string-p name)
      (error 'configuration-error
             :message "A local tool argument key must not be empty."))
    (string-downcase name)))

(-> application-operation--json-value (t) t)
(defun application-operation--json-value (value)
  "Translate one explicit Lisp VALUE into the project's JSON value convention."
  (cond
    ((eq value t)
     t)
    ((eq value false)
     false)
    ((null value)
     false)
    ((eq value ':null)
     nil)
    ((stringp value)
     (copy-seq value))
    ((pathnamep value)
     (namestring value))
    ((characterp value)
     (string value))
    ((or (integerp value) (floatp value))
     value)
    ((keywordp value)
     (string-downcase (symbol-name value)))
    ((symbolp value)
     (string-downcase (symbol-name value)))
    ((hash-table-p value)
     (let ((object (json-object)))
       (maphash
        (lambda (key member)
          (let ((name (application-operation--json-key key)))
            (when (nth-value 1 (gethash name object))
              (error 'configuration-error
                     :message
                     (format nil "A local JSON object repeats key ~S." name)))
            (setf (gethash name object)
                  (application-operation--json-value member))))
        value)
       object))
    ((vectorp value)
     (map 'vector #'application-operation--json-value value))
    ((application-operation--proper-list-p value)
     (map 'vector #'application-operation--json-value value))
    (t
     (error 'configuration-error
            :message
            (format nil
                    "Local tool argument value ~S cannot cross the JSON boundary."
                    value)))))

(-> application-operation--tool-arguments (list) json-object)
(defun application-operation--tool-arguments (arguments)
  "Translate alternating Lisp keyword ARGUMENTS into one JSON object."
  (unless (and (application-operation--proper-list-p arguments)
               (evenp (length arguments)))
    (error 'configuration-error
           :message
           "A local tool call requires alternating keyword and value arguments."))
  (let ((object (json-object)))
    (loop for (key value) on arguments by #'cddr
          for name = (application-operation--json-key key)
          do
             (when (nth-value 1 (gethash name object))
               (error 'configuration-error
                      :message
                      (format nil "A local tool call repeats argument ~S." name)))
             (setf (gethash name object)
                   (application-operation--json-value value)))
    object))

(-> application-operation--command-value (t) string)
(defun application-operation--command-value (value)
  "Return one evaluated Lisp VALUE as compatibility command argument text."
  (etypecase value
    (string (copy-seq value))
    (pathname (namestring value))
    (character (string value))
    (symbol (string-downcase (symbol-name value)))
    (t (princ-to-string value))))

(-> application-operation--command-invocation
    (application-command list)
    application-command-invocation)
(defun application-operation--command-invocation (command arguments)
  "Return COMMAND's canonical compatibility invocation for evaluated ARGUMENTS."
  (let* ((name (application-command-name command))
         (remainder
           (string-trim
            *application-command-whitespace*
            (format nil
                    "~{~A~^ ~}"
                    (mapcar #'application-operation--command-value arguments))))
         (input
           (if (non-empty-string-p remainder)
               (format nil "~A ~A" name remainder)
               name)))
    (make-instance 'application-command-invocation
                   :input input
                   :name name
                   :remainder remainder
                   :argument (application-command--first-token remainder)
                   :command command)))


;;;; -- Authoritative Dispatch --

(-> application-operation--tool-context (application) tool-context)
(defun application-operation--tool-context (application)
  "Return the primary local-user tool context for APPLICATION."
  (make-instance
   'tool-context
   :configuration (application-configuration application)
   :worker (and (slot-boundp application 'worker)
                (application-worker application))
   :conversation (application-conversation application)
   :registry (application-tool-registry application)
   :agent (and (slot-boundp application 'agent)
               (application-agent application))
   :command-authorization-function
   (lambda (command directory)
     (application-authorize-command application command directory))
   :tool-authorization-function
   (lambda (tool arguments)
     (application-authorize-tool application tool arguments))))

(-> application-operation--call-command
    (application application-command list)
    null)
(defun application-operation--call-command (application command arguments)
  "Invoke COMMAND through its existing handler without duplicating the Lisp source."
  (let* ((invocation
           (application-operation--command-invocation command arguments))
         (*application-command-presentation-invocation* invocation)
         (*application-command-presentation-pending-p* nil)
         (action (application-command-execute command application invocation)))
    (case action
      (:continue
       nil)
      (:quit
       (error 'application-operation-loop-action :action ':quit)))))

(-> application-operation--call-tool (application tool list) string)
(defun application-operation--call-tool (application tool arguments)
  "Invoke TOOL through its existing decoder and execution method for APPLICATION."
  (let* ((canonical-name (tool-canonical-name tool))
         (json-arguments (application-operation--tool-arguments arguments))
         (decoded-arguments
           (tool-decode-arguments tool (json-encode json-arguments))))
    (unless (json-object-p decoded-arguments)
      (error 'tool-error
             :message
             (format nil "Arguments for ~A are not a JSON object." canonical-name)
             :tool-name canonical-name))
    (let* ((ui (and (slot-boundp application 'ui)
                    (application-ui application)))
           (previous-status (and ui (terminal-ui-status ui)))
           (result
             (unwind-protect
                  (progn
                    (when ui
                      (application-set-activity
                       application (format nil "running ~A" canonical-name)))
                    (tool-execute
                     tool
                     (application-operation--tool-context application)
                     decoded-arguments))
               (when ui
                 (application-set-activity application previous-status)))))
      (unless (tool-result-success-p result)
        (error 'tool-error
               :message (tool-result-content result)
               :tool-name canonical-name))
      (tool-result-content result))))

(-> application-operation-call
    (application (or string symbol) &rest t)
    t)
(defun application-operation-call (application identifier &rest arguments)
  "Invoke APPLICATION's registered IDENTIFIER with evaluated Lisp ARGUMENTS."
  (let ((operation (application-operation-find application identifier)))
    (unless operation
      (error 'configuration-error
             :message
             (format nil "No registered user operation is named ~S." identifier)))
    (ecase (application-operation-kind operation)
      (:command
       (application-operation--call-command
        application (application-operation-backend operation) arguments))
      (:tool
       (application-operation--call-tool
        application (application-operation-backend operation) arguments)))))


;;;; -- Canonical Function Bindings --

(-> application-operation--function-symbol (non-empty-string) symbol)
(defun application-operation--function-symbol (name)
  "Return the AUTOLITH symbol naming canonical operation NAME."
  (intern (string-upcase name) '#:autolith))

(-> application-operation--binding-function (non-empty-string) function)
(defun application-operation--binding-function (name)
  "Return the dynamic wrapper function for canonical operation NAME."
  (lambda (&rest arguments)
    (unless (typep *application-operation-application* 'application)
      (error 'configuration-error
             :message
             (format nil
                     "Operation ~A requires an active local Autolith evaluation."
                     name)))
    (apply #'application-operation-call
           *application-operation-application* name arguments)))

(-> application-operation--install-binding (application-operation) symbol)
(defun application-operation--install-binding (operation)
  "Install OPERATION's canonical function unless an unrelated binding conflicts."
  (let* ((name (application-operation-name operation))
         (symbol (application-operation--function-symbol name))
         (installed (gethash symbol *application-operation-bindings*))
         (existing (and (fboundp symbol) (fdefinition symbol))))
    (cond
      ((and installed (eq existing installed))
       symbol)
      ((fboundp symbol)
       (error 'configuration-error
              :message
              (format nil
                      "Registered operation ~A conflicts with existing function ~S."
                      name symbol)))
      (t
       (let ((function (application-operation--binding-function name)))
         (setf (symbol-function symbol) function
               (gethash symbol *application-operation-bindings*) function
               (documentation symbol 'function)
               (application-operation-description operation))
         symbol)))))

(-> application-operation-install-bindings (application) list)
(defun application-operation-install-bindings (application)
  "Install canonical Common Lisp function bindings for APPLICATION's operations."
  (mapcar #'application-operation--install-binding
          (application-operation-list application)))
