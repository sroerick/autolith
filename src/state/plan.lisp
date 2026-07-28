(in-package #:autolith)

;;;; -- Workspace Plan --

(defparameter *plan-version* 1
  "The readable workspace plan format version.")

(defparameter *plan-maximum-steps* 32
  "The maximum number of steps retained in one workspace plan.")

(defparameter *plan-step-text-limit* 500
  "The maximum character count of one plan step.")

(deftype plan-status ()
  "The lifecycle of one plan step."
  '(member :pending :doing :done))

(defclass plan-step ()
  ((text
    :initarg :text
    :reader plan-step-text
    :type non-empty-string
    :documentation "The ordered work described by this plan step.")
   (status
    :initarg :status
    :reader plan-step-status
    :type plan-status
    :documentation "Whether this step is pending, in progress, or done."))
  (:documentation "One ordered step in the current workspace plan."))

(defclass workspace-plan ()
  ((directory
    :initarg :directory
    :reader workspace-plan-directory
    :type non-empty-string
    :documentation "The workspace directory this plan belongs to.")
   (explanation
    :initarg :explanation
    :initform nil
    :reader workspace-plan-explanation
    :type (option string)
    :documentation "Optional short explanation of the current plan.")
   (steps
    :initarg :steps
    :initform nil
    :reader workspace-plan-steps
    :type list
    :documentation "Ordered plan steps from first to last.")
   (updated-at
    :initarg :updated-at
    :reader workspace-plan-updated-at
    :type timestamp
    :documentation "The universal time at which this plan last changed."))
  (:documentation "The current ordered plan for one workspace."))


(define-condition plan-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader plan-error-pathname
    :type (option pathname)
    :documentation "The plan state pathname related to the failure.")
   (operation
    :initarg :operation
    :reader plan-error-operation
    :type keyword
    :documentation "The plan operation that failed."))
  (:documentation "A durable workspace plan operation failed.")
  (:report
   (lambda (condition stream)
     (format stream "Plan ~A failed~@[ at ~A~]: ~A"
             (plan-error-operation condition)
             (plan-error-pathname condition)
             (autolith-error-message condition)))))


(-> plan--status (t) (option plan-status))
(defun plan--status (value)
  "Return VALUE as a plan status keyword, or NIL when invalid."
  (cond
    ((typep value 'plan-status) value)
    ((not (stringp value)) nil)
    ((string-equal value "pending") ':pending)
    ((or (string-equal value "doing")
         (string-equal value "in_progress")
         (string-equal value "inProgress"))
     ':doing)
    ((or (string-equal value "done")
         (string-equal value "completed"))
     ':done)
    (t nil)))

(-> plan--step-form-p (t) boolean)
(defun plan--step-form-p (form)
  "Return true when FORM is one portable plan step."
  (and (listp form)
       (eq (first form) :step)
       (let ((text (getf (rest form) :text))
             (status (getf (rest form) :status)))
         (and (non-empty-string-p text)
              (<= (length text) *plan-step-text-limit*)
              (typep status 'plan-status)))))

(-> plan--form-p (t) boolean)
(defun plan--form-p (form)
  "Return true when FORM is a complete portable workspace plan."
  (and (listp form)
       (eq (first form) :plan)
       (= (or (getf (rest form) :version) 0) *plan-version*)
       (non-empty-string-p (getf (rest form) :directory))
       (typep (getf (rest form) :updated-at) 'timestamp)
       (let ((explanation (getf (rest form) :explanation))
             (steps (getf (rest form) :steps)))
         (and (or (null explanation) (stringp explanation))
              (listp steps)
              (<= (length steps) *plan-maximum-steps*)
              (every #'plan--step-form-p steps)))))

(-> plan-step->form (plan-step) list)
(defun plan-step->form (step)
  "Return STEP as one portable form."
  (list :step
        :text (plan-step-text step)
        :status (plan-step-status step)))

(-> plan->form (workspace-plan) list)
(defun plan->form (plan)
  "Return PLAN as one portable form."
  (list :plan
        :version *plan-version*
        :directory (workspace-plan-directory plan)
        :explanation (workspace-plan-explanation plan)
        :updated-at (workspace-plan-updated-at plan)
        :steps (mapcar #'plan-step->form (workspace-plan-steps plan))))

(-> form->plan-step (list) plan-step)
(defun form->plan-step (form)
  "Return the plan step represented by FORM."
  (make-instance 'plan-step
                 :text (getf (rest form) :text)
                 :status (getf (rest form) :status)))

(-> form->plan (list) workspace-plan)
(defun form->plan (form)
  "Return the workspace plan represented by FORM."
  (make-instance 'workspace-plan
                 :directory (getf (rest form) :directory)
                 :explanation (getf (rest form) :explanation)
                 :updated-at (getf (rest form) :updated-at)
                 :steps (mapcar #'form->plan-step (getf (rest form) :steps))))

(-> plan--directory-name (configuration) string)
(defun plan--directory-name (configuration)
  "Return CONFIGURATION's canonical workspace directory key."
  (workspace-directory-name
   (configuration-working-directory configuration)))

(-> plan--pathname (configuration) pathname)
(defun plan--pathname (configuration)
  "Return CONFIGURATION's workspace-specific plan pathname."
  (configuration-plan-path
   configuration
   (workspace-directory-identifier
    (configuration-working-directory configuration))))

(-> plan--read-path (pathname) (option workspace-plan))
(defun plan--read-path (pathname)
  "Return the valid plan at PATHNAME, or NIL when absent or malformed."
  (when (probe-file pathname)
    (handler-case
        (multiple-value-bind (form complete-p)
            (snapshot-read pathname)
          (when (and complete-p (plan--form-p form))
            (form->plan form)))
      (error (condition)
        (error 'plan-error
               :message (format nil "Could not load the workspace plan: ~A"
                                condition)
               :pathname pathname
               :operation ':load)))))

(-> plan--write-path (pathname workspace-plan) workspace-plan)
(defun plan--write-path (pathname plan)
  "Atomically publish PLAN at PATHNAME."
  (handler-case
      (progn
        (ensure-directories-exist pathname)
        (snapshot-write pathname (plan->form plan))
        plan)
    (error (condition)
      (error 'plan-error
             :message (format nil "Could not write the workspace plan: ~A"
                              condition)
             :pathname pathname
             :operation ':write))))

(-> plan--delete-path (pathname keyword) null)
(defun plan--delete-path (pathname operation)
  "Remove PATHNAME when present, reporting failures as OPERATION."
  (when (probe-file pathname)
    (handler-case
        (delete-file pathname)
      (error (condition)
        (error 'plan-error
               :message (format nil "Could not remove the workspace plan: ~A"
                                condition)
               :pathname pathname
               :operation operation))))
  nil)

(-> plan--retire-matching-legacy (configuration string) null)
(defun plan--retire-matching-legacy (configuration directory)
  "Remove the legacy global plan when it belongs to DIRECTORY."
  (let* ((pathname (configuration-legacy-plan-path configuration))
         (plan (plan--read-path pathname)))
    (when (and plan
               (string= (workspace-plan-directory plan) directory))
      (plan--delete-path pathname ':migrate)))
  nil)

(-> plan-load (configuration) (option workspace-plan))
(defun plan-load (configuration)
  "Return CONFIGURATION's workspace plan, migrating legacy state when needed."
  (let* ((directory (plan--directory-name configuration))
         (pathname (plan--pathname configuration))
         (plan (plan--read-path pathname)))
    (cond
      (plan
       (and (string= (workspace-plan-directory plan) directory)
            plan))
      ((probe-file pathname)
       nil)
      (t
       (let* ((legacy-pathname
                (configuration-legacy-plan-path configuration))
              (legacy-plan (plan--read-path legacy-pathname)))
         (when (and legacy-plan
                    (string= (workspace-plan-directory legacy-plan) directory))
           (plan--write-path pathname legacy-plan)
           (plan--delete-path legacy-pathname ':migrate)
           legacy-plan))))))

(-> plan-write (configuration workspace-plan) workspace-plan)
(defun plan-write (configuration plan)
  "Atomically publish PLAN for CONFIGURATION without affecting other workspaces."
  (let ((directory (plan--directory-name configuration)))
    (plan--write-path (plan--pathname configuration) plan)
    (plan--retire-matching-legacy configuration directory)
    plan))

(-> plan-clear (configuration) null)
(defun plan-clear (configuration)
  "Remove only CONFIGURATION's workspace plan when present."
  (let ((directory (plan--directory-name configuration)))
    (plan--delete-path (plan--pathname configuration) ':clear)
    (plan--retire-matching-legacy configuration directory))
  nil)

(-> plan-update
    (configuration list &key (:explanation (option string)))
    workspace-plan)
(defun plan-update (configuration steps &key explanation)
  "Replace CONFIGURATION's plan with ordered STEPS and optional EXPLANATION.

STEPS is a list of (:text string :status plan-status) property lists."
  (unless (and (listp steps) (plusp (length steps)))
    (error 'plan-error
           :message "A plan requires at least one step."
           :pathname (plan--pathname configuration)
           :operation ':update))
  (when (> (length steps) *plan-maximum-steps*)
    (error 'plan-error
           :message (format nil "A plan may contain at most ~D steps."
                            *plan-maximum-steps*)
           :pathname (plan--pathname configuration)
           :operation ':update))
  (when (and explanation
             (not (stringp explanation)))
    (error 'plan-error
           :message "Plan explanation must be a string."
           :pathname (plan--pathname configuration)
           :operation ':update))
  (let ((plan-steps
          (mapcar
           (lambda (step)
             (let* ((text (getf step :text))
                    (status (plan--status (getf step :status))))
               (unless (and (non-empty-string-p text)
                            (<= (length text) *plan-step-text-limit*))
                 (error 'plan-error
                        :message "Each plan step needs bounded non-empty text."
                        :pathname (plan--pathname configuration)
                        :operation ':update))
               (unless status
                 (error 'plan-error
                        :message "Plan status must be pending, doing, or done."
                        :pathname (plan--pathname configuration)
                        :operation ':update))
               (make-instance 'plan-step :text text :status status)))
           steps)))
    (plan-write
     configuration
     (make-instance
      'workspace-plan
      :directory (plan--directory-name configuration)
      :explanation (and (non-empty-string-p explanation) explanation)
      :updated-at (get-universal-time)
      :steps plan-steps))))

(-> plan-render ((option workspace-plan)) string)
(defun plan-render (plan)
  "Return PLAN as compact model-visible text."
  (if (or (null plan) (null (workspace-plan-steps plan)))
      "No active plan."
      (with-output-to-string (stream)
        (when (workspace-plan-explanation plan)
          (format stream "Explanation: ~A~%"
                  (workspace-plan-explanation plan)))
        (loop for step in (workspace-plan-steps plan)
              for index from 1
              do (format stream "~D. [~(~A~)] ~A~%"
                         index
                         (plan-step-status step)
                         (plan-step-text step))))))
