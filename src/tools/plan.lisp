(in-package #:autolith)

;;;; -- Plan Tool Classes --

(defclass plan-tool (tool)
  ()
  (:documentation "A tool reading or updating the current workspace plan."))

(defclass plan-list-tool (plan-tool)
  ()
  (:documentation "A tool reading the current workspace plan."))

(defclass plan-update-tool (plan-tool)
  ()
  (:documentation "A tool replacing the current workspace plan."))

(defmethod tool-execution-policy ((tool plan-update-tool))
  "Serialize plan replacement against the rest of one provider batch."
  (declare (ignore tool))
  ':exclusive)


;;;; -- Tool Executions --

(defmethod tool-execute ((tool plan-list-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Read the current workspace plan."
  (declare (ignore tool arguments))
  (handler-case
      (let ((plan (plan-load (tool-context-configuration context))))
        (tool-success (plan-render plan)))
    (plan-error (condition)
      (error 'tool-error
             :message (format nil "~A" condition)
             :tool-name "plan.list"))))

(defmethod tool-execute ((tool plan-update-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Replace the current workspace plan."
  (declare (ignore tool))
  (handler-case
      (let* ((raw-steps (gethash "steps" arguments))
             (explanation (gethash "explanation" arguments))
             (steps
               (cond
                 ((not (vectorp raw-steps))
                  (error 'tool-error
                         :message "plan.update requires a steps array."
                         :tool-name "plan.update"))
                 (t
                  (map 'list
                       (lambda (item)
                         (unless (json-object-p item)
                           (error 'tool-error
                                  :message "Each plan step must be an object."
                                  :tool-name "plan.update"))
                         (list :text (or (json-get item "step")
                                         (json-get item "text"))
                               :status (json-get item "status")))
                       raw-steps)))))
         (let* ((configuration (tool-context-configuration context))
                (plan
                  (if steps
                      (plan-update configuration
                                   steps
                                   :explanation
                                   (and (stringp explanation) explanation))
                      (progn
                        (plan-clear configuration)
                        nil))))
           (tool-success (plan-render plan))))
    (plan-error (condition)
      (error 'tool-error
             :message (format nil "~A" condition)
             :tool-name "plan.update"))))
