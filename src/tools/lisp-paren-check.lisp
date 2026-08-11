(in-package #:autolith)

;;;; -- Lisp Parenthesis Check Tool --

(defclass lisp-paren-check-tool (workspace-tool)
  ()
  (:documentation
   "Check workspace Lisp-family files for unmatched or mismatched delimiters."))

(defmethod tool-child-safe-p ((tool lisp-paren-check-tool))
  "Permit bounded Lisp-family source checks inside child agents."
  (declare (ignore tool))
  t)

(defmethod tool-compact-result-visible-p ((tool lisp-paren-check-tool))
  "Keep successful explicit source checks visible in compact presentation."
  (declare (ignore tool))
  t)

(defmethod tool-execute ((tool lisp-paren-check-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Check one recognized Lisp-family file or a directory tree beneath the workspace."
  (declare (ignore tool))
  (let ((requested (tool-argument arguments "path" :required t)))
    (unless (non-empty-string-p requested)
      (error 'tool-error
             :message "lisp.paren-check requires a non-empty string path."
             :tool-name "lisp.paren-check"))
    (multiple-value-bind (success-p content)
        (lisp-paren-check-path
         (workspace-tool-path context requested)
         :readable-roots *workspace-tool-readable-roots*)
      (if success-p
          (tool-success content)
          (tool-failure content)))))
