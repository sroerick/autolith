(in-package #:autolith)

;;;; -- Papercut Tool Classes --

(defclass papercut-tool (tool)
  ()
  (:documentation "A tool for recording a user-visible report about an Autolith problem."))

(defclass papercut-report-tool (papercut-tool)
  ()
  (:documentation "Record one new papercut report."))


;;;; -- Tool Results --

(-> papercut-tool--result (papercut) string)
(defun papercut-tool--result (papercut)
  "Return the bounded acknowledgement for a newly recorded PAPERCUT."
  (format nil
          "papercut-id: ~A~%title: ~A"
          (papercut-identifier papercut)
          (papercut-title papercut)))


;;;; -- Tool Executions --

(defmethod tool-conversation-persistence ((tool papercut-report-tool))
  "Keep papercut calls and results only through the next provider response."
  (declare (ignore tool))
  ':next-response)

(defmethod tool-compact-result-visible-p ((tool papercut-report-tool))
  "Keep every successful papercut report visible in compact presentation."
  t)

(defmethod tool-execute ((tool papercut-report-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Record a new papercut from complete supplied title and content."
  (declare (ignore tool))
  (let ((title (tool-argument arguments "title" :required t))
        (content (tool-argument arguments "content" :required t)))
    (unless (and (stringp title) (stringp content))
      (error 'tool-error
             :message "papercut.report requires string title and content."
             :tool-name "papercut.report"))
    (let ((papercut
            (papercut-report
             (tool-context-configuration context)
             :title title
             :content content
             :source-conversation
             (conversation-identifier (tool-context-conversation context)))))
      (tool-success (papercut-tool--result papercut)))))
