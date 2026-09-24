(in-package #:autolith)

;;;; -- Release Versions --

(defvar *autolith-version*)

(-> version-components (string) list)
(defun version-components (version)
  "Return the leading integer components of VERSION, ignoring any suffix.

Development builds such as 0.50.3-dev.5 compare as their base release."
  (let ((components nil)
        (start 0))
    (loop
      (let* ((end (or (position-if-not #'digit-char-p version :start start)
                      (length version)))
             (component (and (> end start)
                             (parse-integer version :start start :end end))))
        (unless component
          (return))
        (push component components)
        (if (and (< end (length version))
                 (char= (char version end) #\.))
            (setf start (1+ end))
            (return))))
    (nreverse components)))

(-> version< (string string) boolean)
(defun version< (left right)
  "Return true when release LEFT precedes release RIGHT."
  (let ((left-components (version-components left))
        (right-components (version-components right)))
    (loop
      (let ((left-component (or (pop left-components) 0))
            (right-component (or (pop right-components) 0)))
        (cond
          ((< left-component right-component)
           (return t))
          ((> left-component right-component)
           (return nil))
          ((and (null left-components) (null right-components))
           (return nil)))))))


;;;; -- Legacy Configuration Phase --

(defparameter *legacy-configuration-warning-version* "0.52.0"
  "The release from which the legacy configuration API warns on every call.")

(defparameter *legacy-configuration-removal-version* "0.54.0"
  "The release in which the legacy configuration API and its migration must be deleted.")

(deftype legacy-configuration-phase ()
  "How the current release treats the legacy configuration API."
  '(member :silent :warn :due))

(-> legacy-configuration-phase () legacy-configuration-phase)
(defun legacy-configuration-phase ()
  "Return the legacy configuration API phase for the running release.

The API stays silent in 0.51, warns on each call from 0.52, and is due for
deletion from 0.54, where the canary test fails until the code is gone."
  (cond
    ((version< *autolith-version* *legacy-configuration-warning-version*)
     ':silent)
    ((version< *autolith-version* *legacy-configuration-removal-version*)
     ':warn)
    (t
     ':due)))

(-> legacy-configuration-removal-reminder () string)
(defun legacy-configuration-removal-reminder ()
  "Return the message reminding maintainers what the removal release deletes."
  (format nil
          "Autolith ~A reached the legacy configuration removal release ~A. Delete src/configuration/preferences-legacy.lisp with its versions 1 to 7 reader and migration, the deprecated configuration-<knob> readers, configuration-with-*, configuration--clone, the preferences-* shims, and define-deprecated-function once no shim remains, then delete this reminder."
          *autolith-version*
          *legacy-configuration-removal-version*))

(define-condition deprecation-warning (style-warning)
  ((name
    :initarg :name
    :reader deprecation-warning-name
    :type symbol
    :documentation "The legacy function that was called.")
   (replacement
    :initarg :replacement
    :reader deprecation-warning-replacement
    :type string
    :documentation "The supported form that replaces the legacy call."))
  (:report (lambda (condition stream)
             (format stream
                     "~A is deprecated and is deleted in Autolith ~A; use ~A."
                     (deprecation-warning-name condition)
                     *legacy-configuration-removal-version*
                     (deprecation-warning-replacement condition))))
  (:documentation "Signaled by a legacy configuration entry point after its silent phase."))

(-> deprecation-note (symbol string) null)
(defun deprecation-note (name replacement)
  "Warn that legacy NAME was called when the release phase asks for it."
  (unless (eq (legacy-configuration-phase) ':silent)
    (warn 'deprecation-warning :name name :replacement replacement))
  nil)

(defmacro define-deprecated-function (name lambda-list replacement &body body)
  "Define NAME with LAMBDA-LIST as a legacy entry point around BODY.

REPLACEMENT is the supported form named in the warning. BODY may begin with a
documentation string and declarations, which stay at the head of the
definition. The expansion is an ordinary DEFUN that notes the call through
DEPRECATION-NOTE before evaluating the remaining forms, so the function stays
silent in the silent phase and warns afterwards."
  (let* ((documentation (and (stringp (first body)) (rest body) (first body)))
         (forms (if documentation (rest body) body))
         (declarations (loop while (and (consp (first forms))
                                        (eq (first (first forms)) 'declare))
                             collect (pop forms))))
    `(defun ,name ,lambda-list
       ,@(when documentation (list documentation))
       ,@declarations
       (deprecation-note ',name ,replacement)
       ,@forms)))
