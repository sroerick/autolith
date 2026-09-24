(in-package #:autolith)

;;;; -- Setting Protocol --

(deftype setting-scope ()
  "Where a setting's value lives and how long it lasts.

:PROCESS values come from the command line, the environment, or defaults and
never change while the process runs. :DURABLE values persist across processes
in the preferences file. :SESSION values may change while the process runs and
are forgotten at exit. :DERIVED values are computed from other settings."
  '(member :process :durable :session :derived))

(defclass setting ()
  ((name
    :initarg :name
    :reader setting-name
    :type keyword
    :documentation "The keyword naming the setting in CONFIG calls.")
   (type
    :initarg :type
    :initform t
    :reader setting-type
    :type t
    :documentation "The type specifier every stored value satisfies.")
   (documentation
    :initarg :documentation
    :reader setting-documentation
    :type string
    :documentation "The one-sentence explanation shown beside the setting.")
   (label
    :initarg :label
    :reader setting-label
    :type string
    :documentation "The short human name shown on the settings page.")
   (group
    :initarg :group
    :initform ':general
    :reader setting-group
    :type keyword
    :documentation "The settings page section the setting belongs to.")
   (default
    :initarg :default
    :initform nil
    :reader setting-default
    :type t
    :documentation "The default value, or a function of the configuration returning it.")
   (options
    :initarg :options
    :initform nil
    :reader setting-options-designator
    :type t
    :documentation "The permitted values as a list, or a function of the configuration returning them.")
   (scope
    :initarg :scope
    :initform ':process
    :reader setting-scope
    :type setting-scope
    :documentation "Where the value lives; see SETTING-SCOPE.")
   (environment
    :initarg :environment
    :initform nil
    :reader setting-environment
    :type (option string)
    :documentation "The environment variable that supplies the value at process start.")
   (visible-p
    :initarg :visible-p
    :initform t
    :reader setting-visible-p
    :type boolean
    :documentation "Whether the settings page lists the setting."))
  (:documentation "One named, typed, documented configuration value."))

(defclass boolean-setting (setting)
  ()
  (:default-initargs :type 'boolean)
  (:documentation "A setting holding T or NIL, written as on or off."))

(defclass choice-setting (setting)
  ()
  (:documentation "A setting whose value is one of its options."))

(defclass integer-setting (setting)
  ((minimum
    :initarg :minimum
    :initform nil
    :reader setting-minimum
    :type (option integer)
    :documentation "The smallest permitted value, when bounded.")
   (maximum
    :initarg :maximum
    :initform nil
    :reader setting-maximum
    :type (option integer)
    :documentation "The largest permitted value, when bounded."))
  (:default-initargs :type 'integer)
  (:documentation "A setting holding an optionally bounded integer."))

(defclass string-setting (setting)
  ()
  (:default-initargs :type 'string)
  (:documentation "A setting holding a string."))

(defclass pathname-setting (setting)
  ()
  (:default-initargs :type 'pathname)
  (:documentation "A setting holding a pathname."))

(defclass derived-setting (setting)
  ((function
    :initarg :function
    :reader setting-function
    :type (or symbol function)
    :documentation "The function of the configuration computing the value."))
  (:default-initargs :scope ':derived)
  (:documentation "A read-only setting computed from the configuration on each read."))

(-> setting-coerce (setting t t) t)
(defgeneric setting-coerce (setting value configuration)
  (:documentation
   "Return VALUE converted to SETTING's representation for CONFIGURATION.

Environment variables, the settings page, and command arguments supply strings;
coercion turns them into the stored representation without validating them."))

(-> setting-validate (setting t t) null)
(defgeneric setting-validate (setting value configuration)
  (:documentation
   "Signal a CONFIGURATION-ERROR unless VALUE is acceptable for SETTING."))

(-> setting-options (setting t) list)
(defgeneric setting-options (setting configuration)
  (:documentation "Return SETTING's permitted values for CONFIGURATION, or NIL when open."))

(-> setting-render-value (setting t) string)
(defgeneric setting-render-value (setting value)
  (:documentation "Return VALUE as the text the settings page shows for SETTING."))

(defmethod setting-coerce ((setting setting) value configuration)
  "Store VALUE as given."
  (declare (ignore setting configuration))
  value)

(defmethod setting-coerce ((setting boolean-setting) (value string) configuration)
  "Read on, true, yes, and 1 as true and off, false, no, 0, and empty as false."
  (declare (ignore configuration))
  (let ((text (string-downcase (string-trim '(#\Space #\Tab) value))))
    (cond
      ((member text '("on" "true" "yes" "1") :test #'string=)
       t)
      ((member text '("off" "false" "no" "0" "") :test #'string=)
       nil)
      (t
       (error 'configuration-error
              :message (format nil "~A must be on or off, not ~S."
                               (setting-label setting) value))))))

(defmethod setting-coerce ((setting integer-setting) (value string) configuration)
  "Parse VALUE as a decimal integer."
  (declare (ignore configuration))
  (handler-case (parse-integer (string-trim '(#\Space #\Tab) value))
    (error ()
      (error 'configuration-error
             :message (format nil "~A must be an integer, not ~S."
                              (setting-label setting) value)))))

(defmethod setting-coerce ((setting pathname-setting) (value string) configuration)
  "Parse VALUE as a pathname."
  (declare (ignore setting configuration))
  (parse-namestring value))

(defmethod setting-coerce ((setting choice-setting) (value string) configuration)
  "Match VALUE against keyword options case-insensitively, else keep the string."
  (let ((options (setting-options setting configuration)))
    (or (find-if (lambda (option)
                   (and (keywordp option)
                        (string-equal (symbol-name option) value)))
                 options)
        value)))

(defmethod setting-validate ((setting setting) value configuration)
  "Require VALUE to satisfy SETTING's type."
  (declare (ignore configuration))
  (unless (typep value (setting-type setting))
    (error 'configuration-error
           :message (format nil "~A does not accept ~S." (setting-label setting) value)))
  nil)

(defmethod setting-validate ((setting choice-setting) value configuration)
  "Require VALUE to be one of SETTING's options when it declares any.

NIL stays acceptable when SETTING's type admits it, so an optional choice
can be unset without appearing among the options."
  (call-next-method)
  (let ((options (setting-options setting configuration)))
    (when (and options
               (not (and (null value) (typep nil (setting-type setting))))
               (not (member value options :test #'equal)))
      (error 'configuration-error
             :message (format nil "~A must be one of ~{~A~^, ~}, not ~A."
                              (setting-label setting)
                              (mapcar (lambda (option)
                                        (setting-render-value setting option))
                                      options)
                              (setting-render-value setting value)))))
  nil)

(defmethod setting-validate ((setting integer-setting) value configuration)
  "Require VALUE to fall within SETTING's bounds."
  (call-next-method)
  (let ((minimum (setting-minimum setting))
        (maximum (setting-maximum setting)))
    (when (or (and minimum (< value minimum))
              (and maximum (> value maximum)))
      (error 'configuration-error
             :message (format nil "~A must be ~@[at least ~D~]~@[ and ~*~]~@[at most ~D~], not ~D."
                              (setting-label setting)
                              minimum
                              (and minimum maximum)
                              maximum
                              value))))
  nil)

(defmethod setting-options ((setting setting) configuration)
  "Resolve the options designator: a list as is, a function by calling it."
  (let ((designator (setting-options-designator setting)))
    (cond
      ((null designator)
       nil)
      ((functionp designator)
       (funcall designator configuration))
      ((and (symbolp designator) (fboundp designator))
       (funcall designator configuration))
      (t
       designator))))

(defmethod setting-options ((setting boolean-setting) configuration)
  "Offer on and off."
  (declare (ignore setting configuration))
  (list t nil))

(defmethod setting-render-value ((setting setting) value)
  "Show keywords in lower case, pathnames as namestrings, and NIL as unset."
  (declare (ignore setting))
  (cond
    ((null value)
     "unset")
    ((keywordp value)
     (string-downcase (symbol-name value)))
    ((pathnamep value)
     (namestring value))
    (t
     (princ-to-string value))))

(defmethod setting-render-value ((setting boolean-setting) value)
  "Show true as on and false as off."
  (declare (ignore setting))
  (if value "on" "off"))

(-> setting-default-value (setting t) t)
(defun setting-default-value (setting configuration)
  "Return SETTING's default for CONFIGURATION, calling a function default."
  (let ((default (setting-default setting)))
    (if (or (functionp default)
            (and (symbolp default) (not (keywordp default)) default (fboundp default)))
        (funcall default configuration)
        default)))

(-> setting-parse (setting t t) t)
(defun setting-parse (setting text configuration)
  "Return TEXT coerced and validated as a value for SETTING in CONFIGURATION."
  (let ((value (setting-coerce setting text configuration)))
    (setting-validate setting value configuration)
    value))


;;;; -- Setting Registry --

(defvar *settings* (make-ordered-map :test #'eq)
  "Every defined setting by name, in definition order.")

(-> register-setting (setting) setting)
(defun register-setting (setting)
  "Record SETTING in the registry, replacing an earlier definition of its name."
  (ordered-map-set *settings* (setting-name setting) setting)
  setting)

(-> find-setting (keyword &optional t) setting)
(defun find-setting (name &optional (settings *settings*))
  "Return the setting named NAME in SETTINGS, signaling on an unknown name."
  (or (ordered-map-get settings name)
      (error 'configuration-error
             :message (format nil "Unknown setting ~S." name))))

(-> settings-list (&optional t) list)
(defun settings-list (&optional (settings *settings*))
  "Return the settings in SETTINGS in definition order."
  (coerce (ordered-map-values settings) 'list))

(defmacro define-setting (name (class) &rest initargs)
  "Define and register the setting NAME as an instance of CLASS with INITARGS.

NAME is a keyword. The expansion evaluates INITARGS at load time, so option
and default functions may be quoted symbols or lambda forms."
  (check-type name keyword)
  `(register-setting (make-instance ',class :name ,name ,@initargs)))
