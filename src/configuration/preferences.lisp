(in-package #:autolith)

;;;; -- Durable Settings --

(defparameter *preferences-version* 8
  "The readable global preferences file format version.

Version 8 stores durable settings as one flat property list keyed by setting
name; unknown keys survive rewrites so releases can share one file.")

(-> preferences--form-p (t) boolean)
(defun preferences--form-p (form)
  "Return true when FORM is one complete version 8 preferences record."
  (and (consp form)
       (eq (first form) ':preferences)
       (listp (rest form))
       (eql (getf (rest form) :version) *preferences-version*)
       (loop for (key nil) on (rest form) by #'cddr
             always (keywordp key))
       t))

(-> preferences--form->plist (list) list)
(defun preferences--form->plist (form)
  "Return the durable value plist carried by a version 8 FORM."
  (loop for (key value) on (rest form) by #'cddr
        unless (eq key ':version)
          append (list key value)))

(-> preferences--plist->form (list) list)
(defun preferences--plist->form (plist)
  "Return the version 8 record holding PLIST."
  (list* ':preferences ':version *preferences-version* plist))

(-> preferences--read (configuration) list)
(defun preferences--read (configuration)
  "Read CONFIGURATION's durable values from its version 8 preferences file."
  (block nil
    (let ((pathname (configuration-preferences-path configuration)))
      (unless (probe-file pathname)
        (return nil))
      (handler-case
          (multiple-value-bind (form sole-form-p)
              (snapshot-read pathname)
            (if (and sole-form-p (preferences--form-p form))
                (preferences--form->plist form)
                (error 'preferences-error
                       :message (format nil "Preferences at ~A are malformed or unsupported."
                                        pathname)
                       :pathname pathname
                       :operation ':read
                       :cause nil)))
        (preferences-error (condition)
          (error condition))
        (error (cause)
          (error 'preferences-error
                 :message (format nil "Could not read preferences at ~A: ~A"
                                  pathname cause)
                 :pathname pathname
                 :operation ':read
                 :cause cause))))))

(-> preferences--write (configuration list) null)
(defun preferences--write (configuration plist)
  "Atomically write PLIST as CONFIGURATION's version 8 preferences file."
  (let ((pathname (configuration-preferences-path configuration)))
    (handler-case
        (progn
          (ensure-directories-exist pathname)
          (snapshot-write pathname (preferences--plist->form plist)))
      (error (cause)
        (error 'preferences-error
               :message (format nil "Could not persist preferences at ~A: ~A"
                                pathname cause)
               :pathname pathname
               :operation ':write
               :cause cause))))
  nil)

(-> preferences-load-values (configuration) list)
(defun preferences-load-values (configuration)
  "Return CONFIGURATION's persisted durable values.

Report malformed or unsupported files through PREFERENCES-LOAD-WARNING and
return NIL."
  (with-lock-held ((configuration-preferences-lock configuration))
    (handler-case
        (preferences--read configuration)
      (preferences-error (condition)
        (warn 'preferences-load-warning
              :pathname (preferences-error-pathname condition)
              :cause condition)
        nil))))

(-> preferences-store (configuration keyword t) null)
(defun preferences-store (configuration name value)
  "Persist VALUE under NAME, merging into whatever the file currently holds.

The file is re-read before writing so values another process stored since
this one started are kept."
  (with-lock-held ((configuration-preferences-lock configuration))
    (let ((plist (handler-case (preferences--read configuration)
                   (preferences-error ()
                     nil))))
      (setf (getf plist name) value)
      (preferences--write configuration plist)))
  nil)

(setf *configuration-durable-values-function* 'preferences-load-values
      *configuration-persist-function* 'preferences-store)
