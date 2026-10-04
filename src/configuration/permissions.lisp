(in-package #:autolith)

;;;; -- Persistent Command Permissions --

(defparameter *permissions-version* 1
  "The readable command permission file format version.")

(defclass command-permission ()
  ((command
    :initarg :command
    :reader command-permission-command
    :type non-empty-string
    :documentation "The exact shell command approved by the user.")
   (directory
    :initarg :directory
    :reader command-permission-directory
    :type non-empty-string
    :documentation "The canonical working directory in which COMMAND is approved."))
  (:documentation "One exact persistent shell command approval."))

(defclass permission-state ()
  ((rules
    :initarg :rules
    :initform nil
    :accessor permission-state-rules
    :type list
    :documentation "The exact command and working-directory approvals."))
  (:documentation "Validated persistent command approvals for one user."))

(-> permissions--directory-name ((or pathname string)) string)
(defun permissions--directory-name (directory)
  "Return DIRECTORY as a canonical directory namestring."
  (let ((existing (uiop:directory-exists-p directory)))
    (unless existing
      (error 'permissions-error
             :message (format nil "Permission directory ~A does not exist."
                              directory)
             :pathname (pathname directory)
             :operation ':validate
             :cause nil))
    (namestring (uiop:ensure-directory-pathname
                 (platform-truename *platform* existing)))))

(-> permissions--rule-form-p (t) boolean)
(defun permissions--rule-form-p (form)
  "Return true when FORM is one exact command permission record."
  (and (record-check form
                     :properties-p t
                     :fields (list (list :indicator ':command
                                         :validate #'non-empty-string-p
                                         :required t)
                                   (list :indicator ':directory
                                         :validate #'non-empty-string-p
                                         :required t))
                     :allow-other-keys nil)
       t))

(-> permissions--rules-p (t) boolean)
(defun permissions--rules-p (rules)
  "Return true when RULES is a list of exact command permission records."
  (and (listp rules)
       (integerp (list-length rules))
       (every #'permissions--rule-form-p rules)))

(-> permissions--form-p (t) boolean)
(defun permissions--form-p (form)
  "Return true when FORM is one complete supported permission state."
  (and (record-check form
                     :tag ':permissions
                     :versions (list *permissions-version*)
                     :fields (list (list :indicator ':rules
                                         :validate #'permissions--rules-p
                                         :required t))
                     :allow-other-keys nil)
       t))

(-> permissions--form->state (list) permission-state)
(defun permissions--form->state (form)
  "Return the permission state represented by validated FORM."
  (make-instance
   'permission-state
   :rules (loop for rule in (record-property form ':rules)
                collect (make-instance 'command-permission
                                       :command (copy-seq (getf rule :command))
                                       :directory (copy-seq (getf rule :directory))))))

(-> permissions--store
    (configuration &key (:recover-read-error (or null function)))
    sexp-store:snapshot-store)
(defun permissions--store (configuration &key recover-read-error)
  "Construct CONFIGURATION's validated transactional permission store."
  (make-instance 'sexp-store:snapshot-store
                 :pathname (configuration-permissions-path configuration)
                 :lock-pathname (merge-pathnames
                                  "permissions.lock"
                                  (config :state-root configuration))
                 :initial-state (lambda () (make-instance 'permission-state))
                 :validator #'permissions--form-p
                 :decoder #'permissions--form->state
                 :encoder #'permissions--state-form
                 :recover-read-error recover-read-error))

(-> permissions--read (configuration) permission-state)
(defun permissions--read (configuration)
  "Read CONFIGURATION's command permissions or return an empty state."
  (handler-case
      (sexp-store:store-read (permissions--store configuration))
    (sexp-store:store-error (cause)
      (error 'permissions-error
             :message (format nil "Could not read command permissions at ~A: ~A"
                              (configuration-permissions-path configuration)
                              cause)
             :pathname (configuration-permissions-path configuration)
             :operation ':read
             :cause cause))))

(-> permissions-load (configuration) permission-state)
(defun permissions-load (configuration)
  "Return saved command permissions, warning and denying after corruption."
  (handler-case
      (permissions--read configuration)
    (permissions-error (condition)
      (warn 'permissions-load-warning
            :pathname (permissions-error-pathname condition)
            :cause condition)
      (make-instance 'permission-state))))

(-> permissions--state-form (permission-state) list)
(defun permissions--state-form (state)
  "Return STATE as one portable readable form."
  (list ':permissions
        ':version *permissions-version*
        ':rules
        (loop for rule in (permission-state-rules state)
              collect (list ':command
                            (command-permission-command rule)
                            ':directory
                            (command-permission-directory rule)))))


(-> permissions-allowed-p
    (permission-state string (or pathname string))
    boolean)
(defun permissions-allowed-p (state command directory)
  "Return true when exact COMMAND is permanently approved in DIRECTORY."
  (let ((directory-name (permissions--directory-name directory)))
    (not
     (null
      (find-if (lambda (rule)
                 (and (string= command (command-permission-command rule))
                      (string= directory-name
                               (command-permission-directory rule))))
               (permission-state-rules state))))))

(-> permissions-allow
    (&key
     (:configuration configuration)
     (:state permission-state)
     (:command string)
     (:directory (or pathname string)))
    null)
(defun permissions-allow (&key configuration state command directory)
  "Permanently approve exact COMMAND in DIRECTORY unless already present."
  (unless (non-empty-string-p command)
    (error 'permissions-error
           :message "Cannot approve an empty shell command."
           :pathname (configuration-permissions-path configuration)
           :operation ':validate
           :cause nil))
  (let ((directory-name (permissions--directory-name directory)))
    (handler-case
        (sexp-store:store-transact
         (permissions--store
          configuration
          :recover-read-error (lambda (condition)
                                (declare (ignore condition))
                                (make-instance 'permission-state)))
         (lambda (current)
           (let ((rules (permission-state-rules current)))
             (if (find-if (lambda (rule)
                            (and (string= command
                                         (command-permission-command rule))
                                 (string= directory-name
                                          (command-permission-directory rule))))
                          rules)
                 (values current nil nil)
                 (values
                  (make-instance
                   'permission-state
                   :rules (append rules
                                  (list (make-instance
                                         'command-permission
                                         :command (copy-seq command)
                                         :directory (copy-seq directory-name)))))
                  nil t))))
         :publish (lambda (committed)
                    (setf (permission-state-rules state)
                          (permission-state-rules committed))))
      (sexp-store:store-error (cause)
        (error 'permissions-error
               :message (format nil "Could not persist command permissions at ~A: ~A"
                                (configuration-permissions-path configuration)
                                cause)
               :pathname (configuration-permissions-path configuration)
               :operation ':write
               :cause cause))))
  nil)

(-> permissions-clear (configuration permission-state) null)
(defun permissions-clear (configuration state)
  "Remove every permanently approved shell command."
  (let ((pathname (configuration-permissions-path configuration))
        (replacement (make-instance 'permission-state)))
    (handler-case
        (sexp-store:store-transact
         (permissions--store
          configuration
          :recover-read-error (lambda (condition)
                                (declare (ignore condition))
                                (make-instance 'permission-state)))
         (lambda (current)
           (declare (ignore current))
           (values replacement nil t))
         :publish (lambda (committed)
                    (setf (permission-state-rules state)
                          (permission-state-rules committed))))
      (sexp-store:store-error (cause)
        (error 'permissions-error
               :message (format nil "Could not persist command permissions at ~A: ~A"
                                pathname cause)
               :pathname pathname
               :operation ':write
               :cause cause))))
  nil)
