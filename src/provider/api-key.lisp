(in-package #:autolith)

;;;; -- Persistent API-Key Credential Sources --

(defparameter *api-key-store-version* 1
  "The portable version of Autolith's OpenAI-compatible API-key store.")

(defvar *api-key-store-lock*
  (make-lock "Autolith API-key store")
  "The lock protecting in-process API-key store updates and reads.")

(defclass api-key-credential-source (autolith-credential-source)
  ((provider-name
    :initarg :provider-name
    :reader api-key-credential-source-provider-name
    :type non-empty-string
    :documentation "The provider name associated with this key source."))
  (:documentation
   "A private persistent credential source for one API-key provider."))

(-> api-key--canonical-provider-name (string) string)
(defun api-key--canonical-provider-name (provider-name)
  "Return the case-insensitive store key for PROVIDER-NAME."
  (string-downcase provider-name))

(-> api-key--call-with-store-lock (pathname function) t)
(defun api-key--call-with-store-lock (pathname function)
  "Call FUNCTION while holding the process and filesystem API-key locks."
  (let ((lock-pathname
          (merge-pathnames
           "api-keys.lock"
           (uiop:pathname-directory-pathname pathname)))
        (descriptor nil))
    (handler-case
        (progn
          (ensure-directories-exist lock-pathname)
          (with-lock-held (*api-key-store-lock*)
            (setf descriptor
                  (sb-posix:open
                   (namestring lock-pathname)
                   (logior sb-posix:o-creat sb-posix:o-rdwr)
                   #o600))
            (unwind-protect
                 (progn
                   (sb-posix:lockf descriptor sb-posix:f-lock 0)
                   (funcall function))
              (ignore-errors (sb-posix:lockf descriptor sb-posix:f-ulock 0))
              (ignore-errors (sb-posix:close descriptor))
              (setf descriptor nil))))
      (authentication-error (condition)
        (error condition))
      (error (cause)
        (error 'authentication-error
               :message
               (format nil "Could not lock the API-key store at ~A: ~A"
                       lock-pathname cause))))))

(-> api-key--invalid-store (pathname) null)
(defun api-key--invalid-store (pathname)
  "Signal that PATHNAME is not a valid private API-key store."
  (error 'authentication-error
         :message (format nil "Invalid Autolith API-key store at ~A." pathname)))

(-> api-key--entry-provider-name (list) (option string))
(defun api-key--entry-provider-name (entry)
  "Return the provider name from one validated or unvalidated ENTRY."
  (and (listp entry)
       (getf (rest entry) :provider-name)))

(-> api-key--entry-key (list) (option string))
(defun api-key--entry-key (entry)
  "Return the secret key from one validated or unvalidated ENTRY."
  (and (listp entry)
       (getf (rest entry) :api-key)))

(-> api-key--validate-entries (list pathname) list)
(defun api-key--validate-entries (entries pathname)
  "Validate API-key ENTRIES read from PATHNAME and return them unchanged."
  (unless (listp entries)
    (api-key--invalid-store pathname))
  (let ((seen nil))
    (dolist (entry entries)
      (let ((provider-name (api-key--entry-provider-name entry))
            (api-key (api-key--entry-key entry)))
        (unless (and (listp entry)
                     (eq (first entry) :provider)
                     (non-empty-string-p provider-name)
                     (non-empty-string-p api-key))
          (api-key--invalid-store pathname))
        (let ((canonical-name (api-key--canonical-provider-name provider-name)))
          (when (member canonical-name seen :test #'string=)
            (api-key--invalid-store pathname))
          (push canonical-name seen))))
    entries))

(-> api-key--read-entries (pathname) list)
(defun api-key--read-entries (pathname)
  "Read and validate the provider entries in PATHNAME, or return NIL when absent."
  (if (not (probe-file pathname))
      nil
      (handler-case
          (let* ((record (read-portable-form pathname))
                 (version (and (listp record) (getf (rest record) :version)))
                 (entries (and (listp record)
                               (eq (first record) :api-keys)
                               (getf (rest record) :providers))))
            (unless (and (listp record)
                         (eq (first record) :api-keys)
                         (= version *api-key-store-version*))
              (api-key--invalid-store pathname))
            (api-key--validate-entries entries pathname))
        (authentication-error (condition)
          (error condition))
        (error ()
          (api-key--invalid-store pathname)))))

(-> api-key--entry-for (string list) (option list))
(defun api-key--entry-for (provider-name entries)
  "Find the API-key entry for PROVIDER-NAME in ENTRIES."
  (find (api-key--canonical-provider-name provider-name)
        entries
        :key (lambda (entry)
               (api-key--canonical-provider-name
                (api-key--entry-provider-name entry)))
        :test #'string=))

(defmethod credential-source-label ((source api-key-credential-source))
  "Name the API-key provider in credential failures."
  (api-key-credential-source-provider-name source))

(defmethod credential-source-load ((source api-key-credential-source))
  "Load SOURCE's API key into request scope, without retaining it in the image."
  (let ((pathname (credential-source-pathname source)))
    (api-key--call-with-store-lock
     pathname
     (lambda ()
       (let* ((entry (api-key--entry-for
                      (api-key-credential-source-provider-name source)
                      (api-key--read-entries pathname)))
              (access-token (and entry (api-key--entry-key entry))))
         (when (non-empty-string-p access-token)
           (make-instance
            'oauth-credentials
            :access-token access-token
            :refresh-token nil
            :id-token nil
            :account-id
            (format nil "api-key/~A"
                    (api-key--canonical-provider-name
                     (api-key-credential-source-provider-name source)))
            :expires-at nil
            :source-path pathname)))))))

(defmethod credential-source-save ((source api-key-credential-source)
                                   (credentials oauth-credentials))
  "Atomically save CREDENTIALS' API key in SOURCE's private store."
  (call-with-secret-use
   (lambda ()
     (let ((api-key (oauth-credentials-access-token credentials)))
       (unless (non-empty-string-p api-key)
         (error 'authentication-error
                :message (format nil "The ~A API key is empty."
                                 (credential-source-label source))))
       (let* ((pathname (credential-source-pathname source))
              (provider-name
                (api-key--canonical-provider-name
                 (api-key-credential-source-provider-name source))))
         (api-key--call-with-store-lock
          pathname
          (lambda ()
            (let* ((entries (api-key--read-entries pathname))
                   (updated-entry
                     (list :provider
                           :provider-name provider-name
                           :api-key api-key)))
              (ensure-directories-exist pathname)
              (snapshot-write
               pathname
               (list :api-keys
                     :version *api-key-store-version*
                     :providers
                     (append
                      (remove-if
                       (lambda (entry)
                         (string= provider-name
                                  (api-key--canonical-provider-name
                                   (api-key--entry-provider-name entry))))
                       entries)
                      (list updated-entry))
               :mode #o600))))))
       credentials))))


;;;; -- Interactive API-Key Input --

(-> api-key--strip-bracketed-paste (string) string)
(defun api-key--strip-bracketed-paste (text)
  "Remove one terminal bracketed-paste wrapper from TEXT."
  (let* ((escape (code-char 27))
         (start  (format nil "~C[200~~" escape))
         (end    (format nil "~C[201~~" escape)))
    (if (and (uiop:string-prefix-p start text)
             (uiop:string-suffix-p text end)
             (>= (length text) (+ (length start) (length end))))
        (subseq text (length start) (- (length text) (length end)))
        text)))

(-> api-key--input-file-descriptor (stream (option integer)) (option integer))
(defun api-key--input-file-descriptor (input configured)
  "Return INPUT's configured or direct file descriptor."
  (or configured
      (ignore-errors (sb-sys:fd-stream-fd input))))

(-> api-key--interactive-file-descriptor-p (integer) boolean)
(defun api-key--interactive-file-descriptor-p (file-descriptor)
  "Return true when FILE-DESCRIPTOR names an interactive terminal."
  (and (not (minusp file-descriptor))
       (let ((result (sb-unix:unix-isatty file-descriptor)))
         (and result (plusp result)))))

(-> api-key--hidden-input-mode (stream (option integer)) (option cons))
(defun api-key--hidden-input-mode (input configured-descriptor)
  "Hide terminal echo for INPUT's known descriptor and return the mode to restore."
  (let ((descriptor
          (api-key--input-file-descriptor input configured-descriptor)))
    (when (null descriptor)
      (error 'authentication-error
             :message
             "Could not identify the input descriptor for API-key entry; no key was read."))
    (when (api-key--interactive-file-descriptor-p descriptor)
      (handler-case
          (let ((saved-mode (sb-posix:tcgetattr descriptor))
                (hidden-mode (sb-posix:tcgetattr descriptor)))
            (setf (sb-posix:termios-lflag hidden-mode)
                  (logandc2 (sb-posix:termios-lflag hidden-mode) sb-posix:echo))
            (sb-posix:tcsetattr descriptor sb-posix:tcsanow hidden-mode)
            (cons descriptor saved-mode))
        (error ()
          (error 'authentication-error
                 :message
                 "Could not disable terminal echo for API-key entry; no key was read."))))))

(-> api-key--restore-input-mode (cons) null)
(defun api-key--restore-input-mode (saved-mode)
  "Restore one terminal mode returned by API-KEY--HIDDEN-INPUT-MODE."
  (sb-posix:tcsetattr (first saved-mode)
                      sb-posix:tcsanow
                      (rest saved-mode))
  nil)

(-> api-key-read-hidden
    (string &key
            (:input stream)
            (:input-file-descriptor (option integer))
            (:stream stream)
            (:note (option string)))
    (option string))
(defun api-key-read-hidden
    (provider-name
     &key
       (input *standard-input*)
       input-file-descriptor
       (stream *standard-output*)
       note)
  "Clearly prompt for PROVIDER-NAME's API key and read it without terminal echo."
  (format stream
          "~&╭─ ~A authentication~%~
             │ Paste the ~A API key below, then press Enter.~%~
             │ Input is hidden. Nothing will appear while you type or paste.~%"
          provider-name provider-name)
  (when (non-empty-string-p note)
    (format stream "│ ~A~%" note))
  (write-string "╰─ API key › " stream)
  (finish-output stream)
  (let ((saved-mode
          (api-key--hidden-input-mode input input-file-descriptor)))
    (unwind-protect
         (let ((value (read-line input nil nil)))
           (and value (api-key--strip-bracketed-paste value)))
      (when saved-mode
        (api-key--restore-input-mode saved-mode))
      (terpri stream)
      (finish-output stream))))


;;;; -- API-Key Credential Manager --

(defclass api-key-credential-manager (credential-manager)
  ()
  (:documentation "A credential manager for one persistent API-key source."))

(defmethod credential-manager-provider-label ((manager api-key-credential-manager))
  "Name the API-key provider in credential failures."
  (credential-source-label (credential-manager-primary-source manager)))

(defmethod credential-manager-login-hint ((manager api-key-credential-manager))
  "Describe the command that stores this provider's API key."
  (format nil "run autolith --auth ~A to enter it"
          (credential-manager-provider-label manager)))

(defmethod credential-manager-refreshable-p ((manager api-key-credential-manager))
  "Static provider API keys cannot refresh."
  (declare (ignore manager))
  nil)

(-> api-key-credential-manager-create
    (&key
     (:provider-name non-empty-string)
     (:pathname pathname))
    api-key-credential-manager)
(defun api-key-credential-manager-create (&key provider-name pathname)
  "Create an API-key manager backed by PROVIDER-NAME in PATHNAME."
  (let ((source
          (make-instance
           'api-key-credential-source
           :pathname pathname
           :provider-name provider-name)))
    (make-instance 'api-key-credential-manager
                   :primary-source source
                   :bootstrap-source source)))

(-> api-key-credential-manager-save-key
    (api-key-credential-manager non-empty-string)
    oauth-credentials)
(defun api-key-credential-manager-save-key (manager api-key)
  "Save API-KEY for MANAGER and return request-scoped credential metadata."
  (call-with-secret-use
   (lambda ()
     (let* ((source (credential-manager-primary-source manager))
            (credentials
              (make-instance
               'oauth-credentials
               :access-token api-key
               :refresh-token nil
               :id-token nil
               :account-id
               (format nil "api-key/~A"
                       (api-key--canonical-provider-name
                        (api-key-credential-source-provider-name source)))
               :expires-at nil
               :source-path (credential-source-pathname source))))
       (credential-source-save source credentials)))))

(-> api-key-credential-available-p (api-key-credential-manager) boolean)
(defun api-key-credential-available-p (manager)
  "Return true when MANAGER has a stored API key.

The probe uses the same request-scope secret accounting as a provider request and
never retains the resulting credential after this call."
  (handler-case
      (progn
        (call-with-credentials
         manager
         (lambda (credentials)
           (declare (ignore credentials))
           t))
        t)
    (credentials-unavailable () nil)))
