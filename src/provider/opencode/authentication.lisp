(in-package #:autolith)

;;;; -- OpenCode API Key Credential Source --

;;; OpenCode authenticates with a static account API key sent as a bearer
;;; token. The key rides in the generic oauth-credentials access-token slot so
;;; transport and redaction treat it exactly like any other provider credential.
;;; Keys carry no expiry, so Autolith never attempts a refresh; a rejected key
;;; fails as unauthorized and the user re-runs `autolith --auth opencode`.

(defparameter *opencode-account-label* "opencode"
  "The synthetic account identifier pinned for static OpenCode API keys.")

(defparameter *opencode-environment-variable* "OPENCODE_API_KEY"
  "The environment variable holding the OpenCode account API key.")

(defclass opencode-environment-credential-source (credential-source)
  ()
  (:documentation
   "A read-only adapter loading the OpenCode API key from the environment."))

(defmethod credential-source-pathname
    ((source opencode-environment-credential-source))
  "Report a conventional key path because the environment has no pathname."
  (declare (ignore source))
  (merge-pathnames "opencode-auth.sexp"
                   (environment-directory
                    "XDG_STATE_HOME"
                    (merge-pathnames ".local/state/autolith/"
                                     (user-homedir-pathname)))))

(defmethod credential-source-label
    ((source opencode-environment-credential-source))
  "Name the OpenCode environment source in user-visible failures."
  (declare (ignore source))
  "the OPENCODE_API_KEY environment variable")

(defmethod credential-source-load
    ((source opencode-environment-credential-source))
  "Load the OpenCode API key from OPENCODE_API_KEY, or return NIL."
  (let ((key (uiop:getenv *opencode-environment-variable*)))
    (when (non-empty-string-p key)
      (make-instance 'oauth-credentials
                     :access-token key
                     :refresh-token nil
                     :id-token nil
                     :account-id *opencode-account-label*
                     :expires-at nil
                     :source-path (credential-source-pathname source)))))

(defmethod credential-source-save
    ((source opencode-environment-credential-source)
     (credentials oauth-credentials))
  "Reject writes to the OpenCode environment source."
  (declare (ignore credentials))
  (error 'authentication-error
         :message "The OPENCODE_API_KEY environment source is read-only."))


;;;; -- OpenCode Credential Manager --

(defclass opencode-credential-manager (credential-manager)
  ()
  (:documentation
   "The static API key credential manager behind the OpenCode provider."))

(defmethod credential-manager-provider-label
    ((manager opencode-credential-manager))
  "Name the OpenCode account service in user-visible failures."
  (declare (ignore manager))
  "OpenCode")

(defmethod credential-manager-login-hint
    ((manager opencode-credential-manager))
  "Point OpenCode credential failures at the OpenCode login command."
  (declare (ignore manager))
  "run autolith --auth opencode")

(-> opencode-credential-manager-create (configuration) opencode-credential-manager)
(defun opencode-credential-manager-create (configuration)
  "Create the OpenCode credential manager for CONFIGURATION's private paths."
  (make-instance 'opencode-credential-manager
                 :primary-source
                 (make-instance
                  'autolith-credential-source
                  :pathname (configuration-opencode-auth-path configuration))
                 :bootstrap-source
                 (make-instance 'opencode-environment-credential-source)))

(defmethod credential-manager-load ((manager opencode-credential-manager))
  "Prefer the current environment key, then load the saved interactive key."
  (let ((environment
          (credential-source-load
           (credential-manager-bootstrap-source manager))))
    (credential-manager-accept-account
     manager
     (or environment
         (credential-source-load
          (credential-manager-primary-source manager))
         (error 'credentials-unavailable
                :message
                (format nil "No OpenCode API key is available; ~A."
                        (credential-manager-login-hint manager))
                :searched-paths
                (list (credential-source-pathname
                       (credential-manager-primary-source manager))))))))

(defmethod credential-manager-refreshable-p
    ((manager opencode-credential-manager))
  "Static OpenCode API keys cannot refresh."
  (declare (ignore manager))
  nil)

(defmethod credential-manager-refresh-exchange
    ((manager opencode-credential-manager)
     (credentials oauth-credentials)
     (refresh-token string))
  "Refuse to refresh a static OpenCode API key; this path is unreachable."
  (declare (ignore credentials refresh-token))
  (error 'token-refresh-failed
         :message (format nil "OpenCode API keys never refresh; ~A."
                          (credential-manager-login-hint manager))
         :status nil
         :response nil))


;;;; -- OpenCode API Key Login --

(-> opencode-api-key-login
    (opencode-credential-manager &key
                                 (:stream stream)
                                 (:input stream)
                                 (:input-file-descriptor (option integer)))
    string)
(defun opencode-api-key-login
    (manager
     &key
       (stream *standard-output*)
       (input *standard-input*)
       input-file-descriptor)
  "Prompt for an OpenCode API key and save it to MANAGER's private store.

OpenCode's model-list endpoint is public, so login cannot validate a key without
issuing a real chat request. A rejected key therefore fails on its first provider
request with the normal actionable static-key authentication error."
  (call-with-secret-use
   (lambda ()
     (let ((key
             (string-trim
              '(#\Space #\Tab #\Newline #\Return)
              (or (api-key-read-hidden
                   "OpenCode"
                   :input input
                   :input-file-descriptor input-file-descriptor
                   :stream stream
                   :note
                   (format nil "~A overrides the stored key when set."
                           *opencode-environment-variable*))
                  ""))))
       (unless (non-empty-string-p key)
         (error 'authentication-error
                :message "No OpenCode API key was entered."))
       (credential-source-save
        (credential-manager-primary-source manager)
        (make-instance 'oauth-credentials
                       :access-token key
                       :refresh-token nil
                       :id-token nil
                       :account-id *opencode-account-label*
                       :expires-at nil
                       :source-path
                       (credential-source-pathname
                        (credential-manager-primary-source manager))))
       "OpenCode authentication was saved by Autolith."))))
