(in-package #:autolith)

;;;; -- Anthropic API Key Credential Source --

;;; Anthropic authenticates with a static account API key sent as the
;;; x-api-key header rather than a bearer token. The key rides in the
;;; generic oauth-credentials access-token slot so transport and redaction
;;; treat it exactly like any other provider credential. Keys carry no
;;; expiry, so Autolith never attempts a refresh; a rejected key fails as
;;; unauthorized and the user re-runs `autolith --auth anthropic`.

(defparameter *anthropic-account-label* "anthropic"
  "The synthetic account identifier pinned for static Anthropic API keys.")

(defparameter *anthropic-environment-variable* "ANTHROPIC_API_KEY"
  "The environment variable holding the Anthropic account API key.")

(defclass anthropic-environment-credential-source (credential-source)
  ()
  (:documentation
   "A read-only adapter loading the Anthropic API key from the environment."))

(defmethod credential-source-pathname
    ((source anthropic-environment-credential-source))
  "Report a conventional key path because the environment has no pathname."
  (declare (ignore source))
  (merge-pathnames "anthropic-auth.sexp"
                   (environment-directory
                    "XDG_STATE_HOME"
                    (merge-pathnames ".local/state/autolith/"
                                     (user-homedir-pathname)))))

(defmethod credential-source-label
    ((source anthropic-environment-credential-source))
  "Name the Anthropic environment source in user-visible failures."
  (declare (ignore source))
  "the ANTHROPIC_API_KEY environment variable")

(defmethod credential-source-load
    ((source anthropic-environment-credential-source))
  "Load the Anthropic API key from ANTHROPIC_API_KEY, or return NIL."
  (let ((key (uiop:getenv *anthropic-environment-variable*)))
    (when (non-empty-string-p key)
      (make-instance 'oauth-credentials
                     :access-token key
                     :refresh-token nil
                     :id-token nil
                     :account-id *anthropic-account-label*
                     :expires-at nil
                     :source-path (credential-source-pathname source)))))

(defmethod credential-source-save
    ((source anthropic-environment-credential-source)
     (credentials oauth-credentials))
  "Reject writes to the Anthropic environment source."
  (declare (ignore credentials))
  (error 'authentication-error
         :message "The ANTHROPIC_API_KEY environment source is read-only."))


;;;; -- Anthropic Credential Manager --

(defclass anthropic-credential-manager (api-key-credential-manager)
  ()
  (:documentation
   "The static API key credential manager behind the Anthropic provider."))

(defmethod credential-manager-provider-label
    ((manager anthropic-credential-manager))
  "Name the Anthropic account service in user-visible failures."
  (declare (ignore manager))
  "Anthropic")

(-> anthropic-credential-manager-create (configuration) anthropic-credential-manager)
(defun anthropic-credential-manager-create (configuration)
  "Create the Anthropic credential manager for CONFIGURATION's key store."
  (make-instance
   'anthropic-credential-manager
   :primary-source
   (make-instance 'api-key-credential-source
                  :pathname (configuration-api-keys-path configuration)
                  :provider-name "anthropic")
   :bootstrap-source
   (make-instance 'anthropic-environment-credential-source)))

(defmethod credential-manager-load ((manager anthropic-credential-manager))
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
                (format nil "No Anthropic API key is available; ~A."
                        (credential-manager-login-hint manager))
                :searched-paths
                (list (credential-source-pathname
                       (credential-manager-primary-source manager))))))))


;;;; -- Anthropic API Key Validation --

(-> anthropic-validate-api-key (string) null)
(defun anthropic-validate-api-key (key)
  "Probe the Anthropic models endpoint with KEY, signaling on rejection."
  (handler-case
      (dexador:get
       *anthropic-models-endpoint*
       :headers (list (cons "x-api-key" key)
                      (cons "anthropic-version" *anthropic-api-version*)
                      (cons "User-Agent" (provider-user-agent)))
       :force-string t
       :keep-alive nil
       :connect-timeout 30
       :read-timeout 60)
    (dexador.error:http-request-unauthorized ()
      (error 'authentication-error
             :message "Anthropic rejected the entered API key."))
    (http-request-failed (condition)
      (error 'authentication-error
             :message (format nil
                              "The Anthropic API key validation failed (HTTP ~D)."
                              (response-status condition))))
    (error (condition)
      (if (typep condition 'authentication-error)
          (error condition)
          (error 'authentication-error
                 :message
                 "The Anthropic API key could not be validated; check the network."))))
  nil)

(-> anthropic-api-key-login
    (anthropic-credential-manager &key (:stream t))
    string)
(defun anthropic-api-key-login (manager &key (stream *standard-output*))
  "Prompt for, validate, and store the Anthropic API key."
  (format stream
          "~&The key is stored in Autolith's private credential store; ~
           the ~A environment variable overrides it.~%"
          *anthropic-environment-variable*)
  (finish-output stream)
  (call-with-secret-use
   (lambda ()
     (let ((key (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (or (openai-compatible--read-api-key "Anthropic" stream)
                                 ""))))
       (unless (non-empty-string-p key)
         (error 'authentication-error
                :message "No Anthropic API key was entered."))
       (anthropic-validate-api-key key)
       (api-key-credential-manager-save-key manager key)
       "Anthropic authentication was saved by Autolith."))))
