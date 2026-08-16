(in-package #:autolith)

;;;; -- Anthropic API Key Authentication --

;;; Anthropic authenticates with a static account API key sent as the
;;; x-api-key header rather than a bearer token. The key rides in the
;;; generic oauth-credentials access-token slot so transport and redaction
;;; treat it exactly like any other provider credential. Keys carry no
;;; expiry, so Autolith never attempts a refresh; a rejected key fails as
;;; unauthorized and the user re-runs `autolith auth anthropic`.

(defparameter *anthropic-account-label* "anthropic"
  "The synthetic account identifier pinned for static Anthropic API keys.")

(defparameter *anthropic-environment-variable* "ANTHROPIC_API_KEY"
  "The environment variable holding the Anthropic account API key.")

(defclass anthropic-environment-credential-source
    (environment-api-key-credential-source)
  ()
  (:default-initargs
   :environment-variable *anthropic-environment-variable*
   :account-id *anthropic-account-label*)
  (:documentation
   "A read-only adapter loading the Anthropic API key from the environment."))


;;;; -- Anthropic Credential Manager --

(defclass anthropic-credential-manager (static-api-key-credential-manager)
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


;;;; -- Anthropic API Key Validation --

(-> anthropic-validate-api-key (string) null)
(defun anthropic-validate-api-key (key)
  "Probe the Anthropic models endpoint with KEY, signaling on rejection."
  (api-key-validate-probe
   "Anthropic"
   (lambda ()
     (dexador:get
      *anthropic-models-endpoint*
      :headers (list (cons "x-api-key" key)
                     (cons "anthropic-version" *anthropic-api-version*)
                     (cons "User-Agent" (provider-user-agent)))
      :force-string t
      :keep-alive nil
      :connect-timeout 30
      :read-timeout 60))))

(-> anthropic-api-key-login
    (anthropic-credential-manager &key (:stream t))
    string)
(defun anthropic-api-key-login (manager &key (stream *standard-output*))
  "Prompt for, validate, and store the Anthropic API key."
  (api-key-login manager
                 :stream stream
                 :validate #'anthropic-validate-api-key))
