(in-package #:autolith)

;;;; -- OpenRouter API Key Authentication --

(defparameter *openrouter-account-label* "openrouter"
  "The synthetic account identifier pinned for static OpenRouter API keys.")

(defparameter *openrouter-environment-variable* "OPENROUTER_API_KEY"
  "The environment variable holding the OpenRouter API key.")

(defclass openrouter-environment-credential-source
    (environment-api-key-credential-source)
  ()
  (:default-initargs
   :environment-variable *openrouter-environment-variable*
   :account-id *openrouter-account-label*)
  (:documentation
   "A read-only adapter loading the OpenRouter API key from the environment."))


;;;; -- OpenRouter Credential Manager --

(defclass openrouter-credential-manager (static-api-key-credential-manager)
  ()
  (:documentation
   "The static API key credential manager behind the OpenRouter provider."))

(defmethod credential-manager-provider-label
    ((manager openrouter-credential-manager))
  "Name OpenRouter in user-visible credential failures."
  (declare (ignore manager))
  "OpenRouter")

(-> openrouter-credential-manager-create (configuration) openrouter-credential-manager)
(defun openrouter-credential-manager-create (configuration)
  "Create the OpenRouter credential manager for CONFIGURATION's key store."
  (make-instance
   'openrouter-credential-manager
   :primary-source
   (make-instance 'api-key-credential-source
                  :pathname (configuration-api-keys-path configuration)
                  :provider-name "openrouter")
   :bootstrap-source
   (make-instance 'openrouter-environment-credential-source)))


;;;; -- OpenRouter API Key Validation --

(-> openrouter-validate-api-key (string) null)
(defun openrouter-validate-api-key (key)
  "Probe the configured OpenRouter models endpoint with KEY."
  (api-key-validate-probe
   "OpenRouter"
   (lambda ()
     (dexador:get
      (openrouter-models-endpoint)
      :headers (list (cons "Authorization" (concatenate 'string "Bearer " key)))
      :force-string t
      :keep-alive nil
      :connect-timeout 30
      :read-timeout 60))))

(-> openrouter-api-key-login
    (openrouter-credential-manager &key (:stream stream))
    string)
(defun openrouter-api-key-login (manager &key (stream *standard-output*))
  "Prompt for, validate, and store the OpenRouter API key."
  (api-key-login manager
                 :stream stream
                 :validate #'openrouter-validate-api-key))
