(in-package #:autolith)

;;;; -- Mistral API Key Authentication --

(defparameter *mistral-account-label* "mistral"
  "The synthetic account identifier pinned for static Mistral API keys.")

(defparameter *mistral-environment-variable* "MISTRAL_API_KEY"
  "The environment variable holding the Mistral account API key.")

(defclass mistral-environment-credential-source
    (environment-api-key-credential-source)
  ()
  (:default-initargs
   :environment-variable *mistral-environment-variable*
   :account-id *mistral-account-label*)
  (:documentation
   "A read-only adapter loading the Mistral key from the environment."))


;;;; -- Mistral Credential Manager --

(defclass mistral-credential-manager (static-api-key-credential-manager)
  ()
  (:documentation
   "The static API key credential manager behind the Mistral provider."))

(defmethod credential-manager-provider-label
    ((manager mistral-credential-manager))
  "Name the Mistral account service in user-visible failures."
  (declare (ignore manager))
  "Mistral")

(-> mistral-credential-manager-create (configuration) mistral-credential-manager)
(defun mistral-credential-manager-create (configuration)
  "Create the Mistral credential manager for CONFIGURATION's key store."
  (make-instance
   'mistral-credential-manager
   :primary-source
   (make-instance 'api-key-credential-source
                  :pathname (configuration-api-keys-path configuration)
                  :provider-name "mistral")
   :bootstrap-source
   (make-instance 'mistral-environment-credential-source)))


;;;; -- Mistral API Key Validation --

(-> mistral-validate-api-key (string) null)
(defun mistral-validate-api-key (key)
  "Probe the configured Mistral models endpoint with KEY."
  (api-key-validate-probe
   "Mistral"
   (lambda ()
     (dexador:get
      (mistral-models-endpoint)
      :headers (list (cons "Authorization" (concatenate 'string "Bearer " key)))
      :force-string t
      :keep-alive nil
      :connect-timeout 30
      :read-timeout 60))))

(-> mistral-api-key-login
    (mistral-credential-manager &key (:stream stream))
    string)
(defun mistral-api-key-login (manager &key (stream *standard-output*))
  "Prompt for, validate, and store the Mistral API key."
  (api-key-login manager
                 :stream stream
                 :validate #'mistral-validate-api-key))
