(in-package #:autolith)

;;;; -- Fireworks API Key Authentication --

;;; Fireworks authenticates with a static account API key rather than an
;;; OAuth token family. The key rides in the generic oauth-credentials
;;; access-token slot so transport and redaction treat it exactly like a
;;; subscription bearer token. Account scoping never applies, so the slot
;;; holds the fixed label "fireworks". Keys carry no expiry, so Autolith
;;; never attempts a refresh; a rejected key fails as unauthorized and the
;;; user re-runs `autolith auth fireworks`.

(defparameter *fireworks-account-label* "fireworks"
  "The synthetic account identifier pinned for static Fireworks API keys.")

(defparameter *fireworks-environment-variable* "FIREWORKS_API_KEY"
  "The environment variable holding the Fireworks account API key.")

(defclass fireworks-environment-credential-source
    (environment-api-key-credential-source)
  ()
  (:default-initargs
   :environment-variable *fireworks-environment-variable*
   :account-id *fireworks-account-label*)
  (:documentation
   "A read-only adapter loading the Fireworks API key from the environment."))


;;;; -- Fireworks Credential Manager --

(defclass fireworks-credential-manager (static-api-key-credential-manager)
  ()
  (:documentation
   "The static API key credential manager behind the Fireworks provider."))

(defmethod credential-manager-provider-label
    ((manager fireworks-credential-manager))
  "Name the Fireworks account service in user-visible failures."
  (declare (ignore manager))
  "Fireworks")

(defmethod credential-manager-login-hint
    ((manager fireworks-credential-manager))
  "Point Fireworks credential failures at the Fireworks login command."
  (declare (ignore manager))
  "run autolith auth fireworks")

(-> fireworks-credential-manager-create
    (configuration)
    fireworks-credential-manager)
(defun fireworks-credential-manager-create (configuration)
  "Create the Fireworks credential manager for CONFIGURATION's private paths."
  (make-instance 'fireworks-credential-manager
                 :primary-source
                 (make-instance
                  'autolith-credential-source
                  :pathname (configuration-fireworks-auth-path configuration))
                 :bootstrap-source
                 (make-instance 'fireworks-environment-credential-source)))


;;;; -- Fireworks API Key Validation --

(-> fireworks-validate-api-key (string) null)
(defun fireworks-validate-api-key (key)
  "Probe the Fireworks Responses API with KEY, signaling on rejection."
  (api-key-validate-probe
   "Fireworks"
   (lambda ()
     (let ((request
             (json-object
              "model" *default-fireworks-model*
              "input" "Reply with the single word: ok"
              "store" false
              "stream" false)))
       (dexador:post
        (or (uiop:getenv "AUTOLITH_FIREWORKS_PROVIDER_ENDPOINT")
            *fireworks-responses-endpoint*)
        :headers (list (cons "Authorization" (format nil "Bearer ~A" key))
                       (cons "Content-Type" "application/json")
                       (cons "Accept" "application/json")
                       (cons "User-Agent" (provider-user-agent)))
        :content (json-encode-utf8 request)
        :force-string t
        :keep-alive nil
        :connect-timeout 30
        :read-timeout 60)))))

(-> fireworks-api-key-login
    (fireworks-credential-manager &key
                                  (:stream stream)
                                  (:input stream)
                                  (:input-file-descriptor (option integer)))
    string)
(defun fireworks-api-key-login
    (manager
     &key
       (stream *standard-output*)
       (input *standard-input*)
       input-file-descriptor)
  "Prompt for a Fireworks API key, validate it, and save it to MANAGER's store."
  (api-key-login manager
                 :stream stream
                 :input input
                 :input-file-descriptor input-file-descriptor
                 :validate #'fireworks-validate-api-key))
