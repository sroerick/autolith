(in-package #:autolith)

;;;; -- Grok OAuth Endpoints --

(-> grok-oauth-url (string) string)
(defun grok-oauth-url (path)
  "Return the xAI OAuth issuer joined to absolute PATH."
  (concatenate 'string *grok-oauth-issuer* path))

(-> grok-oauth-token-endpoint () string)
(defun grok-oauth-token-endpoint ()
  "Return the xAI OAuth token endpoint."
  (grok-oauth-url "/oauth2/token"))

(-> grok-auth-json-scope () string)
(defun grok-auth-json-scope ()
  "Return the Grok Build auth.json scope key of the first-party xAI client."
  (format nil "~A::~A" *grok-oauth-issuer* *grok-oauth-client-id*))


;;;; -- Grok Bootstrap Credential Source --

(defclass grok-bootstrap-credential-source (credential-source)
  ()
  (:documentation "A read-only adapter for an existing Grok Build auth.json file."))

(defmethod credential-source-label ((source grok-bootstrap-credential-source))
  "Name the Grok Build bootstrap source in user-visible failures."
  (declare (ignore source))
  "Grok Build")

;; The rotating refresh token is deliberately never imported. Spending it
;; would invalidate Grok Build's own copy and can revoke the whole token
;; family, so Autolith only copies the bounded access token and obtains its
;; own renewable credentials through device authentication.
(defmethod credential-source-load ((source grok-bootstrap-credential-source))
  "Load one non-renewable Grok bootstrap credential without modifying Grok Build."
  (let ((pathname (credential-source-pathname source)))
    (when (probe-file pathname)
      (handler-case
          (let* ((document (read-json-file-with-retry pathname))
                 (record (json-get document (grok-auth-json-scope)))
                 (auth-mode (and (json-object-p record)
                                 (json-get record "auth_mode")))
                 (access-token (and (json-object-p record)
                                    (json-get record "key")))
                 (user-id (and (json-object-p record)
                               (json-get record "user_id")))
                 (account-id (or (and (non-empty-string-p user-id) user-id)
                                 (and (stringp access-token)
                                      (jwt-subject access-token))))
                 (expires-at (and (json-object-p record)
                                  (json-get record "expires_at"))))
            (when (and (stringp auth-mode)
                       (string-equal auth-mode "oidc")
                       (non-empty-string-p access-token)
                       (non-empty-string-p account-id))
              (make-instance 'oauth-credentials
                             :access-token access-token
                             :refresh-token nil
                             :id-token nil
                             :account-id account-id
                             :expires-at (or (and (stringp expires-at)
                                                  (rfc3339->universal-time
                                                   expires-at))
                                             (jwt-expiration access-token))
                             :source-path pathname)))
        (error ()
          nil)))))

(defmethod credential-source-save ((source grok-bootstrap-credential-source)
                                   (credentials oauth-credentials))
  "Reject writes to the Grok Build bootstrap source."
  (declare (ignore credentials))
  (error 'authentication-error
         :message (format nil "The Grok Build bootstrap store ~A is read-only."
                          (credential-source-pathname source))))


;;;; -- Grok Credential Manager --

(defclass grok-credential-manager (oauth-credential-manager)
  ()
  (:documentation "The xAI OAuth credential manager behind the Grok provider."))

(defmethod credential-manager-provider-label ((manager grok-credential-manager))
  "Name the Grok account service in user-visible failures."
  (declare (ignore manager))
  "Grok")

(defmethod credential-manager-login-hint ((manager grok-credential-manager))
  "Point Grok credential failures at the Grok login command."
  (declare (ignore manager))
  "run autolith auth grok")

(-> grok-credential-manager-create (configuration) grok-credential-manager)
(defun grok-credential-manager-create (configuration)
  "Create the Grok credential manager for CONFIGURATION's private paths."
  (make-instance 'grok-credential-manager
                 :primary-source
                 (make-instance
                  'autolith-credential-source
                  :pathname (configuration-grok-auth-path configuration))
                 :bootstrap-source
                 (make-instance
                  'grok-bootstrap-credential-source
                  :pathname (config :grok-bootstrap-auth-path
                             configuration))))

(defmethod credential-manager-token-endpoint ((manager grok-credential-manager))
  "Refresh Grok credentials at the xAI OAuth token endpoint."
  (declare (ignore manager))
  (grok-oauth-token-endpoint))

(defmethod credential-manager-client-id ((manager grok-credential-manager))
  "Refresh as the first-party xAI OAuth client."
  (declare (ignore manager))
  *grok-oauth-client-id*)
