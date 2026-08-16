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

(defclass grok-credential-manager (credential-manager)
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
                  :pathname (configuration-grok-bootstrap-auth-path
                             configuration))))

(-> grok-refresh-response-credentials
    (grok-credential-manager oauth-credentials string)
    oauth-credentials)
(defun grok-refresh-response-credentials (manager credentials body)
  "Validate refresh BODY and return account-continuous Grok credentials."
  (handler-case
      (let ((response (json-decode body)))
        (unless (json-object-p response)
          (error "The Grok OAuth refresh root is not an object."))
        (let* ((access-token (json-get response "access_token"))
               (response-id-token (json-get response "id_token"))
               (id-token (or response-id-token
                             (oauth-credentials-id-token credentials)))
               (rotated-refresh-token
                 (or (json-get response "refresh_token")
                     (oauth-credentials-refresh-token credentials)))
               (expires-in (json-get response "expires_in")))
          (unless (and (non-empty-string-p access-token)
                       (or (null response-id-token)
                           (non-empty-string-p response-id-token))
                       (non-empty-string-p rotated-refresh-token))
            (error "The Grok OAuth refresh response omitted required fields."))
          (let* ((previous-account
                   (oauth-credentials-account-id credentials))
                 (token-account
                   (or (and id-token (jwt-subject id-token))
                       (jwt-subject access-token)))
                 (account-id (or token-account previous-account)))
            (when (and token-account
                       (not (string= token-account previous-account)))
              (error 'token-refresh-failed
                     :message "The Grok OAuth refresh response changed accounts."
                     :status nil
                     :response nil))
            (make-instance
             'oauth-credentials
             :access-token access-token
             :refresh-token rotated-refresh-token
             :id-token id-token
             :account-id account-id
             :expires-at (or (and (integerp expires-in)
                                  (plusp expires-in)
                                  (+ (get-universal-time) expires-in))
                             (jwt-expiration access-token))
             :source-path
             (credential-source-pathname
              (credential-manager-primary-source manager))))))
    (token-refresh-failed (condition)
      (error condition))
    (error ()
      (error 'token-refresh-failed
             :message "The Grok OAuth refresh response was malformed."
             :status nil
             :response nil))))

(defmethod credential-manager-refresh-exchange
    ((manager grok-credential-manager)
     (credentials oauth-credentials)
     (refresh-token string))
  "Rotate REFRESH-TOKEN at the xAI OAuth token endpoint."
  (handler-case
      (let* ((content (url-encode-params
                       (list
                        (cons "grant_type" "refresh_token")
                        (cons "refresh_token" refresh-token)
                        (cons "client_id" *grok-oauth-client-id*))))
             (body (dexador:post
                    (grok-oauth-token-endpoint)
                    :headers '(("Content-Type"
                                . "application/x-www-form-urlencoded")
                               ("Accept" . "application/json"))
                    :content content
                    :force-string t
                    :connect-timeout 30
                    :read-timeout 60)))
        (values (grok-refresh-response-credentials manager credentials body)
                t))
    (http-request-failed (condition)
      (let* ((body (response-body condition))
             (raw-code (oauth-error-code body))
             (code
               (and
                raw-code
                (let ((secrets
                        (oauth-credentials-secret-values credentials)))
                  (redact-exact-string-values
                   raw-code
                   secrets
                   (safe-redaction-marker
                    "[OAUTH CREDENTIAL REDACTED]"
                    secrets))))))
        (error 'token-refresh-failed
               :message (format nil "Grok OAuth token refresh failed~@[ (~A)~]."
                                code)
               :status (response-status condition)
               :response code)))
    (authentication-error (condition)
      (error condition))
    (error ()
      (error 'token-refresh-failed
             :message "Grok OAuth token refresh could not be completed."
             :status nil
             :response nil))))
