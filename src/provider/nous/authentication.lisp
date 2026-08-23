(in-package #:autolith)

;;;; -- Nous OAuth Credential Management --

;;; Nous refresh tokens rotate on every exchange and are single-use. Autolith
;;; therefore serializes refresh and login publication across processes sharing
;;; one state root, and always reloads the latest credential record while the
;;; filesystem lock is held.

(defvar *nous-auth-store-lock*
  (make-lock "Autolith Nous OAuth store")
  "The in-process lock protecting Nous credential publication and rotation.")

(defclass nous-credential-manager (credential-manager)
  ((refresh-request-function
    :initarg :refresh-request-function
    :reader nous-credential-manager-refresh-request-function
    :type function
    :documentation "The injected HTTP request function used for token refresh."))
  (:documentation "The OAuth credential manager for Nous Research inference."))


;;;; -- Store Lock --

(-> nous-authentication--lock-pathname (pathname) pathname)
(defun nous-authentication--lock-pathname (credential-pathname)
  "Return the process-shared lock pathname for CREDENTIAL-PATHNAME."
  (merge-pathnames
   "nous-auth.lock"
   (uiop:pathname-directory-pathname credential-pathname)))

(-> nous-authentication--call-with-store-lock (pathname function) t)
(defun nous-authentication--call-with-store-lock (credential-pathname function)
  "Call FUNCTION while holding the process and filesystem Nous OAuth locks."
  (let ((lock-pathname
          (nous-authentication--lock-pathname credential-pathname))
        (descriptor nil))
    (handler-case
        (progn
          (ensure-directories-exist lock-pathname)
          (with-lock-held (*nous-auth-store-lock*)
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
               (format nil "Could not lock the Nous OAuth store at ~A: ~A"
                       lock-pathname cause))))))


;;;; -- Access Token Validation --

(-> nous-authentication--scope-values (t) list)
(defun nous-authentication--scope-values (value)
  "Return normalized OAuth scope strings represented by VALUE."
  (labels ((collect-scopes (candidate)
             (cond
               ((stringp candidate)
                (remove-if-not
                 #'non-empty-string-p
                 (uiop:split-string
                  candidate
                  :separator '(#\Space #\Tab #\Newline #\Return #\,))))
               ((vectorp candidate)
                (loop for item across candidate
                      append (collect-scopes item)))
               ((listp candidate)
                (loop for item in candidate
                      append (collect-scopes item)))
               (t
                nil))))
    (remove-duplicates (collect-scopes value) :test #'string=)))

(-> nous-authentication--access-token-scope-p (string string) boolean)
(defun nous-authentication--access-token-scope-p (access-token required-scope)
  "Return true when ACCESS-TOKEN is a JWT carrying REQUIRED-SCOPE."
  (let ((payload (jwt-payload access-token)))
    (if (and payload
             (member required-scope
                     (append
                      (nous-authentication--scope-values
                       (json-get payload "scope"))
                      (nous-authentication--scope-values
                       (json-get payload "scp")))
                     :test #'string=))
        t
        nil)))

(-> nous-authentication--access-token-account-id (string) (option string))
(defun nous-authentication--access-token-account-id (access-token)
  "Return the stable subject carried by a valid Nous access JWT."
  (jwt-subject access-token))

(-> nous-authentication--validate-stored-credentials
    (nous-credential-manager oauth-credentials)
    oauth-credentials)
(defun nous-authentication--validate-stored-credentials (manager credentials)
  "Validate stored Nous CREDENTIALS before returning them to request scope."
  (let* ((access-token (oauth-credentials-access-token credentials))
         (token-account
           (nous-authentication--access-token-account-id access-token)))
    (unless (and (jwt-payload access-token)
                 (nous-authentication--access-token-scope-p
                  access-token
                  *nous-oauth-scope*)
                 (non-empty-string-p token-account)
                 (string= token-account
                          (oauth-credentials-account-id credentials)))
      (error 'credentials-unavailable
             :message
             (format nil
                     "The stored Nous OAuth credentials cannot invoke inference; ~A."
                     (credential-manager-login-hint manager))
             :searched-paths
             (list (oauth-credentials-source-path credentials))))
    credentials))


;;;; -- Credential Manager Protocol --

(defmethod credential-manager-provider-label ((manager nous-credential-manager))
  "Name the Nous Research account service in user-visible failures."
  (declare (ignore manager))
  "Nous Research")

(defmethod credential-manager-login-hint ((manager nous-credential-manager))
  "Point Nous credential failures at the browser login command."
  (declare (ignore manager))
  "run autolith auth nous")

(-> nous-credential-manager-create
    (configuration &key (:refresh-request-function function))
    nous-credential-manager)
(defun nous-credential-manager-create
    (configuration &key
                     (refresh-request-function #'nous-authentication--request))
  "Create a Nous credential manager for CONFIGURATION's private state root."
  (make-instance
   'nous-credential-manager
   :primary-source
   (make-instance
    'autolith-credential-source
    :pathname (configuration-nous-auth-path configuration))
   :refresh-request-function refresh-request-function))

(defmethod credential-manager-load ((manager nous-credential-manager))
  "Load only Autolith-owned Nous credentials under the shared store lock."
  (let* ((source (credential-manager-primary-source manager))
         (pathname (credential-source-pathname source)))
    (nous-authentication--call-with-store-lock
     pathname
     (lambda ()
       (let ((credentials (credential-source-load source)))
         (unless credentials
           (error 'credentials-unavailable
                  :message
                  (format nil "No Nous Research OAuth credentials are available; ~A."
                          (credential-manager-login-hint manager))
                  :searched-paths (list pathname)))
         (credential-manager-accept-account
          manager
          (nous-authentication--validate-stored-credentials
           manager
           credentials)))))))


;;;; -- Refresh Exchange --

(-> nous-authentication--request
    (&key (:method keyword)
          (:url string)
          (:headers list)
          (:content string))
    (values string integer t))
(defun nous-authentication--request (&key method url headers content)
  "Perform one Nous OAuth HTTP request and return body, status, and headers."
  (unless (eq method ':post)
    (error 'authentication-error
           :message "Nous OAuth transport supports only HTTP POST requests."))
  (handler-case
      (multiple-value-bind (body status response-headers)
          (dexador:post url
                        :headers headers
                        :content content
                        :force-string t
                        :keep-alive nil
                        :connect-timeout 30
                        :read-timeout 60)
        (values body status response-headers))
    (http-request-failed (condition)
      (values (or (response-body condition) "")
              (response-status condition)
              (response-headers condition)))))

(-> nous-authentication--credential-version-different-p
    (oauth-credentials oauth-credentials)
    boolean)
(defun nous-authentication--credential-version-different-p (left right)
  "Return true when LEFT and RIGHT represent different token rotations."
  (if (or (not (string= (oauth-credentials-access-token left)
                        (oauth-credentials-access-token right)))
          (not (equal (oauth-credentials-refresh-token left)
                      (oauth-credentials-refresh-token right))))
      t
      nil))

(-> nous-refresh-response-credentials
    (nous-credential-manager oauth-credentials string)
    oauth-credentials)
(defun nous-refresh-response-credentials (manager credentials body)
  "Validate refresh BODY and return account-continuous Nous credentials."
  (handler-case
      (let ((response (json-decode body)))
        (unless (json-object-p response)
          (error "The Nous OAuth refresh root is not an object."))
        (let* ((access-token (json-get response "access_token"))
               (refresh-token (json-get response "refresh_token"))
               (id-token (json-get response "id_token"))
               (expires-in (json-get response "expires_in"))
               (previous-refresh-token
                 (oauth-credentials-refresh-token credentials)))
          (unless (and (non-empty-string-p access-token)
                       (non-empty-string-p refresh-token)
                       (not (string= refresh-token previous-refresh-token))
                       (or (null id-token) (non-empty-string-p id-token)))
            (error "The Nous OAuth refresh response omitted rotated credentials."))
          (unless (nous-authentication--access-token-scope-p
                   access-token
                   *nous-oauth-scope*)
            (error 'token-refresh-failed
                   :message
                   "The refreshed Nous access token lacks the inference:invoke scope."
                   :status nil
                   :response nil))
          (let ((account-id
                  (nous-authentication--access-token-account-id access-token))
                (previous-account
                  (oauth-credentials-account-id credentials)))
            (unless (non-empty-string-p account-id)
              (error "The refreshed Nous access token omitted its subject."))
            (unless (string= account-id previous-account)
              (error 'token-refresh-failed
                     :message "The Nous OAuth refresh response changed accounts."
                     :status nil
                     :response nil))
            (make-instance
             'oauth-credentials
             :access-token access-token
             :refresh-token refresh-token
             :id-token id-token
             :account-id account-id
             :expires-at
             (or (and (integerp expires-in)
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
             :message "The Nous OAuth refresh response was malformed."
             :status nil
             :response nil))))

(-> nous-authentication--redacted-error-code
    (string oauth-credentials)
    (option string))
(defun nous-authentication--redacted-error-code (body credentials)
  "Return BODY's bounded OAuth error code without credential material."
  (let ((code (oauth-error-code body))
        (secrets (oauth-credentials-secret-values credentials)))
    (and code
         (redact-exact-string-values
          code
          secrets
          (safe-redaction-marker "[OAUTH CREDENTIAL REDACTED]" secrets)))))

(defmethod credential-manager-refresh-exchange
    ((manager nous-credential-manager)
     (credentials oauth-credentials)
     (refresh-token string))
  "Rotate Nous credentials atomically across processes sharing the state root."
  (declare (ignore refresh-token))
  (let* ((source (credential-manager-primary-source manager))
         (pathname (credential-source-pathname source)))
    (nous-authentication--call-with-store-lock
     pathname
     (lambda ()
       (let ((latest (credential-source-load source)))
         (when (and latest
                    (nous-authentication--credential-version-different-p
                     latest
                     credentials))
           (return-from credential-manager-refresh-exchange
             (values
              (credential-manager-accept-account
               manager
               (nous-authentication--validate-stored-credentials manager latest))
              nil)))
         (let* ((effective (or latest credentials))
                (effective-refresh-token
                  (oauth-credentials-refresh-token effective)))
           (unless (non-empty-string-p effective-refresh-token)
             (error 'token-refresh-failed
                    :message
                    (format nil "These Nous credentials cannot refresh; ~A."
                            (credential-manager-login-hint manager))
                    :status nil
                    :response nil))
           (multiple-value-bind (body status response-headers)
               (handler-case
                   (funcall
                    (nous-credential-manager-refresh-request-function manager)
                    :method ':post
                    :url (concatenate 'string
                                      (nous-portal-url)
                                      "/api/oauth/token")
                    :headers
                    (list
                     (cons "x-nous-refresh-token" effective-refresh-token)
                     (cons "Content-Type" "application/x-www-form-urlencoded")
                     (cons "Accept" "application/json")
                     (cons "User-Agent" (device-authentication--user-agent)))
                    :content
                    (url-encode-params
                     (list
                      (cons "grant_type" "refresh_token")
                      (cons "client_id" *nous-oauth-client-id*))))
                 (authentication-error (condition)
                   (error condition))
                 (error ()
                   (error 'token-refresh-failed
                          :message "Nous OAuth token refresh could not be completed."
                          :status nil
                          :response nil)))
             (declare (ignore response-headers))
             (unless (and (stringp body) (integerp status))
               (error 'token-refresh-failed
                      :message "The Nous OAuth refresh transport returned an invalid response."
                      :status nil
                      :response nil))
             (if (<= 200 status 299)
                 (let ((refreshed
                         (credential-manager-accept-account
                          manager
                          (nous-refresh-response-credentials
                           manager
                           effective
                           body))))
                   (credential-source-save source refreshed)
                   (values refreshed nil))
                 (let* ((code
                          (nous-authentication--redacted-error-code body effective))
                        (newer (credential-source-load source)))
                   (if (and newer
                            (nous-authentication--credential-version-different-p
                             newer
                             effective))
                       (values
                        (credential-manager-accept-account
                         manager
                         (nous-authentication--validate-stored-credentials
                          manager
                          newer))
                        nil)
                       (error 'token-refresh-failed
                              :message
                              (format nil
                                      "Nous OAuth token refresh failed~@[ (~A)~]; ~A."
                                      code
                                      (credential-manager-login-hint manager))
                              :status status
                              :response code)))))))))))
