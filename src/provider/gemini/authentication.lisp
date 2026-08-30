(in-package #:autolith)

;;;; -- Gemini Installed-App OAuth Conditions --

(define-condition gemini-oauth-error (authentication-error)
  ((stage
    :initarg :stage
    :reader gemini-oauth-error-stage
    :type keyword
    :documentation "The OAuth stage that failed.")
   (status
    :initarg :status
    :initform nil
    :reader gemini-oauth-error-status
    :type (option integer)
    :documentation "The HTTP status returned by Google, if known.")
   (code
    :initarg :code
    :initform nil
    :reader gemini-oauth-error-code
    :type (option string)
    :documentation "A bounded non-secret OAuth error code, if supplied.")
   (response
    :initarg :response
    :initform nil
    :reader gemini-oauth-error-response
    :type (option string)
    :documentation "A bounded redacted OAuth error description, if supplied."))
  (:documentation "A failure in Google installed-application OAuth for Gemini."))


;;;; -- Gemini Credential Management --

(defclass gemini-credential-source (autolith-credential-source)
  ()
  (:documentation "Autolith's private Gemini OAuth credential source."))

(defclass gemini-credential-manager (credential-manager)
  ()
  (:documentation "The Google OAuth credential manager for Gemini subscriptions."))

(defmethod credential-manager-provider-label ((manager gemini-credential-manager))
  "Name Gemini in user-visible credential failures."
  (declare (ignore manager))
  "Gemini")

(defmethod credential-manager-login-hint ((manager gemini-credential-manager))
  "Point Gemini credential failures at its installed-app login function."
  (declare (ignore manager))
  "authenticate with gemini-oauth-login")

(-> configuration-gemini-auth-path (configuration) pathname)
(defun configuration-gemini-auth-path (configuration)
  "Return Autolith's private Gemini OAuth credential pathname."
  (merge-pathnames "gemini-auth.sexp" (configuration-state-root configuration)))

(-> gemini-credential-manager-create (configuration) gemini-credential-manager)
(defun gemini-credential-manager-create (configuration)
  "Create a Gemini credential manager for CONFIGURATION's private store."
  (make-instance 'gemini-credential-manager
                 :primary-source
                 (make-instance 'gemini-credential-source
                                :pathname
                                (configuration-gemini-auth-path configuration))))


;;;; -- PKCE and Request Data --


(-> gemini-oauth--random-hex (integer) string)
(defun gemini-oauth--random-hex (octet-count)
  "Return OCTET-COUNT cryptographically random octets as lowercase hex."
  (with-output-to-string (stream)
    (loop for octet across (random-data octet-count)
          do (format stream "~2,'0x" octet))))

(-> gemini-oauth-create-pkce () (values string string))
(defun gemini-oauth-create-pkce ()
  "Return a fresh 256-bit PKCE verifier and its S256 challenge."
  (oauth--create-pkce :verifier-octets 32))

(-> gemini-oauth-authorization-url
    (&key
     (:redirect-uri string)
     (:state string)
     (:code-challenge string)
     (:client-id string)
     (:authorization-endpoint string))
    string)
(defun gemini-oauth-authorization-url
    (&key
       redirect-uri
       state
       code-challenge
       (client-id (gemini-oauth-client-id))
       (authorization-endpoint *gemini-oauth-authorization-endpoint*))
  "Build the Google installed-app authorization URL for one PKCE flow."
  (format nil "~A?~A"
          authorization-endpoint
          (url-encode-params
           (list
            (cons "client_id" client-id)
            (cons "redirect_uri" redirect-uri)
            (cons "response_type" "code")
            (cons "scope" (format nil "~{~A~^ ~}" *gemini-oauth-scopes*))
            (cons "access_type" "offline")
            (cons "prompt" "consent")
            (cons "code_challenge" code-challenge)
            (cons "code_challenge_method" "S256")
            (cons "state" state)))))


(-> gemini-oauth--redacted-value (t list) (option string))
(defun gemini-oauth--redacted-value (value secrets)
  "Return bounded VALUE after exact secret redaction, or NIL."
  (when (stringp value)
    (let ((bounded (bounded-string value :limit 256)))
      (redact-exact-string-values
       bounded secrets
       (safe-redaction-marker "[OAUTH VALUE REDACTED]" secrets)))))

(-> gemini-oauth--fail
    (&key
     (:stage keyword)
     (:message string)
     (:status (option integer))
     (:code (option string))
     (:response (option string)))
    nil)
(defun gemini-oauth--fail (&key stage message status code response)
  "Signal a structured Gemini OAuth failure containing only safe metadata."
  (error 'gemini-oauth-error
         :message message
         :stage stage
         :status status
         :code code
         :response response))


;;;; -- Loopback Callback --

(-> gemini-oauth-loopback-open () (values sb-bsd-sockets:inet-socket string))
(defun gemini-oauth-loopback-open ()
  "Open an ephemeral IPv4 loopback listener and return it with its redirect URI."
  (handler-case
      (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                     :type ':stream
                                     :protocol ':tcp)))
        (sb-bsd-sockets:socket-bind
         listener
         (sb-bsd-sockets:make-inet-address "127.0.0.1")
         0)
        (sb-bsd-sockets:socket-listen listener 4)
        (multiple-value-bind (address port)
            (sb-bsd-sockets:socket-name listener)
          (declare (ignore address))
          (values listener
                  (format nil "http://127.0.0.1:~D/oauth2callback" port))))
    (error ()
      (gemini-oauth--fail
       :stage ':callback-listen
       :message "Could not start the Gemini OAuth loopback callback server."))))

(-> gemini-oauth--write-callback-response (stream boolean) null)
(defun gemini-oauth--write-callback-response (stream success-p)
  "Write a minimal browser response for a completed callback."
  (let ((body (if success-p
                  "Gemini authentication succeeded. You may close this tab."
                  "Gemini authentication failed. Return to Autolith.")))
    (format stream
            "HTTP/1.1 ~A~C~CContent-Type: text/plain; charset=utf-8~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C~A"
            (if success-p "200 OK" "400 Bad Request")
            #\Return #\Linefeed #\Return #\Linefeed
            (length body)
            #\Return #\Linefeed #\Return #\Linefeed
            #\Return #\Linefeed
            body)
    (finish-output stream)
    nil))

(-> gemini-oauth-await-loopback
    (sb-bsd-sockets:inet-socket string &key (:timeout integer))
    string)
(defun gemini-oauth-await-loopback (listener expected-state
                                    &key
                                      (timeout *gemini-oauth-callback-timeout*))
  "Wait at most TIMEOUT seconds for a valid loopback callback and return its code."
  (unless (sb-sys:wait-until-fd-usable
           (sb-bsd-sockets:socket-file-descriptor listener)
           ':input
           timeout)
    (gemini-oauth--fail
     :stage ':callback-wait
     :message "Gemini authentication timed out waiting for the browser callback."))
  (let ((socket nil)
        (stream nil))
    (unwind-protect
         (progn
           (setf socket (sb-bsd-sockets:socket-accept listener)
                 stream (sb-bsd-sockets:socket-make-stream
                         socket
                         :input t
                         :output t
                         :element-type 'character
                         :external-format ':utf-8
                         :buffering ':none))
           (let* ((request-line (read-line stream nil ""))
                  (first-space (position #\Space request-line))
                  (second-space (and first-space
                                     (position #\Space request-line
                                               :start (1+ first-space))))
                  (target (and second-space
                               (subseq request-line
                                       (1+ first-space)
                                       second-space)))
                  (parameters (and target
                                   (oauth--query-parameters target)))
                  (state (cdr (assoc "state" parameters :test #'string=)))
                  (code (cdr (assoc "code" parameters :test #'string=)))
                  (oauth-error (cdr (assoc "error" parameters :test #'string=))))
             (cond
               ((or (null target)
                    (not (uiop:string-prefix-p "/oauth2callback?" target)))
                (gemini-oauth--write-callback-response stream nil)
                (gemini-oauth--fail
                 :stage ':callback
                 :message "The Gemini OAuth callback used an unexpected path."))
               ((not (and state (string= state expected-state)))
                (gemini-oauth--write-callback-response stream nil)
                (gemini-oauth--fail
                 :stage ':callback
                 :message "The Gemini OAuth callback state did not match."))
               (oauth-error
                (gemini-oauth--write-callback-response stream nil)
                (gemini-oauth--fail
                 :stage ':authorization
                 :message (format nil "Google rejected Gemini authorization (~A)."
                                  (bounded-string oauth-error :limit 128))
                 :code (bounded-string oauth-error :limit 128)))
               ((not (non-empty-string-p code))
                (gemini-oauth--write-callback-response stream nil)
                (gemini-oauth--fail
                 :stage ':callback
                 :message "The Gemini OAuth callback omitted its authorization code."))
               (t
                (gemini-oauth--write-callback-response stream t)
                code))))
      (when stream
        (ignore-errors (close stream)))
      (when (and socket (null stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))


;;;; -- Token Exchange and Refresh --

(-> gemini-oauth--request
    (&key (:url string) (:content string))
    (values string integer list))
(defun gemini-oauth--request (&key url content)
  "POST one form-encoded request to Google's OAuth token endpoint."
  (handler-case
      (multiple-value-bind (body status headers uri stream)
          (dexador:post url
                        :headers '(("Content-Type" . "application/x-www-form-urlencoded")
                                   ("Accept" . "application/json"))
                        :content content
                        :force-string t
                        :connect-timeout 30
                        :read-timeout 60)
        (declare (ignore uri stream))
        (values body status headers))
    (http-request-failed (condition)
      (values (or (response-body condition) "")
              (response-status condition)
              (response-headers condition)))))

(-> gemini-oauth--token-document
    (function string list keyword)
    json-object)
(defun gemini-oauth--token-document (request-function endpoint parameters stage)
  "POST PARAMETERS and validate the JSON token response for STAGE."
  (let* ((content (url-encode-params parameters))
         (secrets (remove-if-not #'non-empty-string-p
                                 (mapcar #'rest parameters))))
    (multiple-value-bind (body status headers)
        (funcall request-function :url endpoint :content content)
      (declare (ignore headers))
      (unless (and (integerp status) (<= 200 status 299))
        (let* ((document (handler-case (json-decode body) (error () nil)))
               (raw-code (and (json-object-p document)
                              (json-get document "error")))
               (raw-description (and (json-object-p document)
                                     (json-get document "error_description")))
               (code (gemini-oauth--redacted-value raw-code secrets))
               (description
                 (gemini-oauth--redacted-value raw-description secrets)))
          (gemini-oauth--fail
           :stage stage
           :message (format nil "Gemini OAuth token request failed~@[ (~A)~]." code)
           :status (and (integerp status) status)
           :code code
           :response description)))
      (handler-case
          (let ((document (json-decode body)))
            (unless (json-object-p document)
              (error "not an object"))
            document)
        (error ()
          (gemini-oauth--fail
           :stage stage
           :message "The Gemini OAuth token response contained invalid JSON."
           :status status))))))

(-> gemini-oauth--credentials-from-document
    (gemini-credential-manager json-object &key
     (:previous (option oauth-credentials)))
    oauth-credentials)
(defun gemini-oauth--credentials-from-document (manager document &key previous)
  "Validate DOCUMENT and return persisted Gemini credentials."
  (let* ((access-token (json-get document "access_token"))
         (refresh-token (or (json-get document "refresh_token")
                            (and previous
                                 (oauth-credentials-refresh-token previous))))
         (id-token (or (json-get document "id_token")
                       (and previous (oauth-credentials-id-token previous))))
         (expires-in (json-get document "expires_in"))
         (account-id
           (or (and (non-empty-string-p id-token) (jwt-subject id-token))
               (and previous (oauth-credentials-account-id previous))
               "google-main-account")))
    (unless (and (non-empty-string-p access-token)
                 (non-empty-string-p refresh-token)
                 (or (null id-token) (non-empty-string-p id-token))
                 (integerp expires-in)
                 (plusp expires-in))
      (gemini-oauth--fail
       :stage ':token-response
       :message "The Gemini OAuth token response omitted required fields."))
    (make-instance 'oauth-credentials
                   :access-token access-token
                   :refresh-token refresh-token
                   :id-token id-token
                   :account-id account-id
                   :expires-at (+ (get-universal-time) expires-in)
                   :source-path
                   (credential-source-pathname
                    (credential-manager-primary-source manager)))))

(-> gemini-oauth-exchange-code
    (gemini-credential-manager string string string
     &key (:request-function function) (:client-id string)
     (:client-secret (option string)) (:token-endpoint string))
    oauth-credentials)
(defun gemini-oauth-exchange-code
    (manager code verifier redirect-uri
     &key
       (request-function #'gemini-oauth--request)
       (client-id (gemini-oauth-client-id))
       (client-secret (gemini-oauth-client-secret))
       (token-endpoint *gemini-oauth-token-endpoint*))
  "Exchange one authorization CODE using VERIFIER and persist no state."
  (let ((parameters
          (append
           (list (cons "client_id" client-id)
                 (cons "code" code)
                 (cons "code_verifier" verifier)
                 (cons "grant_type" "authorization_code")
                 (cons "redirect_uri" redirect-uri))
           (when client-secret
             (list (cons "client_secret" client-secret))))))
    (gemini-oauth--credentials-from-document
     manager
     (gemini-oauth--token-document
      request-function token-endpoint parameters ':exchange))))

(defmethod credential-manager-refresh-exchange
    ((manager gemini-credential-manager)
     (credentials oauth-credentials)
     (refresh-token string))
  "Refresh Gemini credentials with Google's installed-app OAuth endpoint."
  (let* ((client-secret (gemini-oauth-client-secret))
         (parameters
           (append
            (list (cons "client_id" (gemini-oauth-client-id))
                  (cons "grant_type" "refresh_token")
                  (cons "refresh_token" refresh-token))
            (when client-secret
              (list (cons "client_secret" client-secret)))))
         (document
           (gemini-oauth--token-document
            #'gemini-oauth--request
            *gemini-oauth-token-endpoint*
            parameters
            ':refresh)))
    (values (gemini-oauth--credentials-from-document
             manager document :previous credentials)
            t)))


;;;; -- Public Login Flow --

(-> gemini-oauth-login
    (gemini-credential-manager
     &key (:stream stream) (:open-browser-p boolean)
     (:browser-function function) (:callback-function function)
     (:request-function function) (:timeout integer))
    oauth-credentials)
(defun gemini-oauth-login
    (manager
     &key
       (stream *standard-output*)
       (open-browser-p t)
       (browser-function #'device-authentication-open-browser)
       (callback-function #'gemini-oauth-await-loopback)
       (request-function #'gemini-oauth--request)
       (timeout *gemini-oauth-callback-timeout*))
  "Authenticate Gemini through Google installed-app OAuth and save credentials.

The authorization URL is always printed, so browser-launch failure has a manual
fallback. The loopback callback remains bounded by TIMEOUT."
  (call-with-secret-use
   (lambda ()
     (multiple-value-bind (listener redirect-uri)
         (gemini-oauth-loopback-open)
       (unwind-protect
            (multiple-value-bind (verifier challenge)
                (gemini-oauth-create-pkce)
              (let* ((state (gemini-oauth--random-hex 32))
                     (authorization-url
                       (gemini-oauth-authorization-url
                        :redirect-uri redirect-uri
                        :state state
                        :code-challenge challenge)))
                (format stream
                        "~&Sign in with Gemini in your browser:~%  ~A~%~%Waiting up to ~D seconds for the local callback.~%"
                        authorization-url timeout)
                (finish-output stream)
                (when open-browser-p
                  (unless (handler-case
                              (funcall browser-function authorization-url)
                            (error () nil))
                    (format stream
                            "Could not open a browser. Open the URL above manually.~%")
                    (finish-output stream)))
                (let* ((code (funcall callback-function
                                      listener state :timeout timeout))
                       (credentials
                         (gemini-oauth-exchange-code
                          manager code verifier redirect-uri
                          :request-function request-function)))
                  (credential-manager-accept-account
                   manager credentials :allow-change t)
                  (credential-source-save
                   (credential-manager-primary-source manager)
                   credentials))))
         (ignore-errors (sb-bsd-sockets:socket-close listener)))))))
