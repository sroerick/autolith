(in-package #:autolith)

;;;; -- Grok Device Authentication --

;;; Autolith implements the RFC 8628 device authorization grant against the
;;; xAI OAuth issuer, matching grok-build reference commit 47348d13.

(defparameter *grok-device-slow-down-increment* 5
  "Seconds added to the polling interval after an RFC 8628 slow_down error.")

(defclass grok-device-authentication-client (device-authentication-client)
  ()
  (:documentation "The RFC 8628 device authorization client for the xAI issuer."))

(defclass grok-device-authorization (device-authorization)
  ((expires-in
    :initarg :expires-in
    :reader grok-device-authorization-expires-in
    :type (integer 1)
    :documentation "The server-reported device code lifetime in seconds."))
  (:documentation "One pending xAI device authorization and its lifetime."))

(-> grok-device-authentication-client-create
    (&key
     (:issuer string)
     (:client-id string)
     (:request-function (option function))
     (:poll-function (option function))
     (:sleep-function function)
     (:clock-function function)
     (:browser-function function)
     (:poll-timeout integer))
    grok-device-authentication-client)
(defun grok-device-authentication-client-create
    (&key
       (issuer *grok-oauth-issuer*)
       (client-id *grok-oauth-client-id*)
       request-function
       poll-function
       (sleep-function #'sleep)
       (clock-function #'device-authentication--monotonic-seconds)
       (browser-function #'device-authentication-open-browser)
       (poll-timeout *device-authentication-timeout*))
  "Create a Grok device client, optionally replacing every external effect."
  (unless (and (non-empty-string-p issuer)
               (non-empty-string-p client-id)
               (plusp poll-timeout))
    (device-authentication--fail
     :stage ':configuration
     :message "Grok device authentication configuration is invalid."))
  (make-instance 'grok-device-authentication-client
                 :issuer (string-right-trim '(#\/) issuer)
                 :client-id client-id
                 :request-function
                 (or request-function #'device-authentication--request)
                 :poll-function
                 (or poll-function #'grok-device-authentication--poll-for-tokens)
                 :sleep-function sleep-function
                 :clock-function clock-function
                 :browser-function browser-function
                 :poll-timeout poll-timeout))

(defmethod device-authentication-display-code
    ((client grok-device-authentication-client)
     (authorization device-authorization)
     (stream stream))
  "Display the Grok verification URL and one-time code."
  (declare (ignore client))
  (format stream
          "~&Sign in with Grok:~%  Open: ~A~%  Code: ~A~%~%Continue only if you started this login in Autolith and the browser shows the same code.~%"
          (device-authorization-verification-url authorization)
          (device-authorization-user-code authorization))
  (finish-output stream)
  nil)


;;;; -- Request Code --

(-> grok-device-authentication--valid-user-code-p (string) boolean)
(defun grok-device-authentication--valid-user-code-p (user-code)
  "Return true when USER-CODE contains only alphanumerics and hyphens."
  (if (every (lambda (character)
               (or (alphanumericp character)
                   (char= character #\-)))
             user-code)
      t
      nil))

(-> grok-device-authentication--safe-verification-url-p (string) boolean)
(defun grok-device-authentication--safe-verification-url-p (url)
  "Return true when URL is HTTPS or points at a local development issuer."
  (if (and (notany (lambda (character)
                     (< (char-code character) 32))
                   url)
           (or (uiop:string-prefix-p "https://" url)
               (uiop:string-prefix-p "http://localhost" url)
               (uiop:string-prefix-p "http://127.0.0.1" url)))
      t
      nil))

(defmethod device-authentication-request-code
    ((client grok-device-authentication-client))
  "Request a fresh RFC 8628 device code from the xAI issuer."
  (call-with-secret-use
   (lambda ()
     (let* ((content
              (url-encode-params
               (list
                (cons "client_id" (device-authentication-client-id client))
                (cons "scope" (format nil "~{~A~^ ~}" *grok-oauth-scopes*))
                (cons "referrer" "autolith"))))
            (document
              (device-authentication--json-request
               :client client
               :url (device-authentication--issuer-url
                     client
                     "/oauth2/device/code")
               :content-type "application/x-www-form-urlencoded"
               :content content
               :stage ':request-code))
            (device-code (json-get document "device_code"))
            (user-code (json-get document "user_code"))
            (verification-uri (json-get document "verification_uri"))
            (verification-uri-complete
              (json-get document "verification_uri_complete"))
            (expires-in (json-get document "expires_in"))
            (interval (json-get document "interval")))
       (unless (and (non-empty-string-p device-code)
                    (non-empty-string-p user-code)
                    (grok-device-authentication--valid-user-code-p user-code)
                    (non-empty-string-p verification-uri))
         (device-authentication--fail
          :stage ':request-code
          :message "The device authorization response omitted required fields."))
       (let ((verification-url
               (if (non-empty-string-p verification-uri-complete)
                   verification-uri-complete
                   verification-uri)))
         (unless (grok-device-authentication--safe-verification-url-p
                  verification-url)
           (device-authentication--fail
            :stage ':request-code
            :message "The device authorization verification URL is not acceptable."))
         (make-instance 'grok-device-authorization
                        :verification-url verification-url
                        :user-code user-code
                        :device-authorization-id device-code
                        :poll-interval
                        (device-authentication--poll-interval (or interval 5))
                        :expires-in
                        (if (and (integerp expires-in) (plusp expires-in))
                            expires-in
                            *device-authentication-timeout*)))))))


;;;; -- Poll and Completion --

(-> grok-device-authentication--poll-for-tokens
    (grok-device-authentication-client grok-device-authorization)
    json-object)
(defun grok-device-authentication--poll-for-tokens (client authorization)
  "Poll the xAI token endpoint until AUTHORIZATION succeeds, fails, or expires."
  (let* ((clock (device-authentication-client-clock-function client))
         (started-at (funcall clock))
         (deadline (+ started-at
                      (min (device-authentication-client-poll-timeout client)
                           (grok-device-authorization-expires-in authorization))))
         (interval (device-authorization-poll-interval authorization))
         (url (device-authentication--issuer-url client "/oauth2/token"))
         (content
           (url-encode-params
            (list
             (cons "grant_type" "urn:ietf:params:oauth:grant-type:device_code")
             (cons "device_code" (device-authorization-id authorization))
             (cons "client_id" (device-authentication-client-id client))))))
    (loop
      ;; Sleep before polling: an immediate poll on a fresh code only
      ;; returns authorization_pending and risks a slow_down penalty.
      (let ((now (funcall clock)))
        (when (>= now deadline)
          (device-authentication--fail
           :stage ':poll
           :message "Device authentication timed out before approval."))
        (funcall (device-authentication-client-sleep-function client)
                 (min interval (max 1 (- deadline now)))))
      (multiple-value-bind (body status response-headers)
          (device-authentication--invoke-request
           :client client
           :url url
           :headers (list (cons "Content-Type"
                                "application/x-www-form-urlencoded")
                          (cons "Accept" "application/json")
                          (cons "User-Agent"
                                (device-authentication--user-agent)))
           :content content
           :stage ':poll)
        (declare (ignore response-headers))
        (if (device-authentication--success-status-p status)
            (let ((document
                    (handler-case
                        (json-decode body)
                      (error ()
                        (device-authentication--fail
                         :stage ':poll
                         :message "The approved device response contained invalid JSON.")))))
              (unless (json-object-p document)
                (device-authentication--fail
                 :stage ':poll
                 :message "The approved device response was not a JSON object."))
              (return document))
            (let ((code
                    (device-authentication--error-code
                     body
                     (list (device-authorization-id authorization) content))))
              (cond
                ((equal code "authorization_pending")
                 nil)
                ((equal code "slow_down")
                 (incf interval *grok-device-slow-down-increment*))
                ((equal code "access_denied")
                 (device-authentication--fail
                  :stage ':poll
                  :message "The authorization request was denied."
                  :status status
                  :code code))
                ((equal code "expired_token")
                 (device-authentication--fail
                  :stage ':poll
                  :message "The device code expired before approval."
                  :status status
                  :code code))
                (t
                 (device-authentication--fail
                  :stage ':poll
                  :message (format nil "Device authorization was not completed~@[ (~A)~]."
                                   code)
                  :status status
                  :code code)))))))))

(-> grok-device-authentication--credentials
    (json-object pathname)
    oauth-credentials)
(defun grok-device-authentication--credentials (document source-path)
  "Return renewable Grok credentials carried by token DOCUMENT for SOURCE-PATH."
  (let* ((access-token (json-get document "access_token"))
         (refresh-token (json-get document "refresh_token"))
         (id-token (json-get document "id_token"))
         (expires-in (json-get document "expires_in"))
         (account-id
           (or (and (stringp id-token) (jwt-subject id-token))
               (and (stringp access-token) (jwt-subject access-token)))))
    (unless (and (non-empty-string-p access-token)
                 (non-empty-string-p refresh-token)
                 (non-empty-string-p account-id))
      (device-authentication--fail
       :stage ':credentials
       :message "The device token response omitted required credential fields."))
    (make-instance 'oauth-credentials
                   :access-token access-token
                   :refresh-token refresh-token
                   :id-token (and (non-empty-string-p id-token) id-token)
                   :account-id account-id
                   :expires-at (or (and (integerp expires-in)
                                        (plusp expires-in)
                                        (+ (get-universal-time) expires-in))
                                   (jwt-expiration access-token))
                   :source-path source-path)))

(defmethod device-authentication-complete
    ((client grok-device-authentication-client)
     (authorization grok-device-authorization)
     (manager credential-manager))
  "Poll AUTHORIZATION at the xAI issuer and securely publish the result."
  (call-with-secret-use
   (lambda ()
     (let* ((document
              (funcall (device-authentication-client-poll-function client)
                       client
                       authorization))
            (primary-source (credential-manager-primary-source manager)))
       (unless (json-object-p document)
         (device-authentication--fail
          :stage ':poll
          :message "The device authorization poll returned an invalid result."))
       (let ((credentials
               (grok-device-authentication--credentials
                document
                (credential-source-pathname primary-source))))
         (credential-manager-accept-account
          manager credentials :allow-change t)
         (credential-source-save primary-source credentials))
       t))))
