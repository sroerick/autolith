(in-package #:autolith)

;;;; -- Grok Device Authentication Test Support --

(-> grok-device-test--manager () grok-credential-manager)
(defun grok-device-test--manager ()
  "Return a Grok credential manager whose writable source records test data."
  (make-instance
   'grok-credential-manager
   :primary-source
   (make-instance 'recording-autolith-credential-source
                  :pathname #P"/tmp/autolith-grok-device/grok-auth.sexp")
   :bootstrap-source
   (make-instance 'grok-bootstrap-credential-source
                  :pathname #P"/tmp/autolith-grok-device/grok-auth.json")))

(-> grok-device-test--token-response (&key (:subject string)) string)
(defun grok-device-test--token-response (&key (subject "grok-user-1"))
  "Return a successful RFC 8628 token response body carrying SUBJECT."
  (json-encode
   (json-object
    "access_token" (device-authentication-test--jwt
                    (json-object "sub" subject))
    "refresh_token" "grok-refresh-test"
    "expires_in" 900
    "id_token" (device-authentication-test--jwt
                (json-object "sub" subject
                             "email" "grok@test.invalid")))))

(-> grok-device-test--request-code-response (&key (:user-code string)) string)
(defun grok-device-test--request-code-response (&key (user-code "GROK-CODE"))
  "Return a device authorization response body carrying USER-CODE."
  (json-encode
   (json-object
    "device_code" "grok-device-code-1"
    "user_code" user-code
    "verification_uri" "https://issuer.test/activate"
    "verification_uri_complete"
    (format nil "https://issuer.test/activate?user_code=~A" user-code)
    "expires_in" 600
    "interval" 2)))


;;;; -- Grok Device Authentication Tests --

(-> grok-device-test--complete-flow () null)
(defun grok-device-test--complete-flow ()
  "Exercise request, pending and slow_down polls, and secure publication."
  (let* ((requests nil)
         (poll-count 0)
         (clock 0)
         (sleeps nil)
         (opened-url nil)
         (*device-authentication-test-saved-credentials* nil))
    (flet ((request (&key method url headers content)
             (push (list :method method
                         :url url
                         :headers headers
                         :content content)
                   requests)
             (cond
               ((device-authentication-test--url-suffix-p
                 url
                 "/oauth2/device/code")
                (values (grok-device-test--request-code-response) 200 nil))
               ((device-authentication-test--url-suffix-p url "/oauth2/token")
                (incf poll-count)
                (case poll-count
                  (1
                   (values (json-encode
                            (json-object "error" "authorization_pending"))
                           400
                           nil))
                  (2
                   (values (json-encode (json-object "error" "slow_down"))
                           400
                           nil))
                  (t
                   (values (grok-device-test--token-response) 200 nil))))
               (t
                (error "Unexpected test URL."))))

           (pause (seconds)
             (push seconds sleeps)
             (incf clock seconds))

           (now ()
             clock)

           (open-browser (url)
             (setf opened-url url)
             t))
      (let* ((client
               (grok-device-authentication-client-create
                :issuer "https://issuer.test/"
                :request-function #'request
                :sleep-function #'pause
                :clock-function #'now
                :browser-function #'open-browser))
             (output (make-string-output-stream))
             (manager (grok-device-test--manager))
             (result (device-authentication-login client manager
                                                  :stream output)))
        (test-assert (eq result t)
                     "the Grok device login reports success")
        (let ((credentials *device-authentication-test-saved-credentials*))
          (test-assert
           (and credentials
                (string= (oauth-credentials-account-id credentials)
                         "grok-user-1"))
           "the published Grok credentials carry the token subject")
          (test-assert
           (string= (oauth-credentials-refresh-token credentials)
                    "grok-refresh-test")
           "the published Grok credentials are renewable")
          (test-assert
           (let ((expires-at (oauth-credentials-expires-at credentials)))
             (and expires-at
                  (<= 890 (- expires-at (get-universal-time)) 910)))
           "the published Grok credentials map expires_in to an expiration"))
        (let ((displayed (get-output-stream-string output)))
          (test-assert
           (search "GROK-CODE" displayed)
           "the Grok user code is displayed for confirmation")
          (test-assert
           (search "https://issuer.test/activate?user_code=GROK-CODE"
                   displayed)
           "the complete Grok verification URL is displayed")
          (test-assert
           (not (search "grok-device-code-1" displayed))
           "the secret device code is never displayed"))
        (test-assert
         (string= opened-url
                  "https://issuer.test/activate?user_code=GROK-CODE")
         "the browser opens the complete Grok verification URL")
        (test-assert
         (= poll-count 3)
         "polling continues through pending and slow_down responses")
        (test-assert
         (equal (reverse sleeps) '(2 2 7))
         "a slow_down response widens the polling interval")
        (let ((code-request
                (device-authentication-test--request
                 requests
                 "/oauth2/device/code")))
          (test-assert
           (and code-request
                (search "client_id=" (getf code-request :content))
                (search "scope=" (getf code-request :content)))
           "the device code request carries the client and scopes"))
        (let ((token-request
                (device-authentication-test--request requests "/oauth2/token")))
          (test-assert
           (and token-request
                (search "device_code=grok-device-code-1"
                        (getf token-request :content))
                (search "urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Adevice_code"
                        (getf token-request :content)))
           "the token poll carries the RFC 8628 grant and device code")))))
  nil)

(-> grok-device-test--rejections () null)
(defun grok-device-test--rejections ()
  "Test denial, expiry, timeout, and malformed server responses."
  (labels ((client-for (responder)
             (let ((clock 0))
               (grok-device-authentication-client-create
                :issuer "https://issuer.test"
                :request-function responder
                :sleep-function (lambda (seconds)
                                  (incf clock seconds))
                :clock-function (lambda ()
                                  clock)
                :browser-function (lambda (url)
                                    (declare (ignore url))
                                    t))))

           (poll-responder (body status)
             (lambda (&key method url headers content)
               (declare (ignore method headers content))
               (if (device-authentication-test--url-suffix-p
                    url
                    "/oauth2/device/code")
                   (values (grok-device-test--request-code-response) 200 nil)
                   (values body status nil))))

           (login (responder)
             (device-authentication-login
              (client-for responder)
              (grok-device-test--manager)
              :stream (make-string-output-stream)
              :open-browser-p nil)))
    (device-authentication-test--signals
     (lambda ()
       (login (poll-responder
               (json-encode (json-object "error" "access_denied"))
               400)))
     ':poll
     :status 400
     :code "access_denied")
    (device-authentication-test--signals
     (lambda ()
       (login (poll-responder
               (json-encode (json-object "error" "expired_token"))
               400)))
     ':poll
     :status 400
     :code "expired_token")
    (device-authentication-test--signals
     (lambda ()
       (login (poll-responder "not-json" 200)))
     ':poll)
    (device-authentication-test--signals
     (lambda ()
       (login (poll-responder
               (json-encode (json-object "access_token" "only-access"))
               200)))
     ':credentials)
    (let ((*device-authentication-test-saved-credentials* nil))
      (device-authentication-test--signals
       (lambda ()
         (login (poll-responder
                 (json-encode (json-object "error" "authorization_pending"))
                 400)))
       ':poll)
      (test-assert
       (null *device-authentication-test-saved-credentials*)
       "an endless pending authorization publishes no credentials")))
  nil)

(-> grok-device-test--request-code-validation () null)
(defun grok-device-test--request-code-validation ()
  "Test rejection of malformed device authorization responses."
  (labels ((request-code (body)
             (device-authentication-request-code
              (grok-device-authentication-client-create
               :issuer "https://issuer.test"
               :request-function
               (lambda (&key method url headers content)
                 (declare (ignore method url headers content))
                 (values body 200 nil))))))
    (device-authentication-test--signals
     (lambda ()
       (request-code (json-encode (json-object "user_code" "ONLY-CODE"))))
     ':request-code)
    (device-authentication-test--signals
     (lambda ()
       (request-code
        (json-encode
         (json-object
          "device_code" "grok-device-code-1"
          "user_code" (format nil "EVIL~CCODE" #\Newline)
          "verification_uri" "https://issuer.test/activate"
          "expires_in" 600))))
     ':request-code)
    (device-authentication-test--signals
     (lambda ()
       (request-code
        (json-encode
         (json-object
          "device_code" "grok-device-code-1"
          "user_code" "GROK-CODE"
          "verification_uri" "javascript:alert(1)"
          "expires_in" 600))))
     ':request-code))
  nil)

(-> run-grok-device-authentication-tests () boolean)
(defun run-grok-device-authentication-tests ()
  "Run the offline Grok device authentication tests."
  (grok-device-test--complete-flow)
  (grok-device-test--rejections)
  (grok-device-test--request-code-validation)
  t)
