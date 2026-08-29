(in-package #:autolith)

;;;; -- Gemini OAuth Tests --

(defvar *gemini-test-saved-credentials* nil
  "Credentials observed by the Gemini recording source.")

(defclass gemini-test-credential-source (gemini-credential-source)
  ()
  (:documentation "A Gemini credential source that records test writes."))

(defmethod credential-source-save
    ((source gemini-test-credential-source) (credentials oauth-credentials))
  "Record CREDENTIALS without writing SOURCE."
  (declare (ignore source))
  (setf *gemini-test-saved-credentials* credentials))

(-> gemini-test--manager () gemini-credential-manager)
(defun gemini-test--manager ()
  "Return an isolated recording Gemini credential manager."
  (make-instance 'gemini-credential-manager
                 :primary-source
                 (make-instance 'gemini-test-credential-source
                                :pathname #P"/tmp/gemini-auth.sexp")))

(-> run-gemini-authentication-tests () null)
(defun run-gemini-authentication-tests ()
  "Test PKCE, authorization, exchange, redaction, expiry, and login publication."
  (multiple-value-bind (verifier challenge)
      (gemini-oauth-create-pkce)
    (test-assert (= (length verifier) 43)
                 "Gemini PKCE emits a 256-bit unpadded verifier")
    (test-assert (= (length challenge) 43)
                 "Gemini PKCE emits an S256 challenge")
    (test-assert (not (find #\= challenge))
                 "Gemini PKCE challenge is unpadded"))
  (let ((url (gemini-oauth-authorization-url
              :redirect-uri "http://127.0.0.1:3456/oauth2callback"
              :state "state-test"
              :code-challenge "challenge-test"
              :client-id "client-test"
              :authorization-endpoint "https://accounts.test/auth")))
    (dolist (fragment '("client_id=client-test"
                        "code_challenge=challenge-test"
                        "code_challenge_method=S256"
                        "access_type=offline"
                        "state=state-test"))
      (test-assert (search fragment url)
                   "Gemini authorization URL contains required installed-app fields")))
  (let* ((manager (gemini-test--manager))
         (started-at (get-universal-time))
         (request-content nil)
         (credentials
           (gemini-oauth-exchange-code
            manager "code-secret" "verifier-secret"
            "http://127.0.0.1:1234/oauth2callback"
            :client-id "public-client"
            :client-secret nil
            :token-endpoint "https://oauth.test/token"
            :request-function
            (lambda (&key url content)
              (test-assert (string= url "https://oauth.test/token")
                           "Gemini exchange uses the configured endpoint")
              (setf request-content content)
              (values (json-encode
                       (json-object "access_token" "access-test"
                                    "refresh_token" "refresh-test"
                                    "expires_in" 3600))
                      200 nil)))))
    (test-assert (and (search "code_verifier=verifier-secret" request-content)
                      (not (search "client_secret" request-content)))
                 "Gemini exchange uses PKCE public-client fields without a secret")
    (test-assert (string= (oauth-credentials-access-token credentials)
                          "access-test")
                 "Gemini exchange returns the access token")
    (test-assert (>= (oauth-credentials-expires-at credentials)
                     (+ started-at 3599))
                 "Gemini exchange records expires_in as universal time"))
  (let ((condition nil)
        (secret "refresh-do-not-leak"))
    (handler-case
        (gemini-oauth--token-document
         (lambda (&key url content)
           (declare (ignore url content))
           (values (json-encode
                    (json-object "error" "invalid_grant"
                                 "error_description"
                                 (format nil "bad ~A" secret)))
                   400 nil))
         "https://oauth.test/token"
         (list (cons "refresh_token" secret))
         ':refresh)
      (gemini-oauth-error (caught)
        (setf condition caught)))
    (test-assert (and condition
                      (eq (gemini-oauth-error-stage condition) ':refresh)
                      (= (gemini-oauth-error-status condition) 400))
                 "Gemini token failures use the typed condition")
    (test-assert (not (test-object-contains-string-p condition secret))
                 "Gemini token failure conditions redact credential values"))
  (let* ((manager (gemini-test--manager))
         (*gemini-test-saved-credentials* nil)
         (output (make-string-output-stream))
         (browser-url nil))
    (test-call-with-function-replacements
     (list
      (list 'gemini-oauth-loopback-open
            (lambda () (values ':listener
                               "http://127.0.0.1:4444/oauth2callback")))
      (list 'gemini-oauth-create-pkce
            (lambda () (values "verifier-test" "challenge-test"))))
     (lambda ()
       (gemini-oauth-login
        manager
        :stream output
        :browser-function (lambda (url) (setf browser-url url) nil)
        :callback-function
        (lambda (listener state &key timeout)
          (declare (ignore state timeout))
          (test-assert (eq listener ':listener)
                       "Gemini login waits on its loopback listener")
          "code-test")
        :request-function
        (lambda (&key url content)
          (declare (ignore url content))
          (values (json-encode
                   (json-object "access_token" "access-login"
                                "refresh_token" "refresh-login"
                                "expires_in" 3600))
                  200 nil)))))
    (test-assert (and browser-url
                      (search "Open the URL above manually."
                              (get-output-stream-string output)))
                 "Gemini login exposes manual fallback when browser launch fails")
    (test-assert (and *gemini-test-saved-credentials*
                      (string= (oauth-credentials-access-token
                                *gemini-test-saved-credentials*)
                               "access-login"))
                 "Gemini login publishes credentials through the credential manager"))
  nil)
