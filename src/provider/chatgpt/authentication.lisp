(in-package #:autolith)

;;;; -- ChatGPT Browser OAuth Conditions --

(define-condition chatgpt-oauth-error (authentication-error)
  ((stage
    :initarg :stage
    :reader chatgpt-oauth-error-stage
    :type keyword
    :documentation "The browser OAuth stage that failed.")
   (status
    :initarg :status
    :initform nil
    :reader chatgpt-oauth-error-status
    :type (option integer)
    :documentation "The HTTP status returned by OpenAI, if known.")
   (code
    :initarg :code
    :initform nil
    :reader chatgpt-oauth-error-code
    :type (option string)
    :documentation "A bounded non-secret OAuth error code, if supplied.")
   (response
    :initarg :response
    :initform nil
    :reader chatgpt-oauth-error-response
    :type (option string)
    :documentation "A bounded redacted OAuth error description, if supplied."))
  (:documentation "A failure in ChatGPT browser OAuth."))

(define-condition chatgpt-oauth-state-mismatch (chatgpt-oauth-error)
  ()
  (:documentation
   "A local ChatGPT OAuth callback whose state does not match the active login."))


;;;; -- PKCE and Authorization URL --

(-> chatgpt-oauth-create-pkce () (values string string))
(defun chatgpt-oauth-create-pkce ()
  "Return a fresh 512-bit PKCE verifier and its S256 challenge."
  (oauth--create-pkce :verifier-octets 64))

(-> chatgpt-oauth--state () string)
(defun chatgpt-oauth--state ()
  "Return a fresh 256-bit OAuth state value."
  (oauth--base64url (random-data 32)))

(-> chatgpt-oauth-authorization-url
    (&key
     (:redirect-uri string)
     (:state string)
     (:code-challenge string)
     (:issuer string)
     (:client-id string)
     (:originator string))
    string)
(defun chatgpt-oauth-authorization-url
    (&key
       redirect-uri
       state
       code-challenge
       (issuer *openai-oauth-issuer*)
       (client-id *openai-oauth-client-id*)
       (originator *openai-oauth-originator*))
  "Build the OpenAI authorization URL for one ChatGPT browser login."
  (format nil "~A/oauth/authorize?~A"
          (string-right-trim '(#\/) issuer)
          (url-encode-params
           (list
            (cons "response_type" "code")
            (cons "client_id" client-id)
            (cons "redirect_uri" redirect-uri)
            (cons "scope" (format nil "~{~A~^ ~}" *openai-oauth-scopes*))
            (cons "code_challenge" code-challenge)
            (cons "code_challenge_method" "S256")
            (cons "id_token_add_organizations" "true")
            (cons "codex_cli_simplified_flow" "true")
            (cons "state" state)
            (cons "originator" originator)))))

(-> chatgpt-oauth--redacted-value (t list) (option string))
(defun chatgpt-oauth--redacted-value (value secrets)
  "Return VALUE after exact secret redaction and bounding, or NIL."
  (when (stringp value)
    (bounded-string
     (redact-exact-string-values
      value secrets
      (safe-redaction-marker "[OAUTH VALUE REDACTED]" secrets))
     :limit 256)))

(-> chatgpt-oauth--fail
    (&key
     (:stage keyword)
     (:message string)
     (:status (option integer))
     (:code (option string))
     (:response (option string)))
    nil)
(defun chatgpt-oauth--fail (&key stage message status code response)
  "Signal a structured ChatGPT OAuth failure containing only safe metadata."
  (error 'chatgpt-oauth-error
         :message message
         :stage stage
         :status status
         :code code
         :response response))


;;;; -- Loopback Callback --

(-> chatgpt-oauth--open-listener (integer) (option sb-bsd-sockets:inet-socket))
(defun chatgpt-oauth--open-listener (port)
  "Open one IPv4 loopback listener on PORT, or return NIL when unavailable."
  (let ((listener nil))
    (handler-case
        (progn
          (setf listener (make-instance 'sb-bsd-sockets:inet-socket
                                        :type ':stream
                                        :protocol ':tcp))
          (sb-bsd-sockets:socket-bind
           listener
           (sb-bsd-sockets:make-inet-address "127.0.0.1")
           port)
          (sb-bsd-sockets:socket-listen listener 4)
          listener)
      (error ()
        (when listener
          (ignore-errors (sb-bsd-sockets:socket-close listener)))
        nil))))

(-> chatgpt-oauth-loopback-open () (values sb-bsd-sockets:inet-socket string))
(defun chatgpt-oauth-loopback-open ()
  "Open the first available allowlisted ChatGPT callback port."
  (dolist (port *chatgpt-oauth-callback-ports*)
    (let ((listener (chatgpt-oauth--open-listener port)))
      (when listener
        (return-from chatgpt-oauth-loopback-open
          (values listener
                  (format nil "http://localhost:~D/auth/callback" port))))))
  (chatgpt-oauth--fail
   :stage ':callback-listen
   :message
   (format nil
           "Could not start the ChatGPT OAuth callback server on localhost port~P ~{~D~^ or ~}."
           (length *chatgpt-oauth-callback-ports*)
           *chatgpt-oauth-callback-ports*)))

(-> chatgpt-oauth--write-callback-response (stream string string) null)
(defun chatgpt-oauth--write-callback-response (stream status body)
  "Write one minimal browser response with STATUS and ASCII BODY."
  (format stream
          "HTTP/1.1 ~A~C~CContent-Type: text/plain; charset=utf-8~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C~A"
          status
          #\Return #\Linefeed #\Return #\Linefeed
          (length body)
          #\Return #\Linefeed #\Return #\Linefeed
          #\Return #\Linefeed
          body)
  (finish-output stream)
  nil)

(-> chatgpt-oauth--request-target (string) (option string))
(defun chatgpt-oauth--request-target (request-line)
  "Return the request target from one HTTP GET request line."
  (when (uiop:string-prefix-p "GET " request-line)
    (let* ((first-space (position #\Space request-line))
           (second-space (and first-space
                              (position #\Space request-line
                                        :start (1+ first-space)))))
      (and second-space
           (subseq request-line (1+ first-space) second-space)))))

(-> chatgpt-oauth--callback-target-p ((option string)) boolean)
(defun chatgpt-oauth--callback-target-p (target)
  "Return true when TARGET addresses the ChatGPT OAuth callback path."
  (and target
       (let ((question (position #\? target)))
         (string= (subseq target 0 question) "/auth/callback"))))

(-> chatgpt-oauth--state-matches-p (t string) boolean)
(defun chatgpt-oauth--state-matches-p (actual expected)
  "Return true when ACTUAL is EXPECTED or its supported onboarding variant."
  (and (stringp actual)
       (or (string= actual expected)
           (string= actual
                    (format nil "~A.onboarding_entrypoint=life_sciences"
                            expected)))))

(-> chatgpt-oauth--callback-code (string string) string)
(defun chatgpt-oauth--callback-code (target expected-state)
  "Validate one callback TARGET and return its authorization code."
  (let* ((parameters (oauth--query-parameters target))
         (state (rest (assoc "state" parameters :test #'string=)))
         (code (rest (assoc "code" parameters :test #'string=)))
         (raw-error (rest (assoc "error" parameters :test #'string=)))
         (raw-description
           (rest (assoc "error_description" parameters :test #'string=)))
         (secrets (list expected-state code))
         (safe-error (chatgpt-oauth--redacted-value raw-error secrets))
         (safe-description
           (chatgpt-oauth--redacted-value raw-description secrets)))
    (unless (chatgpt-oauth--state-matches-p state expected-state)
      (error 'chatgpt-oauth-state-mismatch
             :message "The ChatGPT OAuth callback state did not match."
             :stage ':callback
             :status nil
             :code nil
             :response nil))
    (when raw-error
      (chatgpt-oauth--fail
       :stage ':authorization
       :message (format nil "OpenAI rejected ChatGPT authorization~@[ (~A)~]."
                        safe-error)
       :code safe-error
       :response safe-description))
    (unless (non-empty-string-p code)
      (chatgpt-oauth--fail
       :stage ':callback
       :message "The ChatGPT OAuth callback omitted its authorization code."))
    code))


(-> chatgpt-oauth--callback-code-or-continue (string string) (option string))
(defun chatgpt-oauth--callback-code-or-continue (target expected-state)
  "Return a callback code, or NIL when an unrelated local callback should be ignored."
  (handler-case
      (chatgpt-oauth--callback-code target expected-state)
    (chatgpt-oauth-state-mismatch ()
      nil)))

(-> chatgpt-oauth--read-request-line
    (stream integer real
     &key (:clock-function function) (:wait-function function)
     (:request-timeout integer) (:line-limit integer))
    (option string))
(defun chatgpt-oauth--read-request-line
    (stream file-descriptor deadline
     &key
       (clock-function #'device-authentication-monotonic-seconds)
       (wait-function #'sb-sys:wait-until-fd-usable)
       (request-timeout *chatgpt-oauth-request-timeout*)
       (line-limit *chatgpt-oauth-request-line-limit*))
  "Read one bounded callback request line without exceeding the local deadline."
  (let* ((started-at (funcall clock-function))
         (connection-deadline
           (min deadline (+ started-at request-timeout)))
         (characters
           (make-array 128
                       :element-type 'character
                       :adjustable t
                       :fill-pointer 0)))
    (loop
      (when (>= (length characters) line-limit)
        (return nil))
      (let ((remaining (- connection-deadline (funcall clock-function))))
        (unless (and (plusp remaining)
                     (or (listen stream)
                         (funcall wait-function
                                  file-descriptor ':input remaining)))
          (return nil)))
      (let ((character (read-char stream nil nil)))
        (cond
          ((null character)
           (return nil))
          ((char= character #\Linefeed)
           (return
             (string-right-trim '(#\Return)
                                (coerce characters 'string))))
          (t
           (vector-push-extend character characters)))))))

(-> chatgpt-oauth-await-loopback
    (sb-bsd-sockets:inet-socket string &key (:timeout integer))
    string)
(defun chatgpt-oauth-await-loopback
    (listener expected-state &key (timeout *chatgpt-oauth-callback-timeout*))
  "Wait at most TIMEOUT seconds for a valid ChatGPT browser callback."
  (let ((deadline (+ (device-authentication-monotonic-seconds) timeout)))
    (loop
      (let ((remaining (- deadline (device-authentication-monotonic-seconds))))
        (unless (and (plusp remaining)
                     (sb-sys:wait-until-fd-usable
                      (sb-bsd-sockets:socket-file-descriptor listener)
                      ':input
                      remaining))
          (chatgpt-oauth--fail
           :stage ':callback-wait
           :message "ChatGPT authentication timed out waiting for the browser callback."))
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
                 (let* ((request-line
                          (chatgpt-oauth--read-request-line
                           stream
                           (sb-bsd-sockets:socket-file-descriptor socket)
                           deadline))
                        (target
                          (and request-line
                               (chatgpt-oauth--request-target request-line))))
                   (cond
                     ((null request-line)
                      nil)
                     ((not (chatgpt-oauth--callback-target-p target))
                      (chatgpt-oauth--write-callback-response
                       stream "404 Not Found" "Not Found"))
                     (t
                      (handler-case
                          (let ((code
                                  (chatgpt-oauth--callback-code-or-continue
                                   target expected-state)))
                            (if code
                                (progn
                                  (chatgpt-oauth--write-callback-response
                                   stream
                                   "200 OK"
                                   "ChatGPT authorization was received. Return to Autolith.")
                                  (return code))
                                (chatgpt-oauth--write-callback-response
                                 stream
                                 "400 Bad Request"
                                 "ChatGPT authorization did not match this login.")))
                        (chatgpt-oauth-error (condition)
                          (chatgpt-oauth--write-callback-response
                           stream
                           "400 Bad Request"
                           "ChatGPT authorization failed. Return to Autolith.")
                          (error condition)))))))
            (when stream
              (ignore-errors (close stream)))
            (when (and socket (null stream))
              (ignore-errors (sb-bsd-sockets:socket-close socket)))))))))


;;;; -- Token Exchange --

(-> chatgpt-oauth--request
    (&key (:url string) (:content string))
    (values string integer t))
(defun chatgpt-oauth--request (&key url content)
  "POST one form-encoded request to OpenAI's OAuth token endpoint."
  (handler-case
      (multiple-value-bind (body status headers uri stream)
          (provider-call-with-response-deadline
           60
           (lambda ()
             (dexador:post
              url
              :headers
              (list (cons "Content-Type" "application/x-www-form-urlencoded")
                    (cons "Accept" "application/json")
                    (cons "User-Agent" (authentication-user-agent))
                    (cons "originator" *openai-oauth-originator*))
              :content content
              :force-string t
              :connect-timeout 30
              :read-timeout 60)))
        (declare (ignore uri stream))
        (values body status headers))
    (sb-sys:deadline-timeout ()
      (error 'authentication-error
             :message "OpenAI OAuth exceeded its response deadline."))
    (http-request-failed (condition)
      (values (or (provider--error-body-text (response-body condition)) "")
              (response-status condition)
              (response-headers condition)))))

(-> chatgpt-oauth--error-description (t) (option string))
(defun chatgpt-oauth--error-description (document)
  "Return the provider error description carried by DOCUMENT, if any."
  (when (json-object-p document)
    (or (json-get document "error_description")
        (let ((error (json-get document "error")))
          (and (json-object-p error)
               (or (json-get error "message")
                   (json-get error "description")))))))

(-> chatgpt-oauth--token-document (function string list keyword) json-object)
(defun chatgpt-oauth--token-document
    (request-function endpoint parameters stage)
  "POST PARAMETERS and validate the JSON token response for STAGE."
  (let* ((content (url-encode-params parameters))
         (secrets (remove-if-not #'non-empty-string-p
                                 (mapcar #'rest parameters))))
    (multiple-value-bind (body status headers)
        (funcall request-function :url endpoint :content content)
      (declare (ignore headers))
      (unless (and (integerp status) (<= 200 status 299))
        (let* ((document (handler-case (json-decode body) (error () nil)))
               (raw-code (oauth-error-code body))
               (raw-description
                 (chatgpt-oauth--error-description document))
               (code (chatgpt-oauth--redacted-value raw-code secrets))
               (description
                 (chatgpt-oauth--redacted-value raw-description secrets)))
          (chatgpt-oauth--fail
           :stage stage
           :message (format nil "ChatGPT OAuth token request failed~@[ (~A)~]."
                            code)
           :status (and (integerp status) status)
           :code code
           :response description)))
      (handler-case
          (let ((document (json-decode body)))
            (unless (json-object-p document)
              (error "not an object"))
            document)
        (error ()
          (chatgpt-oauth--fail
           :stage stage
           :message "The ChatGPT OAuth token response contained invalid JSON."
           :status status))))))

(-> chatgpt-oauth--credentials-from-document
    (chatgpt-credential-manager json-object)
    oauth-credentials)
(defun chatgpt-oauth--credentials-from-document (manager document)
  "Validate DOCUMENT and return persisted ChatGPT OAuth credentials."
  (let* ((id-token (json-get document "id_token"))
         (access-token (json-get document "access_token"))
         (refresh-token (json-get document "refresh_token"))
         (account-id
           (or (and (non-empty-string-p id-token) (jwt-account-id id-token))
               (and (non-empty-string-p access-token)
                    (jwt-account-id access-token)))))
    (unless (and (non-empty-string-p id-token)
                 (non-empty-string-p access-token)
                 (non-empty-string-p refresh-token)
                 (non-empty-string-p account-id))
      (chatgpt-oauth--fail
       :stage ':token-response
       :message "The ChatGPT OAuth token response omitted required fields."))
    (make-instance 'oauth-credentials
                   :access-token access-token
                   :refresh-token refresh-token
                   :id-token id-token
                   :account-id account-id
                   :expires-at (or (jwt-expiration access-token)
                                   (jwt-expiration id-token))
                   :source-path
                   (credential-source-pathname
                    (credential-manager-primary-source manager)))))

(-> chatgpt-oauth-exchange-code
    (chatgpt-credential-manager string string string
     &key (:request-function function) (:client-id string)
     (:token-endpoint string))
    oauth-credentials)
(defun chatgpt-oauth-exchange-code
    (manager code verifier redirect-uri
     &key
       (request-function #'chatgpt-oauth--request)
       (client-id *openai-oauth-client-id*)
       (token-endpoint *openai-oauth-token-endpoint*))
  "Exchange one authorization CODE using VERIFIER and persist no state."
  (chatgpt-oauth--credentials-from-document
   manager
   (chatgpt-oauth--token-document
    request-function
    token-endpoint
    (list (cons "grant_type" "authorization_code")
          (cons "code" code)
          (cons "redirect_uri" redirect-uri)
          (cons "client_id" client-id)
          (cons "code_verifier" verifier))
    ':exchange)))


;;;; -- Public Login Flow --

(-> chatgpt-oauth-login
    (chatgpt-credential-manager
     &key (:stream stream) (:open-browser-p boolean)
     (:browser-function function) (:callback-function function)
     (:request-function function) (:timeout integer))
    oauth-credentials)
(defun chatgpt-oauth-login
    (manager
     &key
       (stream *standard-output*)
       (open-browser-p t)
       (browser-function #'device-authentication-open-browser)
       (callback-function #'chatgpt-oauth-await-loopback)
       (request-function #'chatgpt-oauth--request)
       (timeout *chatgpt-oauth-callback-timeout*))
  "Authenticate ChatGPT through browser OAuth and save renewable credentials."
  (unless (plusp timeout)
    (chatgpt-oauth--fail
     :stage ':configuration
     :message "The ChatGPT OAuth callback timeout must be positive."))
  (call-with-secret-use
   (lambda ()
     (multiple-value-bind (listener redirect-uri)
         (chatgpt-oauth-loopback-open)
       (unwind-protect
            (multiple-value-bind (verifier challenge)
                (chatgpt-oauth-create-pkce)
              (let* ((state (chatgpt-oauth--state))
                     (authorization-url
                       (chatgpt-oauth-authorization-url
                        :redirect-uri redirect-uri
                        :state state
                        :code-challenge challenge)))
                (format stream
                        "~&Sign in with ChatGPT in your browser:~%  ~A~%~%Starting local callback server on ~A.~%Waiting up to ~D seconds for the browser callback.~%"
                        authorization-url
                        redirect-uri
                        timeout)
                (finish-output stream)
                (when open-browser-p
                  (unless (handler-case
                              (funcall browser-function authorization-url)
                            (error () nil))
                    (format stream
                            "Could not open a browser. Open the URL above manually.~%")
                    (finish-output stream)))
                (let* ((code
                         (funcall callback-function
                                  listener state :timeout timeout))
                       (credentials
                         (chatgpt-oauth-exchange-code
                          manager code verifier redirect-uri
                          :request-function request-function)))
                  (credential-manager-accept-account
                   manager credentials :allow-change t)
                  (credential-source-save
                   (credential-manager-primary-source manager)
                   credentials))))
         (ignore-errors (sb-bsd-sockets:socket-close listener)))))))