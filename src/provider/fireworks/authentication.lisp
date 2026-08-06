(in-package #:autolith)

;;;; -- Fireworks API Key Credential Source --

;;; Fireworks authenticates with a static account API key rather than an
;;; OAuth token family. The key rides in the generic oauth-credentials
;;; access-token slot so transport and redaction treat it exactly like a
;;; subscription bearer token. Account scoping never applies, so the slot
;;; holds the fixed label "fireworks". Keys carry no expiry, so Autolith
;;; never attempts a refresh; a rejected key fails as unauthorized and the
;;; user re-runs `autolith --auth fireworks`.

(defparameter *fireworks-account-label* "fireworks"
  "The synthetic account identifier pinned for static Fireworks API keys.")

(defparameter *fireworks-environment-variable* "FIREWORKS_API_KEY"
  "The environment variable holding the Fireworks account API key.")

(defclass fireworks-environment-credential-source (credential-source)
  ()
  (:documentation
   "A read-only adapter loading the Fireworks API key from the environment."))

(defmethod credential-source-pathname
    ((source fireworks-environment-credential-source))
  "Report a conventional key path because the environment has no pathname."
  (declare (ignore source))
  (merge-pathnames "fireworks-auth.sexp"
                   (environment-directory
                    "XDG_STATE_HOME"
                    (merge-pathnames ".local/state/autolith/"
                                     (user-homedir-pathname)))))

(defmethod credential-source-label
    ((source fireworks-environment-credential-source))
  "Name the Fireworks environment source in user-visible failures."
  (declare (ignore source))
  "the FIREWORKS_API_KEY environment variable")

(defmethod credential-source-load
    ((source fireworks-environment-credential-source))
  "Load the Fireworks API key from FIREWORKS_API_KEY, or return NIL."
  (let ((key (uiop:getenv *fireworks-environment-variable*)))
    (when (non-empty-string-p key)
      (make-instance 'oauth-credentials
                     :access-token key
                     :refresh-token nil
                     :id-token nil
                     :account-id *fireworks-account-label*
                     :expires-at nil
                     :source-path (credential-source-pathname source)))))

(defmethod credential-source-save
    ((source fireworks-environment-credential-source)
     (credentials oauth-credentials))
  "Reject writes to the Fireworks environment source."
  (declare (ignore credentials))
  (error 'authentication-error
         :message "The FIREWORKS_API_KEY environment source is read-only."))


;;;; -- Fireworks Credential Manager --

(defclass fireworks-credential-manager (credential-manager)
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
  "run autolith --auth fireworks")

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

(defmethod credential-manager-load ((manager fireworks-credential-manager))
  "Prefer the current environment key, then load the saved interactive key."
  (let ((environment
          (credential-source-load
           (credential-manager-bootstrap-source manager))))
    (credential-manager-accept-account
     manager
     (or environment
         (credential-source-load
          (credential-manager-primary-source manager))
         (error 'credentials-unavailable
                :message
                (format nil "No Fireworks API key is available; ~A."
                        (credential-manager-login-hint manager))
                :searched-paths
                (list (credential-source-pathname
                       (credential-manager-primary-source manager))))))))

(defmethod credential-manager-refreshable-p
    ((manager fireworks-credential-manager))
  "Static Fireworks API keys cannot refresh."
  (declare (ignore manager))
  nil)

(defmethod credential-manager-refresh-exchange
    ((manager fireworks-credential-manager)
     (credentials oauth-credentials)
     (refresh-token string))
  "Refuse to refresh a static Fireworks API key; this path is unreachable."
  (declare (ignore credentials refresh-token))
  (error 'token-refresh-failed
         :message (format nil "Fireworks API keys never refresh; ~A."
                          (credential-manager-login-hint manager))
         :status nil
         :response nil))


;;;; -- Fireworks API Key Validation --

(-> fireworks-validate-api-key (string) null)
(defun fireworks-validate-api-key (key)
  "Probe the Fireworks Responses API with KEY, signaling on rejection."
  (handler-case
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
         :read-timeout 60))
    (dexador.error:http-request-unauthorized ()
      (error 'authentication-error
             :message "Fireworks rejected the entered API key."))
    (http-request-failed (condition)
      (error 'authentication-error
             :message (format nil
                              "The Fireworks API key validation failed (HTTP ~D)."
                              (response-status condition))))
    (error (condition)
      (if (typep condition 'authentication-error)
          (error condition)
          (error 'authentication-error
                 :message
                 "The Fireworks API key could not be validated; check the network."))))
  nil)

(-> fireworks--read-api-key (stream stream) string)
(defun fireworks--read-api-key (input output)
  "Read one API key from INPUT without terminal echo when INPUT is interactive."
  (if (not (interactive-stream-p input))
      (or (read-line input nil "") "")
      (let* ((descriptor (sb-sys:fd-stream-fd input))
             (saved-mode (sb-posix:tcgetattr descriptor))
             (hidden-mode (sb-posix:tcgetattr descriptor)))
        (setf (sb-posix:termios-lflag hidden-mode)
              (logandc2 (sb-posix:termios-lflag hidden-mode) sb-posix:echo))
        (sb-posix:tcsetattr descriptor sb-posix:tcsanow hidden-mode)
        (unwind-protect
             (or (read-line input nil "") "")
          (sb-posix:tcsetattr descriptor sb-posix:tcsanow saved-mode)
          (terpri output)
          (finish-output output)))))

(-> fireworks-api-key-login
    (fireworks-credential-manager &key (:stream t) (:input t))
    null)
(defun fireworks-api-key-login (manager
                                &key (stream *standard-output*)
                                     (input *standard-input*))
  "Prompt for a Fireworks API key, validate it, and save it to MANAGER's store."
  (format stream "~&Enter your Fireworks API key.~%It is also read from ~A when set.~%API key: "
          *fireworks-environment-variable*)
  (finish-output stream)
  (let ((key (string-trim '(#\Space #\Tab #\Newline #\Return)
                          (fireworks--read-api-key input stream))))
    (unless (non-empty-string-p key)
      (error 'authentication-error
             :message "No Fireworks API key was entered."))
    (fireworks-validate-api-key key)
    (credential-source-save
     (credential-manager-primary-source manager)
     (make-instance 'oauth-credentials
                    :access-token key
                    :refresh-token nil
                    :id-token nil
                    :account-id *fireworks-account-label*
                    :expires-at nil
                    :source-path
                    (credential-source-pathname
                     (credential-manager-primary-source manager))))
    (format stream "~&Fireworks authentication was saved by Autolith.~%")
    nil))
