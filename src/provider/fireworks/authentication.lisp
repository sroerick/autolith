(in-package #:autolith)


;;;; -- Fireworks API Key Authentication --

(define-static-api-key-provider fireworks
  :display-name "Fireworks"
  :environment-variable "FIREWORKS_API_KEY"
  :source-class autolith-credential-source
  :source-path (configuration-fireworks-auth-path configuration)
  :login-hint "run autolith auth fireworks")


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
              "store" (json-false)
              "stream" (json-false))))
       (provider-call-with-response-deadline
        60
        (lambda ()
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
           :read-timeout 60)))))))

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
