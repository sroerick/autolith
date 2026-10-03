(in-package #:autolith)


;;;; -- OpenCode API Key Authentication --

(define-static-api-key-provider opencode
  :display-name "OpenCode"
  :environment-variable "OPENCODE_API_KEY"
  :source-class autolith-credential-source
  :source-path (configuration-opencode-auth-path configuration)
  :login-hint "run autolith auth opencode")


;;;; -- OpenCode API Key Login --

(-> opencode-api-key-login
    (opencode-credential-manager &key
                                  (:stream stream)
                                  (:input stream)
                                  (:input-file-descriptor (option integer)))
    string)
(defun opencode-api-key-login
    (manager
     &key
     (stream *standard-output*)
     (input *standard-input*)
     (input-file-descriptor
       (and (eq input *standard-input*)
            *api-key-input-file-descriptor*)))
  "Prompt for an OpenCode API key and save it to MANAGER's private store.

OpenCode's model-list endpoint is public, so login cannot validate a key without
issuing a real chat request. A rejected key therefore fails on its first provider
request with the normal actionable static-key authentication error."
  (api-key-login manager
                 :stream stream
                 :input input
                 :input-file-descriptor input-file-descriptor))
