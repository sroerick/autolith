(in-package #:autolith)


;;;; -- OpenRouter API Key Authentication --

(define-static-api-key-provider openrouter
  :display-name "OpenRouter"
  :environment-variable "OPENROUTER_API_KEY")


;;;; -- OpenRouter API Key Validation --

(-> openrouter-validate-api-key (string) null)
(defun openrouter-validate-api-key (key)
  "Probe the configured OpenRouter models endpoint with KEY."
  (api-key-validate-probe
   "OpenRouter"
   (lambda ()
     (provider-call-with-response-deadline
      60
      (lambda ()
        (dexador:get
         (openrouter-models-endpoint)
         :headers (list (cons "Authorization" (concatenate 'string "Bearer " key)))
         :force-string t
         :keep-alive nil
         :connect-timeout 30
         :read-timeout 60))))))

(-> openrouter-api-key-login
    (openrouter-credential-manager &key (:stream stream))
    string)
(defun openrouter-api-key-login (manager &key (stream *standard-output*))
  "Prompt for, validate, and store the OpenRouter API key."
  (api-key-login manager
                 :stream stream
                 :validate #'openrouter-validate-api-key))
